#ifndef RIR_COMPILER_H
#define RIR_COMPILER_H

#include "R/Preserve.h"
#include "R/Protect.h"
#include "R/r.h"
#include "runtime/DispatchTable.h"
#include "utils/FunctionWriter.h"
#include "utils/Pool.h"

#include <unordered_map>
#include <iostream>
#include <functional>
#include <cassert>

#include "runtimePatches.h"
#include "utils/UMap.h"

typedef struct RCNTXT RCNTXT;
extern "C" SEXP R_syscall(int n, RCNTXT *cptr);
extern "C" SEXP R_sysfunction(int n, RCNTXT *cptr);

namespace rir {

static size_t getHast(DispatchTable* vtable) {
    auto rirBody = vtable->baseline()->body();
    SEXP ast_1 = src_pool_at(globalContext(), rirBody->src);

    size_t hast = 0;
    hash_ast(ast_1, hast);

    return hast;
}

static void populateHastSrcData(DispatchTable* vtable, size_t hast) {
    int index = 0;
    SEXP map = Pool::get(SRC_HAST_MAP);
    if (map == R_NilValue) {
        UMap::createMapInCp(SRC_HAST_MAP);
        map = Pool::get(SRC_HAST_MAP);
    }
    vtable->baseline()->body()->populateSrcData(hast, map, true, index);
}

static void insertVTable(DispatchTable* vtable, size_t hast) {
    SEXP map = Pool::get(HAST_VTAB_MAP);
    if (map == R_NilValue) {
        UMap::createMapInCp(HAST_VTAB_MAP);
        map = Pool::get(HAST_VTAB_MAP);
    }
    SEXP hastSym = Rf_install(std::to_string(hast).c_str());
    UMap::insert(map, hastSym, vtable->container());
}

static void insertClosObj(SEXP clos, size_t hast) {
    SEXP map = Pool::get(HAST_CLOS_MAP);
    if (map == R_NilValue) {
        UMap::createMapInCp(HAST_CLOS_MAP);
        map = Pool::get(HAST_CLOS_MAP);
    }
    SEXP hastSym = Rf_install(std::to_string(hast).c_str());
    UMap::insert(map, hastSym, clos);
}

static void insertToBlacklist(size_t hast) {
    SEXP map = Pool::get(BL_MAP);
    if (map == R_NilValue) {
        UMap::createMapInCp(BL_MAP);
        map = Pool::get(BL_MAP);
    }

    SEXP hastSym = Rf_install(std::to_string(hast).c_str());

    if (!UMap::symbolExistsInMap(hastSym, map)) {
        UMap::insert(map, hastSym, R_TrueValue);
        #if DEBUG_BLACKLIST == 1
        std::cout << "(R) Blacklisting: " << hast << " (" << hastSym << ")" << std::endl;
        #endif
    }
}

static bool readyForSerialization(DispatchTable* vtable, size_t hast) {
    // if the hast already corresponds to other src addresses and is a different closureObj then
    // there was a collision and we cannot use this function and all functions that depend on it
    auto rirBody = vtable->baseline()->body();

    SEXP hastSym = Rf_install(std::to_string(hast).c_str());

    SEXP vTableMap = Pool::get(HAST_VTAB_MAP);
    if (vTableMap != R_NilValue && UMap::symbolExistsInMap(hastSym, vTableMap)) {
        DispatchTable * oldTab = DispatchTable::unpack(UMap::get(vTableMap, hastSym));
        auto oldRirBody = oldTab->baseline()->body();
        if (oldRirBody->src != rirBody->src) {
            insertToBlacklist(hast);
            return false;
        }
    }
    return true;
}

class Compiler {
    SEXP exp;
    SEXP formals;
    SEXP closureEnv;

    Preserve preserve;

    explicit Compiler(SEXP exp)
        : exp(exp), formals(R_NilValue), closureEnv(nullptr) {
        preserve(exp);
    }

    Compiler(SEXP exp, SEXP formals, SEXP env)
        : exp(exp), formals(formals), closureEnv(env) {
        preserve(exp);
        preserve(formals);
        preserve(env);
    }

  public:
    static bool profile;
    static bool unsoundOpts;
    static bool loopPeelingEnabled;

    SEXP finalize();

    static SEXP compileExpression(SEXP ast) {
#if 0
        size_t count = 1;
        static std::unordered_map<SEXP, size_t> counts;
        if (counts.count(ast)) {
            counts.at(ast) = count = 1 + counts.at(ast);
        } else {
            counts[ast] = 1;
        }
        if (count % 200 == 0) {
            std::cout << "<<<<<<< Warning: expression compiled "
                      << count << "x:\n";
            Rf_PrintValue(ast);
            std::cout << "== Call:\n";
            Rf_PrintValue(R_syscall(0, R_GlobalContext));
            std::cout << "== Function:\n";
            Rf_PrintValue(R_sysfunction(0, R_GlobalContext));
            std::cout << ">>>>>>>\n";
        }
#endif

        // Rf_PrintValue(ast);
        Compiler c(ast);
        auto res = c.finalize();
        return res;
    }

    // To compile a function which is not yet closed
    static SEXP compileFunction(SEXP ast, SEXP formals) {
        Compiler c(ast, formals, nullptr);
        SEXP res = c.finalize();
        PROTECT(res);

        // Allocate a new vtable.
        DispatchTable* vtable = DispatchTable::create();

        // Initialize the vtable. Initially the table has one entry, which is
        // the compiled function.
        vtable->baseline(Function::unpack(res));

        // Set the closure fields.
        UNPROTECT(1);

        size_t hast = getHast(vtable);
        if (readyForSerialization(vtable, hast)) {
            #if DEBUG_TABLE_ENTRIES == 1
            std::cout << "(R) Hast: " << hast << " (Adding table, closure and populating src Map): " << vtable->container() << std::endl;
            #endif
            insertVTable(vtable, hast);
            populateHastSrcData(vtable, hast);
            insertClosObj(vtable->container(), hast);
        }
        return vtable->container();
    }

    static void compileClosure(SEXP inClosure) {

        assert(TYPEOF(inClosure) == CLOSXP);

        Protect p;

        SEXP body = BODY(inClosure);
        SEXP origBC = nullptr;
        if (TYPEOF(body) == BCODESXP) {
            origBC = p(body);
            body = VECTOR_ELT(CDR(body), 0);
        }

        Compiler c(body, FORMALS(inClosure), CLOENV(inClosure));
        SEXP compiledFun = p(c.finalize());

        // Allocate a new vtable.
        DispatchTable* vtable = DispatchTable::create();
        p(vtable->container());

        // Initialize the vtable. Initially the table has one entry, which is
        // the compiled function.
        vtable->baseline(Function::unpack(compiledFun));
        // Keep alive. TODO: why is this needed?
        if (origBC)
            vtable->baseline()->body()->addExtraPoolEntry(origBC);

        // Set the closure fields.
        SET_BODY(inClosure, vtable->container());

        size_t hast = getHast(vtable);
        if (readyForSerialization(vtable, hast)) {
            #if DEBUG_TABLE_ENTRIES == 1
            std::cout << "(R) Hast: " << hast << " (Adding table, closure and populating src Map): " << inClosure << std::endl;
            #endif
            insertVTable(vtable, hast);
            populateHastSrcData(vtable, hast);
            insertClosObj(inClosure, hast);
        }
    }
};

}

#endif
