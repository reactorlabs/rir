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
#include "R/Printing.h"
#include "api.h"

typedef struct RCNTXT RCNTXT;
extern "C" SEXP R_syscall(int n, RCNTXT *cptr);
extern "C" SEXP R_sysfunction(int n, RCNTXT *cptr);

namespace rir {

static SEXP getHast(SEXP body, SEXP env) {
    std::stringstream qHast;
    SEXP x = env;
    if (x == R_GlobalEnv) {
        qHast << "GE:";
    } else if (x == R_BaseEnv) {
        qHast << "BE:";
    } else if (x == R_EmptyEnv) {
        qHast << "EE:";
    } else if (R_IsPackageEnv(x)) {
        qHast << "PE:" << Rf_translateChar(STRING_ELT(R_PackageEnvName(x), 0)) << ":";
    } else if (R_IsNamespaceEnv(x)) {
        qHast << "NS:" << Rf_translateChar(STRING_ELT(R_NamespaceEnvSpec(x), 0)) << ":";
    } else {
        return R_NilValue;
        qHast << "AE:";
    }
    size_t hast = 0;
    hash_ast(body, hast);
    qHast << hast;
    SEXP calcHast = Rf_install(qHast.str().c_str());
    return calcHast;
}

static void populateHastSrcData(DispatchTable* vtable, SEXP hastSym) {
    SEXP map = Pool::get(SRC_HAST_MAP);
    if (map == R_NilValue) {
        SEXP tmp;
        PROTECT(tmp = R_NewEnv(R_EmptyEnv,0,0));
        Pool::patch(SRC_HAST_MAP, tmp);
        UNPROTECT(1);
        map = Pool::get(SRC_HAST_MAP);
    }
    int index = 0;
    vtable->baseline()->body()->populateSrcData(hastSym, map, true, index);
    // int j = 0;
    // vtable->baseline()->body()->printSource(true, j);
}

static void insertVTable(DispatchTable* vtable, SEXP hastSym) {
    SEXP map = Pool::get(HAST_VTAB_MAP);
    if (map == R_NilValue) {
        SEXP tmp;
        PROTECT(tmp = R_NewEnv(R_EmptyEnv,0,0));
        Pool::patch(HAST_VTAB_MAP, tmp);
        UNPROTECT(1);
        map = Pool::get(HAST_VTAB_MAP);
    }
    assert(TYPEOF(map) == ENVSXP);
    Rf_defineVar(hastSym, vtable->container(), map);
    Pool::insert(vtable->container());
}

static void insertClosObj(SEXP clos, SEXP hastSym) {
    SEXP map = Pool::get(HAST_CLOS_MAP);
    if (map == R_NilValue) {
        SEXP tmp;
        PROTECT(tmp = R_NewEnv(R_EmptyEnv,0,0));
        Pool::patch(HAST_CLOS_MAP, tmp);
        UNPROTECT(1);
        map = Pool::get(HAST_CLOS_MAP);
    }
    assert(TYPEOF(map) == ENVSXP);
    Rf_defineVar(hastSym, clos, map);
    Pool::insert(clos);
}

static void insertToBlacklist(SEXP hastSym) {
    SEXP map = Pool::get(BL_MAP);
    if (map == R_NilValue) {
        SEXP tmp;
        PROTECT(tmp = R_NewEnv(R_EmptyEnv,0,0));
        Pool::patch(BL_MAP, tmp);
        UNPROTECT(1);
        map = Pool::get(BL_MAP);
    }

    if (Rf_findVarInFrame(map, hastSym) == R_UnboundValue) {
        assert(TYPEOF(map) == ENVSXP);
        Rf_defineVar(hastSym, R_TrueValue, map);
        #if DEBUG_BLACKLIST == 1
        std::cout << "(R) Blacklisting: " << hast << " (" << hastSym << ")" << std::endl;
        #endif
    }
}

static bool readyForSerialization(DispatchTable* vtable, SEXP hastSym) {
    // if the hast already corresponds to other src addresses and is a different closureObj then
    // there was a collision and we cannot use this function and all functions that depend on it
    // auto rirBody = vtable->baseline()->body();


    SEXP vTableMap = Pool::get(HAST_VTAB_MAP);
    if (vTableMap != R_NilValue && Rf_findVarInFrame(vTableMap, hastSym) != R_UnboundValue) {
        // SEXP vtabContainer = Rf_findVarInFrame(vTableMap, hastSym);
        // if (!DispatchTable::check(vtabContainer)) {
        //     Rf_error("corrupted vtable, serializer error!");
        // }
        // DispatchTable * oldTab = DispatchTable::unpack(vtabContainer);
        // auto oldRirBody = oldTab->baseline()->body();
        // if (oldRirBody->src != rirBody->src) {
        // }
        insertToBlacklist(hastSym);
        return false;
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

        SEXP hast = getHast(body, CLOENV(inClosure));
        // Set the closure fields.
        SET_BODY(inClosure, vtable->container());

        if (hast != R_NilValue && readyForSerialization(vtable, hast)) {
            #if DEBUG_TABLE_ENTRIES == 1
            std::cout << "(R) Hast: " << CHAR(PRINTNAME(hast)) << " (Adding table, closure and populating src Map): " << inClosure << std::endl;
            #endif
            insertVTable(vtable, hast);
            populateHastSrcData(vtable, hast);
            insertClosObj(inClosure, hast);
        }
    }
};

}

#endif
