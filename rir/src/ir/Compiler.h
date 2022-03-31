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
#include "api.h"
#include <chrono>
#include "utils/serializerData.h"
using namespace std::chrono;

#define PRINT_LINKING_STATUS_OVERRIDE 0

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


static void tryLinking(DispatchTable* vtable, SEXP hSym) {

    SEXP serMap = Pool::get(HAST_DEPENDENCY_MAP);
    if (serMap == R_NilValue) return;


    SEXP tabMap = Pool::get(HAST_VTAB_MAP);
    if (tabMap == R_NilValue) {
        SEXP tmp;
        PROTECT(tmp = R_NewEnv(R_EmptyEnv,0,0));
        Pool::patch(HAST_VTAB_MAP, tmp);
        UNPROTECT(1);
        tabMap = Pool::get(HAST_VTAB_MAP);
    }

    if (Rf_findVarInFrame(serMap, hSym) != R_UnboundValue && Rf_findVarInFrame(serMap, hSym) != R_NilValue) {
        #if PRINT_LINKING_STATUS == 1  || PRINT_LINKING_STATUS_OVERRIDE == 1
        std::cout << "symbolExistsInMap: " << CHAR(PRINTNAME(hSym)) << std::endl;
        #endif
        SEXP hastEnvMap = Rf_findVarInFrame(serMap, hSym);

        bool allOffsetsDone = true;

        serializerData::iterateOverOffsets(hastEnvMap, [&] (SEXP offsetSymbol, SEXP offsetEnv) {
            if (offsetEnv != R_NilValue) {
                bool offsetCompletelyLinked = true;
                #if PRINT_LINKING_STATUS == 1  || PRINT_LINKING_STATUS_OVERRIDE == 1
                std::cout << "  offset: " << CHAR(PRINTNAME(offsetSymbol)) << std::endl;
                #endif
                int reqOffset = std::stoi(CHAR(PRINTNAME(offsetSymbol)));
                serializerData::iterateOverContexts(offsetEnv, [&] (SEXP contextSym, SEXP cData) {
                    if (cData != R_NilValue) {
                        #if PRINT_LINKING_STATUS == 1  || PRINT_LINKING_STATUS_OVERRIDE == 1
                        std::cout << "    context: " << CHAR(PRINTNAME(contextSym)) << std::endl;
                        #endif
                        bool allSatisfied = true;
                        for (int j = 1; j < Rf_length(cData); j++) {
                            SEXP dep = VECTOR_ELT(cData, j);
                            if (dep == R_NilValue) continue;
                            if (Rf_findVarInFrame(tabMap, dep) != R_UnboundValue) {
                                SET_VECTOR_ELT(cData, j, R_NilValue);
                            } else {
                                allSatisfied = false;
                            }
                        }
                        if (allSatisfied) {
                            SEXP functionContainer = VECTOR_ELT(cData, 0);

                            DispatchTable * requiredVtab;
                            if (reqOffset == 1) {
                                requiredVtab = vtable;
                            } else {
                                int idx = 0;
                                // int aaa = 0;
                                // vtable->baseline()->body()->printSource(true, aaa);
                                // std::cout << "ReqSrc: " << reqOffset << std::endl;
                                SEXP requiredVtabContainer = vtable->baseline()->body()->getTabAtOffset(true, idx, reqOffset);
                                requiredVtab = DispatchTable::unpack(requiredVtabContainer);
                            }

                            Function * function = Function::unpack(functionContainer);
                            function->body()->populateSrcIdxData();

                            function->inheritFlags(requiredVtab->baseline());
                            requiredVtab->insert(function);
                            #if PRINT_LINKING_STATUS == 1  || PRINT_LINKING_STATUS_OVERRIDE == 1
                            std::cout << "      Linking Success!" << std::endl;
                            #endif
                            // do not need to link this again, linking happens only once.
                            Rf_defineVar(contextSym, R_NilValue, offsetEnv);
                        } else {
                            offsetCompletelyLinked = false;
                            #if PRINT_LINKING_STATUS == 1  || PRINT_LINKING_STATUS_OVERRIDE == 1
                            std::cout << "      Not Linked Yet!" << std::endl;
                            #endif
                        }
                    }

                });
                if (offsetCompletelyLinked) {
                    // we never need to look into this anymore
                    Rf_defineVar(offsetSymbol, R_NilValue, hastEnvMap);
                } else {
                    allOffsetsDone = false;
                }
            }


        });

        if (allOffsetsDone) {
            Rf_defineVar(hSym, R_NilValue, serMap);
        }
    }


    SEXP unlMap = Pool::get(HAST_UNLOCK_MAP);
    if (Rf_findVarInFrame(unlMap, hSym) != R_UnboundValue && Rf_findVarInFrame(unlMap, hSym) != R_NilValue) {
        SEXP workVec = Rf_findVarInFrame(unlMap, hSym);

        for (int j = 0; j < Rf_length(workVec); j++) {
            SEXP workData = VECTOR_ELT(workVec, j);
            SEXP maybeUnlocksHastKey = VECTOR_ELT(workData, 0);
            if (Rf_findVarInFrame(tabMap, maybeUnlocksHastKey) != R_UnboundValue) {
                SEXP tabC = Rf_findVarInFrame(tabMap, maybeUnlocksHastKey);
                DispatchTable * v = DispatchTable::unpack(tabC);
                tryLinking(v, maybeUnlocksHastKey);
            }
        }

        Rf_defineVar(hSym, R_NilValue, unlMap);
    }
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
    static size_t linkTime;

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


        auto start = high_resolution_clock::now();
        if (hast != R_NilValue) {
            insertVTable(vtable, hast);
            insertClosObj(inClosure, hast);
            tryLinking(vtable, hast);
        }
        auto stop = high_resolution_clock::now();
        auto duration = duration_cast<microseconds>(stop - start);
        linkTime += duration.count();
    }
};

}

#endif
