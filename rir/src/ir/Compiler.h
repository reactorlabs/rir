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

typedef struct RCNTXT RCNTXT;
extern "C" SEXP R_syscall(int n, RCNTXT *cptr);
extern "C" SEXP R_sysfunction(int n, RCNTXT *cptr);

namespace rir {

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
        if (TYPEOF(body) == BCODESXP) {
            R_PreserveObject(body);
            body = VECTOR_ELT(CDR(body), 0);
        }

        Compiler c(body, FORMALS(inClosure), CLOENV(inClosure));
        SEXP compiledFun = p(c.finalize());

        // Allocate a new vtable.
        DispatchTable* vtable = DispatchTable::create();

        // Initialize the vtable. Initially the table has one entry, which is
        // the compiled function.
        vtable->baseline(Function::unpack(compiledFun));

        // Set the closure fields.
        SET_BODY(inClosure, vtable->container());

        // std::set<std::string> blackList = {
        //     "source", "integer", "file", "getOption", "readLines", "scan",
        //     "eval", "sys.function", "sys.parent", "formals", "match.arg",
        //     "::",

        //     "tryCatch", "tryCatchOne", "tryCatchList", "doTryCatch",

        //     // flexclust (...)
        //     "newKccaObject", "newKccasimpleObject", "new", "initialize"

        // };

        bool ENABLE_ANNOTATIONS =
            getenv("PIR_ENABLE_ANNOTATIONS") ? true : false;

        if (ENABLE_ANNOTATIONS) {

            static auto tryCatch =
                Rf_findFun(Rf_install("tryCatch"), R_BaseNamespace);

            static auto blocked = std::unordered_set<SEXP>({

                Rf_findFun(Rf_install("source"), R_BaseNamespace),
                Rf_findFun(Rf_install("integer"), R_BaseNamespace),
                Rf_findFun(Rf_install("file"), R_BaseNamespace),
                Rf_findFun(Rf_install("getOption"), R_BaseNamespace),
                Rf_findFun(Rf_install("readLines"), R_BaseNamespace),
                Rf_findFun(Rf_install("scan"), R_BaseNamespace),
                Rf_findFun(Rf_install("eval"), R_BaseNamespace),
                Rf_findFun(Rf_install("sys.function"), R_BaseNamespace),
                Rf_findFun(Rf_install("sys.parent"), R_BaseNamespace),
                Rf_findFun(Rf_install("formals"), R_BaseNamespace),
                Rf_findFun(Rf_install("match.arg"), R_BaseNamespace),
                Rf_findFun(Rf_install("::"), R_BaseNamespace),

                tryCatch,
                // Rf_findFun(Rf_install("tryCatchOne"), R_BaseNamespace),
                // Rf_findFun(Rf_install("tryCatchList"), R_BaseNamespace),
                // Rf_findFun(Rf_install("doTryCatch"), R_BaseNamespace),

                Rf_findFun(Rf_install("new"), R_BaseNamespace),
                Rf_findFun(Rf_install("initialize"), R_BaseNamespace),

                // flexclust (...)
                // Rf_findFun(Rf_install("newKccaObject"), R_BaseNamespace),
                // Rf_findFun(Rf_install("newKccasimpleObject"),
                // R_BaseNamespace),

            }

            );
            // flexclust has to be blacklisted too *****

            SEXP envir = CLOENV(inClosure);
            auto currentCl = envir;
            auto isInnerOfTryCatch = false;
            while (currentCl != R_NilValue) {
                if (currentCl == CLOENV(tryCatch)) {
                    isInnerOfTryCatch = true;
                    break;
                }
                currentCl = ENCLOS(currentCl);
            }

            // auto f1 = Rf_findFun(Rf_install("newKccaObject"), envir);
            // if (f1 != R_NilValue)
            //     blocked.insert(f1);

            // auto f2 = Rf_findFun(Rf_install("newKccasimpleObject"), envir);
            // if (f2 != R_NilValue)
            //     blocked.insert(f2);

            // std::cerr << "executed ann \n";
            if (!blocked.count(inClosure) && !isInnerOfTryCatch) {
                // std::cerr << "annotated: \n";
                vtable->baseline()->flags.set(Function::DepromiseArgs);
            }
        }
    }
};

}

#endif
