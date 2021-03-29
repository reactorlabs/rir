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

        static bool ENABLE_ANNOTATIONS =
            getenv("PIR_ENABLE_ANNOTATIONS") &&
            *getenv("PIR_ENABLE_ANNOTATIONS") == '1';

        if (ENABLE_ANNOTATIONS) {

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

                Rf_findFun(Rf_install("tryCatch"), R_BaseNamespace),

                Rf_findFun(Rf_install("new"), R_BaseNamespace),
                Rf_findFun(Rf_install("initialize"), R_BaseNamespace),
            });

            auto blockTryCatch = [&]() {
                return FORMALS(inClosure) != R_NilValue &&
                       TAG(FORMALS(inClosure)) == Rf_install("expr");
            };
            auto blocknewKccaObject = [&]() {
                auto f = FORMALS(inClosure);
                if (f == R_NilValue)
                    return false;
                if (TAG(f) != Rf_install("x"))
                    return false;
                f = CDR(f);
                if (f == R_NilValue)
                    return false;
                if (TAG(f) != Rf_install("family"))
                    return false;
                return true;
            };

            if (blocked.count(inClosure) || blockTryCatch() ||
                blocknewKccaObject())
                return;

            vtable->baseline()->flags.set(Function::DepromiseArgs);
        }
    }
};

}

#endif
