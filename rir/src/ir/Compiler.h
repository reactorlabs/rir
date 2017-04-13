#ifndef RIR_COMPILER_H
#define RIR_COMPILER_H

#include "R/r.h"
#include "R/Preserve.h"
#include "utils/FunctionHandle.h"

#include <unordered_map>
#include <iostream>
#include <functional>
#include <cassert>

typedef struct RCNTXT RCNTXT;
extern RCNTXT* R_GlobalContext;
extern "C" SEXP R_syscall(int n, RCNTXT *cptr);
extern "C" SEXP R_sysfunction(int n, RCNTXT *cptr);

namespace rir {

class Compiler {
    SEXP exp;
    SEXP formals;
    SEXP env;
    Preserve preserve;

  public:
    struct CompilerRes {
        SEXP bc;
        SEXP formals;
    };

    Compiler(SEXP exp, SEXP env) : exp(exp), formals(R_NilValue), env(env) {
        preserve(exp);
        if (env != R_NilValue)
            preserve(env);
    }

    Compiler(SEXP exp, SEXP formals, SEXP env) : exp(exp), formals(formals), env(env) {
        preserve(exp);
        preserve(formals);
        if (env != R_NilValue)
            preserve(env);
    }

    CompilerRes finalize();

    static CompilerRes compileExpression(SEXP ast, SEXP env = R_NilValue) {
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
        Compiler c(ast, env);
        return c.finalize();
    }

    static CompilerRes compileClosure(SEXP ast, SEXP formals, SEXP env = R_NilValue) {
        Compiler c(ast, formals, env);
        return c.finalize();
    }
};

}

#endif
