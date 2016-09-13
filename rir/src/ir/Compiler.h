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
    Preserve preserve;

  public:
    struct CompilerRes {
        SEXP bc;
        SEXP formals;
    };

    Compiler(SEXP exp) : exp(exp), formals(R_NilValue) { preserve(exp); }

    Compiler(SEXP exp, SEXP formals) : exp(exp), formals(formals) {
        preserve(exp);
        preserve(formals);
    }

    CompilerRes finalize();

    static CompilerRes compileExpression(SEXP ast) {
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
        return c.finalize();
    }
    
    static CompilerRes compileClosure(SEXP ast, SEXP formals) {
        Compiler c(ast, formals);
        return c.finalize();
    }
};
}

#endif
