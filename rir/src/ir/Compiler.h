#ifndef RIR_COMPILER_H
#define RIR_COMPILER_H

#include "R/r.h"
#include "R/Preserve.h"

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
        Compiler c(ast);
        return c.finalize();
    }
    
    static CompilerRes compileClosure(SEXP ast, SEXP env, SEXP formals) {
        Compiler c(ast, formals);
        return c.finalize();
    }
};
}

#endif
