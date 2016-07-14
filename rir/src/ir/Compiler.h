#ifndef RIR_COMPILER_H
#define RIR_COMPILER_H

#include "R/r.h"

namespace rir {

class Compiler {
    SEXP exp;
    SEXP formals;

  public:
    struct CompilerRes {
        SEXP bc;
        SEXP formals;
    };

    Compiler(SEXP exp) : exp(exp), formals(R_NilValue) {
        R_PreserveObject(exp);
    }

    Compiler(SEXP exp, SEXP formals) : exp(exp), formals(formals) {
        R_PreserveObject(exp);
        R_PreserveObject(formals);
    }

    ~Compiler() {
        if (formals)
            R_ReleaseObject(formals);
        R_ReleaseObject(exp);
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
