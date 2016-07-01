#ifndef RIR_COMPILER_H
#define RIR_COMPILER_H

#include "../Precious.h"
#include "../RDefs.h"

namespace rjit {
namespace rir {

class Compiler {
    SEXP exp;
    SEXP formals;

  public:
    struct CompilerRes {
        SEXP bc;
        SEXP formals;
    };

    Compiler(SEXP exp) : exp(exp), formals(R_NilValue) { Precious::add(exp); }

    Compiler(SEXP exp, SEXP formals) : exp(exp), formals(formals) {
        Precious::add(exp);
        Precious::add(formals);
    }

    ~Compiler() {
        if (formals)
            Precious::remove(formals);
        Precious::remove(exp);
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
}

#endif
