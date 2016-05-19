#ifndef RIR_COMPILER_H
#define RIR_COMPILER_H

#include "../RDefs.h"
#include "../Protect.h"

namespace rjit {
namespace rir {

class Function;
class Compiler {
    SEXP exp;
    SEXP formals;

  public:
    Compiler(SEXP exp) : exp(exp), formals(nullptr) { Precious::add(exp); }

    Compiler(SEXP exp, SEXP formals) : exp(exp), formals(formals) {
        Precious::add(exp);
        Precious::add(formals);
    }

    ~Compiler() {
        if (formals)
            Precious::remove(formals);
        Precious::remove(exp);
    }

    Function* finalize();

    static Function* compile(SEXP ast) {
        Compiler c(ast);
        return c.finalize();
    }
};
}
}

#endif
