#ifndef RIR_COMPILER_H
#define RIR_COMPILER_H

#include "../RDefs.h"
#include "../Protect.h"

namespace rjit {
namespace rir {

class Function;
class Compiler {
    SEXP exp;

  public:
    Compiler(SEXP exp) : exp(exp) { Precious::add(exp); }

    ~Compiler() { Precious::remove(exp); }

    Function* finalize();

    static Function* compile(SEXP ast) {
        Compiler c(ast);
        return c.finalize();
    }
};
}
}

#endif
