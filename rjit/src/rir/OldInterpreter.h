#ifdef HAHA

#ifndef RIR_OLD_INTERPRETER_H
#define RIR_OLD_INTERPRETER_H

#include "RDefs.h"

#include "Code.h"

namespace rjit {
namespace rir {

class Interpreter {
    Code* fun;

  public:
    Interpreter(Code* fun) : fun(fun) {}

    SEXP run(SEXP env);

    static void gcCallback(void (*forward_node)(SEXP));
};
}
}
#endif

#endif
