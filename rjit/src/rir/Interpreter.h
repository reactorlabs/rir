#ifndef RIR_INTERPRETER_H
#define RIR_INTERPRETER_H

#include "Function.h"
#include "RDefs.h"

namespace rjit {
namespace rir {

class Interpreter {
    Function* fun;

  public:
    Interpreter(Function* fun) : fun(fun) {}

    SEXP run(SEXP env);
};
}
}
#endif
