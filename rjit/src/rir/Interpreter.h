#ifndef RIR_INTERPRETER_H
#define RIR_INTERPRETER_H

#include "Function.h"

namespace rjit {
namespace rir {

class Interpreter {
    Function& fun;

  public:
    Interpreter(Function& fun) : fun(fun) {}

    SEXP run();
};
}
}
#endif
