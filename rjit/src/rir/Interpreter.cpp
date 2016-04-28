#include "Interpreter.h"
#include "RIntlns.h"
#include "Function.h"

#include <deque>

namespace rjit {
namespace rir {

namespace {

class Stack {
    std::deque<SEXP> stack;

  public:
    void push(SEXP s) { stack.push_back(s); }

    SEXP pop() {
        SEXP res = stack.back();
        stack.pop_back();
        return res;
    }

    SEXP top() { return stack.back(); }
};

SEXP evalFunction(Function& f) {
    Stack stack;
    Code* cur = f.code[0];
    BC_t* pc = cur->bc;
    BC_t* end = cur->end();

    register SEXP acc = R_NilValue;

    while (pc != end) {
        BC bc = BC::advance(&pc);

        switch (bc.bc) {
        case BC_t::push:
            stack.push(acc);
            acc = bc.immediateConst();
            break;
        default:
            assert(false);
            break;
        }
    }

    return acc;
}
}

SEXP Interpreter::run() { return evalFunction(fun); }
}
}
