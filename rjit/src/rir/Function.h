#ifndef RIR_FUNCTION_H
#define RIR_FUNCTION_H

#include "BC.h"
#include <vector>

namespace rjit {
namespace rir {

class Function {
    std::vector<Code*> code;

  public:
    Function() { code.push_back(nullptr); }

    unsigned addCode(Code* c) {
        code.push_back(c);
        return code.size() - 1;
    }

    void setFun(Code* c) { code[0] = c; }

    Code* getFun() { return code[0]; }
};
}
}

#endif
