#ifndef RIR_FUNCTION_H
#define RIR_FUNCTION_H

#include "BC.h"
#include <vector>

namespace rjit {
namespace rir {

class Function {
  public:
    std::vector<Code*> code;

    Function() {}

    void addCode(size_t pos, Code* c) {
        if (pos >= code.size()) {
            code.resize(pos + 1);
        }
        code[pos] = c;
    }

    size_t next() {
        code.push_back(nullptr);
        return code.size() - 1;
    }
};
}
}

#endif
