#ifndef RIR_FUNCTION_H
#define RIR_FUNCTION_H

#include "BC.h"
#include <vector>
#include "RDefs.h"

namespace rjit {
namespace rir {

class Function {
  public:
    std::vector<Code*> code;
    std::vector<SEXP> ast;

    Function() {}

    void addCode(fun_idx_t pos, Code* c, SEXP a) {
        assert(pos < code.size());
        code[pos] = c;
        ast[pos] = a;
    }

    fun_idx_t next() {
        code.push_back(nullptr);
        ast.push_back(nullptr);
        assert(code.size() < MAX_FUN_IDX);
        return code.size() - 1;
    }
};
}
}

#endif
