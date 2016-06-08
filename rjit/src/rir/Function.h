#ifndef RIR_FUNCTION_H
#define RIR_FUNCTION_H

#include "BC.h"
#include "Code.h"

#include "RDefs.h"

#include <vector>

namespace rjit {
namespace rir {

// Function is an array of code objects. Usually contained in a BCClosure
class Function {
  public:

    /** Bytecode version used in BCODESXPs to distinguish rir bytecodes from gnu-r ones.
     */
    static constexpr int RIR_MAGIC_VERSION = 0xff;

    /** Calling convention.
     */
    enum class CC : char {
        envLazy,
        stackLazy,
        stackEager,
    };



    std::vector<Code*> code;

    Function() {}

    void addCode(fun_idx_t pos, Code* c) {
        assert(pos < code.size());
        code[pos] = c;
    }

    fun_idx_t next() {
        code.push_back(nullptr);
        assert(code.size() < MAX_FUN_IDX);
        return code.size() - 1;
    }

    SEXP serialize(SEXP formals, num_args_t nargs, CC cc) {

        return nullptr;

    }

};
}
}

#endif
