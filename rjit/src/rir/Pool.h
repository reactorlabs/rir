
#ifndef RJIT_RIR_POOL
#define RJIT_RIR_POOL

#include "RVector.h"

#include <cassert>
#include <cstddef>

#include "BC_inc.h"
#include "RDefs.h"

#include <unordered_map>

namespace rjit {
namespace rir {

class Pool {
    static std::unordered_map<double, pool_idx_t> numbers;

  public:
    static pool_idx_t insert(SEXP e) {
        // TODO: replace the linear search by something faster
        size_t i = cp_pool_add(globalContext(), e);
        return i;
    }

    static pool_idx_t getNum(double n);

    static SEXP get(pool_idx_t i) { return cp_pool_at(globalContext(), i); }
};
}
}

#endif
