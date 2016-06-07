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
    RVector storage;
    std::unordered_map<double, pool_idx_t> numbers;

  public:
    static Pool& instance() {
        static Pool pool;
        return pool;
    }

    pool_idx_t insert(SEXP e) {
        // TODO: replace the linear search by something faster
        size_t i = storage.insert(e);
        assert(i < MAX_POOL_IDX);
        return i;
    }

    pool_idx_t getNum(double n);

    SEXP get(pool_idx_t i) { return storage.at(i); }
};
}
}

#endif
