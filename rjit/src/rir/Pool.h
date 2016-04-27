#ifndef RJIT_RIR_POOL
#define RJIT_RIR_POOL

#include "RVector.h"

#include <cstddef>
#include <cassert>

#include "BC_inc.h"

namespace rjit {
namespace rir {

class Pool {
    RVector storage;

  public:
    typedef immediate_t idx_t;

    static Pool& instance() {
        static Pool pool;
        return pool;
    }

    idx_t insert(SEXP e) {
        // TODO: replace the linear search by something faster
        size_t i = storage.insert(e);
        assert(i < (1L << 32));
        return i;
    }

    SEXP get(idx_t i) { return storage.at(i); }
};
}
}

#endif
