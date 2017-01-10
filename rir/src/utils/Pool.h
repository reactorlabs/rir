
#ifndef RJIT_RIR_POOL
#define RJIT_RIR_POOL

#include "R/RVector.h"

#include <cassert>
#include <cstddef>

#include "ir/BC_inc.h"
#include "R/r.h"

#include <unordered_map>

#include "interpreter/runtime.h"


namespace rir {

class Pool {
    static std::unordered_map<double, PoolIdxT> numbers;
    static std::unordered_map<int, PoolIdxT> ints;
    static std::unordered_map<SEXP, size_t> contents;

  public:
    static PoolIdxT insert(SEXP e) {
        if (contents.count(e))
            return contents.at(e);

        SET_NAMED(e, 2);
        size_t i = cp_pool_add(globalContext(), e);
        contents[e] = i;
        return i;
    }

    static PoolIdxT getNum(double n);
    static PoolIdxT getInt(int n);

    static SEXP get(PoolIdxT i) { return cp_pool_at(globalContext(), i); }
};
}

#endif
