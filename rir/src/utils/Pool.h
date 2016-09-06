
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
    static std::unordered_map<double, pool_idx_t> numbers;
    static std::unordered_map<int, pool_idx_t> ints;
    static std::unordered_map<SEXP, size_t> contents;

  public:
    static pool_idx_t insert(SEXP e) {
        if (contents.count(e))
            return contents.at(e);

        SET_NAMED(e, 2);
        size_t i = cp_pool_add(globalContext(), e);
        contents[e] = i;
        return i;
    }

    static pool_idx_t getNum(double n);
    static pool_idx_t getInt(int n);

    static SEXP get(pool_idx_t i) { return cp_pool_at(globalContext(), i); }
};
}

#endif
