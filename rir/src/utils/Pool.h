
#ifndef RJIT_RIR_POOL
#define RJIT_RIR_POOL

#include "R/r.h"
#include "bc/BC_inc.h"
#include "interpreter/instance.h"

#include <unordered_map>
#include <unordered_set>

namespace rir {

class Pool {
    static std::unordered_map<double, BC::PoolIdx> numbers;
    static std::unordered_map<int, BC::PoolIdx> ints;
    static std::unordered_map<SEXP, size_t> contents;
    static std::unordered_set<size_t> patchable;

  public:
    static BC::PoolIdx insert(SEXP e) {
        if (contents.count(e))
            return contents.at(e);

        SET_NAMED(e, 2);
        size_t i = cp_pool_add(e);
        contents[e] = i;
        return i;
    }

    static BC::PoolIdx readItem(SEXP ref_table, R_inpstream_t in);

    static BC::PoolIdx makeSpace() {
        size_t i = cp_pool_add(R_NilValue);
        patchable.insert(i);
        return i;
    }

    static void patch(BC::PoolIdx idx, SEXP e) {
        // Patching must not write to contents, otherwise nasty bugs can occur!
        // Eg.: we makeSpace 42, patch X into 42, then patch Y into 42, X gets
        // garbage collected, Z gets allocated to where X used to be, and now
        // insert of Z finds X in contents and returns 42, which, first, returns
        // Y when looked up in the constant pool, and, second, doesn't store Z
        // which may get collected..
        SET_NAMED(e, 2);
        // Also make sure we are not randomly patching a location that doesn't
        // come from makeSpace
        assert(patchable.count(idx));
        cp_pool_set(idx, e);
    }

    static BC::PoolIdx getNum(double n);
    static BC::PoolIdx getInt(int n);

    static SEXP get(BC::PoolIdx i) { return cp_pool_at(i); }
};

} // namespace rir

#endif
