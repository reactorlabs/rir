#include "utils/Pool.h"
#include "R/Protect.h"
#include "UUIDPool.h"

namespace rir {

std::unordered_map<double, unsigned> Pool::numbers;
std::unordered_map<int, unsigned> Pool::ints;
std::unordered_map<SEXP, size_t> Pool::contents;
std::unordered_set<size_t> Pool::patchable;

BC::PoolIdx Pool::readItem(SEXP ref_table, R_inpstream_t in) {
    return insert(UUIDPool::readItem(ref_table, in));
}

BC::PoolIdx Pool::getNum(double n) {
    if (numbers.count(n))
        return numbers.at(n);

    SEXP s = Rf_allocVector(REALSXP, 1);
    Protect p(s);

    REAL(s)[0] = n;
    SET_NAMED(s, 2);

    size_t i = cp_pool_add(s);
    assert(i < BC::MAX_POOL_IDX);

    numbers[n] = i;
    return i;
}

BC::PoolIdx Pool::getInt(int n) {
    if (ints.count(n))
        return ints.at(n);

    SEXP s = Rf_allocVector(INTSXP, 1);
    Protect p(s);

    INTEGER(s)[0] = n;
    SET_NAMED(s, 2);

    size_t i = cp_pool_add(s);
    assert(i < BC::MAX_POOL_IDX);

    ints[n] = i;
    return i;
}

} // namespace rir
