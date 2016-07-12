#include "Pool.h"
#include "R/r.h"
#include "R/Protect.h"
#include "ir/BC.h"

namespace rir {

std::unordered_map<double, unsigned> Pool::numbers;

pool_idx_t Pool::getNum(double n) {
    if (numbers.count(n))
        return numbers.at(n);

    SEXP s = allocVector(REALSXP, 1);
    Protect p(s);

    REAL(s)[0] = n;
    SET_NAMED(s, 2);

    size_t i = cp_pool_add(globalContext(), s);
    assert(i < MAX_POOL_IDX);

    numbers[n] = i;
    return i;
}
}
