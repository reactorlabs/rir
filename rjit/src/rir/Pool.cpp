#include "Pool.h"
#include "RIntlns.h"
#include "../Protect.h"

namespace rjit {
namespace rir {

pool_idx_t Pool::getNum(double n) {
    if (numbers.count(n))
        return numbers.at(n);

    SEXP s = allocVector(REALSXP, 1);
    Protect p(s);

    REAL(s)[0] = n;
    SET_NAMED(s, 2);

    size_t i = storage.insert(s);
    assert(i < MAX_POOL_IDX);

    numbers[n] = i;
    return i;
}
}
}
