#include "LazyEnvironment.h"
#include "utils/Pool.h"

namespace rir {

size_t LazyEnvironment::getArgIdx(SEXP n) {
    size_t i = 0;
    while (i < nargs) {
        auto name = Pool::get(names[i]);
        if (TYPEOF(name) == LISTSXP)
            name = CAR(name);
        if (name == n)
            break;
        i++;
    }
    return i;
}

SEXP LazyEnvironment::getArg(SEXP n) {
    auto i = getArgIdx(n);
    if (i == nargs)
        return R_UnboundValue;
    return getArg(i);
}

bool LazyEnvironment::isMissing(SEXP n) {
    auto i = getArgIdx(n);
    if (i == nargs)
        return false;
    return isMissing(i);
}

bool LazyEnvironment::isMissing(size_t i) {
    assert(i < nargs);
    return missing[i] || getArg(i) == R_MissingArg;
}

LazyEnvironment* LazyEnvironment::deserialize(SEXP refTable, R_inpstream_t inp) {
    (void)refTable;
    (void)inp;
    assert(false && "TODO LazyEnvironment::deserialize");
}

void LazyEnvironment::serialize(SEXP refTable, R_outpstream_t out) const {
    (void)this;
    (void)refTable;
    (void)out;
    assert(false && "TODO LazyEnvironment::serialize");
}


} // namespace rir
