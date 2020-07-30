#include "LazyEnvironment.h"
#include "utils/Pool.h"

namespace rir {

size_t LazyEnvironment::getArgIdx(SEXP n) {
    size_t i = 0;
    while (i < nargs) {
        if (Pool::get(names[i]) == n)
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
    return missing[i] || getArg(i) == R_MissingArg;
}

} // namespace rir
