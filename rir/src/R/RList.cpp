#include "RList.h"
#include "r.h"
#include <cassert>

namespace rir {

RList::RList(SEXP list) : list(list) {
    assert(TYPEOF(list) == LISTSXP || TYPEOF(list) == LANGSXP || TYPEOF(list) == NILSXP);
}

SEXP RListIter::tag() { return TAG(pos); }
bool RListIter::hasTag() { return tag() != R_NilValue; }

SEXP RListIter::operator*() { return CAR(pos); }

void RListIter::operator++() { pos = CDR(pos); }

RListIter RListIter::operator+(unsigned n) {
    RListIter i(pos);
    while (n--)
        ++i;
    return i;
}

SEXP RList::operator[](size_t idx) {
    SEXP pos = list;
    while (idx-- > 0) {
        assert(pos != R_NilValue);
        pos = CDR(pos);
    }
    return CAR(pos);
}
}
