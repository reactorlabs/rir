#include "RList.h"
#include "RIntlns.h"
#include <cassert>

namespace rjit {

RList::RList(SEXP list) : list(list) {
    assert(TYPEOF(list) == LISTSXP || TYPEOF(list) == NILSXP);
}

SEXP RListIter::tag() { return TAG(pos); }
bool RListIter::hasTag() { return tag() != R_NilValue; }

SEXP RListIter::operator*() { return CAR(pos); }

void RListIter::operator++() { pos = CDR(pos); }
}
