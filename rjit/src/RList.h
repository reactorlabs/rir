#ifndef RLIST_H
#define RLIST_H

#include "RDefs.h"

namespace rjit {

class RListIter {
  public:
    SEXP pos;

    RListIter(SEXP pos) : pos(pos) {}

    SEXP operator*() { return CAR(pos); }

    void operator++() { pos = CDR(pos); }

    bool operator!=(RListIter& other) { return pos != other.pos; }
};

class RList {
    SEXP list;

  public:
    RList(SEXP list) : list(list) {}

    RListIter begin() { return RListIter(list); }

    RListIter& end() {
        static RListIter end(R_NilValue);
        return end;
    }

    size_t length() {
        size_t len = 0;
        for (auto b = begin(); b != end(); ++b)
            ++len;
        return len;
    }
};
}

#endif
