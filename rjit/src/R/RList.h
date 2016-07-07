#ifndef RLIST_H
#define RLIST_H

#include "RDefs.h"

#include <cassert>

namespace rir {

class RListIter {
  public:
    SEXP pos;

    RListIter(SEXP pos) : pos(pos) {}

    SEXP tag();
    bool hasTag();

    SEXP operator*();

    void operator++();

    bool operator!=(RListIter& other) { return pos != other.pos; }
};

class RList {
    SEXP list;

  public:
    RList(SEXP list);

    RListIter begin() { return RListIter(list); }

    SEXP operator[](size_t idx);

    static RListIter& end() {
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
