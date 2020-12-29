#ifndef RLIST_H
#define RLIST_H

#include "r.h"

#include <cassert>

namespace rir {

class RListIter {
  public:
    SEXP pos;

    explicit RListIter(SEXP pos) : pos(pos) {}

    SEXP tag();
    bool hasTag();

    SEXP operator*();

    void operator++();

    RListIter operator+(unsigned n);

    bool operator!=(const RListIter& other) { return pos != other.pos; }
};

class RList {
    SEXP list;

  public:
    explicit RList(SEXP list);

    RListIter begin() const { return RListIter(list); }

    SEXP operator[](size_t idx);

    static RListIter& end() {
        static RListIter end(R_NilValue);
        return end;
    }

    size_t length() const {
        size_t len = 0;
        for (auto b = begin(); b != end(); ++b)
            ++len;
        return len;
    }
};
}

#endif
