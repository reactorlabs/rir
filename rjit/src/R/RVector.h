#ifndef R_VECTOR_H
#define R_VECTOR_H

#include "r.h"

namespace rir {

class RVector;
class RVectorIter {
  public:
    SEXP operator*();
    void operator++() { pos++; }
    bool operator!=(RVectorIter& other) {
        return vector != other.vector || pos != other.pos;
    }
    RVectorIter(RVector* vector, size_t pos) : vector(vector), pos(pos) {}

  private:
    RVector* vector;
    size_t pos;
};

class RVector {
  public:
    RVector(SEXP vector);
    RVector(size_t init_size = slack);

    ~RVector() {
        R_ReleaseObject(vector);
    }

    void append(SEXP e);

    size_t insert(SEXP e);

    SEXP at(size_t i);

    size_t size();

    explicit operator SEXP() { return vector; }

    SEXP operator[](size_t idx);

    RVectorIter begin() { return RVectorIter(this, 0); }

    RVectorIter end() { return RVectorIter(this, size_); }

  private:
    constexpr static size_t slack = 8;
    constexpr static float grow = 1.2f;
    size_t size_;
    size_t capacity_;
    SEXP vector;
};
}

#endif
