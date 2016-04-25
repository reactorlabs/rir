#ifndef R_VECTOR_H
#define R_VECTOR_H

#include "RDefs.h"
#include "Protect.h"

namespace rjit {

class RVector {
  public:
    RVector(size_t init_size = slack);

    ~RVector() { Precious::remove(vector); }

    void append(SEXP e);

    size_t insert(SEXP e);

    SEXP at(size_t i);

    size_t size();

  private:
    constexpr static size_t slack = 8;
    constexpr static float grow = 1.2f;
    size_t size_;
    size_t capacity_;
    SEXP vector;
};
}

#endif
