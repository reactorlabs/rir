#ifndef PROTECT_H
#define PROTECT_H

#include "RDefs.h"

#include <cstddef>

namespace rjit {

class Protect {
  public:
    Protect(const Protect& other) = delete;

    Protect(){};
    Protect(SEXP init) {
        Rf_protect(init);
        ++protectedValues_;
    };

    SEXP operator()(SEXP value) {
        Rf_protect(value);
        ++protectedValues_;
        return value;
    }

    ~Protect() { Rf_unprotect(protectedValues_); }

  private:
    /* Prevents heap allocation. */
    void* operator new(size_t);
    void* operator new[](size_t);
    void operator delete(void*);
    void operator delete[](void*);

    unsigned protectedValues_ = 0;
};
}

#endif // PROTECT_H
