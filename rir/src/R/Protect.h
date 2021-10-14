#ifndef PROTECT_H
#define PROTECT_H

#include "r.h"

#include <cstddef>

namespace rir {

class Protect {
  public:
    Protect() {}
    explicit Protect(SEXP init) {
        Rf_protect(init);
        ++protectedValues_;
    }
    Protect(const Protect& other) = delete;
    ~Protect() { clear(); }

    SEXP operator()(SEXP value) {
        Rf_protect(value);
        ++protectedValues_;
        return value;
    }

    void clear() { Rf_unprotect(protectedValues_); }

  private:
    /* Prevents heap allocation. */
    void* operator new(size_t);
    void* operator new[](size_t);
    void operator delete(void*);
    void operator delete[](void*);

    unsigned protectedValues_ = 0;
};

} // namespace rir

#endif // PROTECT_H
