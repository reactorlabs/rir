#ifndef PROTECT_H
#define PROTECT_H

#include "RDefs.h"

#include <set>
#include <cstddef>

SEXP PROTECT(SEXP);
void UNPROTECT(int);

namespace rjit {

class Protect {
  public:
    Protect(const Protect& other) = delete;

    Protect(){};
    Protect(SEXP initial) { this->operator()(initial); };

    SEXP operator()(SEXP value) {
        PROTECT(value);
        ++protectedValues_;
        return value;
    }

    ~Protect() { UNPROTECT(protectedValues_); }

  private:
    /* Prevents heap allocation. */
    void* operator new(size_t);
    void* operator new[](size_t);
    void operator delete(void*);
    void operator delete[](void*);

    unsigned protectedValues_ = 0;
};

class Precious {
  public:
    static void add(SEXP s) { singleton().add_(s); }

    static void remove(SEXP s) { singleton().remove_(s); }

    static void gcCallback(void (*forward_node)(SEXP)) {
        singleton().doGcCallback(forward_node);
    }

  private:
    static Precious& singleton() {
        static Precious p;
        return p;
    }

    void add_(SEXP s) { precious.insert(s); }

    void remove_(SEXP s) { precious.erase(s); }

    void doGcCallback(void (*forward_node)(SEXP)) {
        for (SEXP e : precious) {
            forward_node(e);
        }
    }

    std::set<SEXP> precious;
};
}

#endif // PROTECT_H
