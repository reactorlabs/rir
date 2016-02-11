#ifndef PROTECT_H
#define PROTECT_H

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
}

#endif // PROTECT_H
