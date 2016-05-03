#include "Protect.h"
#include "RIntlns.h"

namespace rjit {

SEXP Protect::operator()(SEXP value) {
    PROTECT(value);
    ++protectedValues_;
    return value;
}

Protect::~Protect() { UNPROTECT(protectedValues_); }
}
