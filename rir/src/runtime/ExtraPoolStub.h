//
// Created by Jakob Hain on 10/9/23.
//

#pragma once

#include "R/r_incl.h"
#include <cstddef>

namespace rir {

struct Code;

class ExtraPoolStub {
  public:
    /// Return whether the SEXP is a known extra pool stub
    static bool check(SEXP sexp);
    /// Assert the SEXP is a known extra pool stub and return its index
    static size_t unpack(SEXP sexp);
    /// Create an SEXP stubbing the extra pool entry at the given index
    static SEXP create(size_t index);
    /// Add entries to the code object's pool until it's `size`.
    static void pad(Code* codeWithPool, size_t size);
};

} // namespace rir
