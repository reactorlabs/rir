#pragma once

#include "R/r_incl.h"
#include <functional>

namespace rir {

class JitCache {
  public:
    static bool enabled;
    static SEXP getEntryOrCreate(SEXP e, std::function<SEXP()> create);
};

} // namespace rir
