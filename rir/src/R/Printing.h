#ifndef RIR_R_PRINT
#define RIR_R_PRINT

#include "R/r.h"

namespace rir {

class Print {
  public:
    static std::string dumpSexp(SEXP src, size_t length = 50);
};

} // namespace rir

#endif
