#ifndef RIR_R_PRINT
#define RIR_R_PRINT

#include "R/r.h"

#include <string>

namespace rir {

class Print {
  public:
    static std::string dumpSexp(SEXP s, size_t length = 60);
};

} // namespace rir

#endif
