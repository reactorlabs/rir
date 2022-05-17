#ifndef RIR_R_PRINT
#define RIR_R_PRINT

#include "R/r.h"

#include <string>

namespace rir {

class Print {
  public:
    // Try to deparse but without triggering eval
    static std::string dumpSexp(SEXP s, size_t length = 50);

  private:
    static std::string sexptype2char(SEXPTYPE type);
    static std::string trim(std::string s, size_t n);
    static std::string unsafeTags(SEXP s);
    static std::string dumpPROMSXP(SEXP s);
    static std::string dumpCLOSXP(SEXP s);
    static std::string dumpLISTSXP(SEXP s, size_t limit);
    static std::string dumpLANGSXP(SEXP s);
    static std::string dumpVector(SEXP s, size_t limit);
    static std::string dumpEXTERNALSXP(SEXP s);
};

} // namespace rir

#endif
