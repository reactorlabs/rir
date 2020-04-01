#ifndef RIR_R_PRINT
#define RIR_R_PRINT

#include "R/r.h"
#include "utils/capture_out.h"
#include <iomanip>
#include <sstream>

namespace rir {

static inline std::string dumpSexp(SEXP src, size_t length = 50) {
    CaptureOut rec;
    if (src == R_UnboundValue) {
        std::cout << "-";
    } else {
        Rf_PrintValue(src);
    }
    return rec.oneline(length);
}

} // namespace rir

#endif
