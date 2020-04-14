#ifndef RIR_R_PRINT
#define RIR_R_PRINT

#include "R/r.h"
#include "utils/capture_out.h"
#include <algorithm>
#include <cctype>
#include <functional>
#include <iomanip>
#include <locale>
#include <sstream>

namespace rir {

// --- From
// https://stackoverflow.com/questions/216823/whats-the-best-way-to-trim-stdstring/25385766

// trim from start (in place)
static inline void ltrim(std::string& s) {
    s.erase(s.begin(), std::find_if(s.begin(), s.end(),
                                    [](int ch) { return !std::isspace(ch); }));
}

// trim from end (in place)
static inline void rtrim(std::string& s) {
    s.erase(std::find_if(s.rbegin(), s.rend(),
                         [](int ch) { return !std::isspace(ch); })
                .base(),
            s.end());
}

// trim from both ends (in place)
static inline void trim(std::string& s) {
    ltrim(s);
    rtrim(s);
}

// ---

static inline std::string dumpSexp(SEXP src, size_t length = 50) {
    CaptureOut rec;
    if (src == R_UnboundValue) {
        std::cout << "-";
    } else {
        Rf_PrintValue(src);
    }
    std::string result = rec.oneline(length);
    trim(result);
    return result;
}

} // namespace rir

#endif
