#ifndef PIR_DEBUGGING
#define PIR_DEBUGGING

#include "utils/EnumSet.h"

namespace rir {
namespace pir {

#ifdef ENABLE_SLOWASSERT
#define LOGGING(code) code
#else
#define LOGGING(code) /* nothing */
#endif

const std::string WARNING_GUARD_STRING = "Guard ignored";

// !!!  This list of arguments *must* be exactly equal to the   !!!
// !!!    one in pir.debugFlags in R/rir.R                      !!!

#define LIST_OF_PIR_PRINT_DEBUGGING_FLAGS(V)                                   \
    V(PrintEarlyRir)                                                           \
    V(PrintEarlyPir)                                                           \
    V(PrintOptimizationPasses)                                                 \
    V(PrintPirAfterOpt)                                                        \
    V(PrintCSSA)                                                               \
    V(PrintLivenessIntervals)                                                  \
    V(PrintFinalPir)                                                           \
    V(PrintFinalRir)

#define LIST_OF_PIR_DEBUGGING_FLAGS(V)                                         \
    V(ShowWarnings)                                                            \
    V(DryRun)                                                                  \
    V(PreserveVersions)                                                        \
    V(DebugAllocator)                                                          \
    V(PrintIntoFiles)                                                          \
    V(PrintIntoStdout)                                                         \
    LIST_OF_PIR_PRINT_DEBUGGING_FLAGS(V)

enum class DebugFlag {
#define V(n) n,
    LIST_OF_PIR_DEBUGGING_FLAGS(V)
#undef V

        FIRST = ShowWarnings,
    LAST = PrintFinalRir
};

typedef EnumSet<DebugFlag> DebugOptions;
const static DebugOptions PrintDebugPasses =
    DebugOptions() |
#define V(n) DebugFlag::n |
    LIST_OF_PIR_PRINT_DEBUGGING_FLAGS(V)
#undef V
        DebugOptions();

} // namespace pir
} // namespace rir
#endif
