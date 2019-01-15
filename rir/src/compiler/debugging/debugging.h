#ifndef PIR_DEBUGGING
#define PIR_DEBUGGING

#include "utils/EnumSet.h"

namespace rir {
namespace pir {

// !!!  This list of arguments *must* be exactly equal to the   !!!
// !!!    one in pir.debugFlags in R/rir.R                      !!!

#define LIST_OF_PIR_PRINT_DEBUGGING_FLAGS(V)                                   \
    V(PrintEarlyRir)                                                           \
    V(PrintEarlyPir)                                                           \
    V(PrintOptimizationPasses)                                                 \
    V(PrintPirAfterOpt)                                                        \
    V(PrintCSSA)                                                               \
    V(PrintAllocator)                                                          \
    V(PrintFinalPir)                                                           \
    V(PrintFinalRir)

#define LIST_OF_PIR_DEBUGGING_FLAGS(V)                                         \
    V(ShowWarnings)                                                            \
    V(DryRun)                                                                  \
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

struct DebugOptions {
    typedef EnumSet<DebugFlag, int> DebugFlags;
    DebugFlags flags;
    const std::string passFilter = "";

    DebugOptions operator|(const DebugFlags& flags) const {
        return {flags | flags, passFilter};
    }
    bool includes(const DebugFlags& otherFlags) const {
        return flags.includes(otherFlags);
    }
    bool intersects(const DebugFlags& otherFlags) const {
        return flags.intersects(otherFlags);
    }

    DebugOptions(unsigned long long flags) : flags(flags) {}
    DebugOptions(const DebugFlags& flags, const std::string& filter)
        : flags(flags), passFilter(filter) {}
    DebugOptions() {}
};

const static DebugOptions::DebugFlags PrintDebugPasses =
    DebugOptions::DebugFlags() |
#define V(n) DebugFlag::n |
    LIST_OF_PIR_PRINT_DEBUGGING_FLAGS(V)
#undef V
        DebugOptions::DebugFlags();

} // namespace pir
} // namespace rir

#endif
