#ifndef PIR_DEBUGGING
#define PIR_DEBUGGING

#include "utils/EnumSet.h"

#include <regex>

namespace rir {
namespace pir {

// !!!  This list of arguments *must* be exactly equal to the   !!!
// !!!    one in pir.debugFlags in R/rir.R                      !!!

#define LIST_OF_PIR_PRINT_DEBUGGING_FLAGS(V)                                   \
    V(PrintEarlyRir)                                                           \
    V(PrintEarlyPir)                                                           \
    V(PrintOptimizationPasses)                                                 \
    V(PrintOptimizationPhases)                                                 \
    V(PrintPirAfterOpt)                                                        \
    V(PrintCSSA)                                                               \
    V(PrintAllocator)                                                          \
    V(PrintFinalPir)                                                           \
    V(PrintFinalRir)

#define LIST_OF_PIR_DEBUGGING_FLAGS(V)                                         \
    V(ShowWarnings)                                                            \
    V(DryRun)                                                                  \
    V(PrintPassesIntoFolders)                                                  \
    V(PrintIntoFiles)                                                          \
    V(PrintIntoStdout)                                                         \
    V(OmitDeoptBranches)                                                       \
    V(OnlyChanges)                                                             \
    LIST_OF_PIR_PRINT_DEBUGGING_FLAGS(V)

#define LIST_OF_DEBUG_STYLES(V)                                                \
    V(Standard)                                                                \
    V(GraphViz)                                                                \
    V(GraphVizBB)

enum class DebugFlag {
#define V(n) n,
    LIST_OF_PIR_DEBUGGING_FLAGS(V)
#undef V

        FIRST = ShowWarnings,
    LAST = PrintFinalRir
};

enum class DebugStyle {
#define V(style) style,
    LIST_OF_DEBUG_STYLES(V)
#undef V
};

struct DebugOptions {
    typedef EnumSet<DebugFlag, int> DebugFlags;
    DebugFlags flags;
    const std::regex passFilter;
    const std::regex functionFilter;
    DebugStyle style;

    DebugOptions operator|(const DebugFlags& f) const {
        return {flags | f, passFilter, functionFilter, style};
    }
    bool includes(const DebugFlags& otherFlags) const {
        return flags.includes(otherFlags);
    }
    bool intersects(const DebugFlags& otherFlags) const {
        return flags.intersects(otherFlags);
    }

    explicit DebugOptions(unsigned long long flags)
        : flags(flags), passFilter(".*"), functionFilter(".*"),
          style(DebugStyle::Standard) {}
    DebugOptions(const DebugFlags& flags, const std::regex& filter,
                 const std::regex& functionFilter, DebugStyle style)
        : flags(flags), passFilter(filter), functionFilter(functionFilter),
          style(style) {}
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
