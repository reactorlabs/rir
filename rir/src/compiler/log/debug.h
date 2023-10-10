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
    V(PrintLLVM)                                                               \
    V(PrintAllocator)                                                          \
    V(PrintFinalPir)

#define LIST_OF_PIR_DEBUGGING_FLAGS(V)                                         \
    V(ShowWarnings)                                                            \
    V(DryRun)                                                                  \
    V(PrintPassesIntoFolders)                                                  \
    V(PrintUnbuffered)                                                         \
    V(PrintToStdout)                                                           \
    V(PrintInstructionIds)                                                     \
    V(OmitDeoptBranches)                                                       \
    V(OnlyChanges)                                                             \
    V(LLVMDebugInfo)                                                           \
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
    LAST = PrintFinalPir
};

enum class DebugStyle {
#define V(style) style,
    LIST_OF_DEBUG_STYLES(V)
#undef V
};

struct DebugOptions {
    typedef EnumSet<DebugFlag, int> DebugFlags;
    DebugFlags flags;
    std::regex passFilter;
    std::string passFilterString;
    std::regex functionFilter;
    std::string functionFilterString;
    DebugStyle style;

    DebugOptions operator|(const DebugFlags& f) const {
        return {flags | f, passFilter, passFilterString, functionFilter, functionFilterString, style};
    }
    bool includes(const DebugFlags& otherFlags) const {
        return flags.includes(otherFlags);
    }
    bool includes(const DebugFlag& otherFlag) const {
        return flags.includes(otherFlag);
    }
    bool intersects(const DebugFlags& otherFlags) const {
        return flags.intersects(otherFlags);
    }

    explicit DebugOptions(int flags)
        : DebugOptions(DebugFlags(flags)) {}
    explicit DebugOptions(DebugFlags flags)
        : DebugOptions(flags, ".*", ".*",
                       DebugStyle::Standard) {}
    DebugOptions(const DebugFlags& flags, const std::string& passFilter,
                 const std::string& functionFilter, DebugStyle style)
        : flags(flags), passFilter(std::regex(passFilter)),
          passFilterString(passFilter), functionFilter(functionFilter),
          functionFilterString(functionFilter), style(style) {}
    DebugOptions(const DebugFlags& flags, std::regex passFilter,
                 std::string passFilterString,
                 std::regex functionFilter,
                 std::string functionFilterString,
                 DebugStyle style)
        : flags(flags), passFilter(std::move(passFilter)),
          passFilterString(std::move(passFilterString)),
          functionFilter(std::move(functionFilter)),
          functionFilterString(std::move(functionFilterString)), style(style) {}
    DebugOptions() : DebugOptions(0) {}

    bool multipleFiles() const {
        return includes(DebugFlag::PrintPassesIntoFolders) ||
               style != DebugStyle::Standard;
    }

    friend std::ostream& operator<<(std::ostream& out, const DebugOptions& o) {
        out << "DebugOptions(";
        bool first = true;
#define V(n)                                                                   \
        if (o.includes(DebugFlag::n)) {                                        \
            if (!first) out << ", ";                                           \
            out << #n;                                                         \
            first = false;                                                     \
        }
        LIST_OF_PIR_DEBUGGING_FLAGS(V)
#undef V
        if (o.passFilterString != ".*") {
            if (!first) out << ", ";
            out << "passFilter=" << o.passFilterString;
            first = false;
        }
        if (o.functionFilterString != ".*") {
            if (!first) out << ", ";
            out << "functionFilter=" << o.functionFilterString;
            first = false;
        }
        if (o.style != DebugStyle::Standard) {
            if (!first) out << ", ";
            out << "style=" << (int)o.style;
            first = false;
        }
        out << ")";
        return out;
    }

    static DebugOptions DefaultDebugOptions;
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
