#ifndef PIR_DEBUGGING
#define PIR_DEBUGGING

#include "utils/EnumSet.h"

namespace rir {
namespace pir {

#define LIST_OF_PIR_PRINT_DEBUGGING_FLAGS(V)                                   \
    V(PrintOriginal)                                                           \
    V(PrintRawPir)                                                             \
    V(PrintOptimizationPasses)                                                 \
    V(PrintInlining)                                                           \
    V(PrintLiveness)                                                           \
    V(PrintStackAllocation)                                                    \
    V(PrintFinalPir)

#define LIST_OF_PIR_DEBUGGING_FLAGS(V)                                         \
    V(ShowWarnings)                                                            \
    V(DryRun)                                                                  \
    V(PreserveVersions)                                                        \
    V(DebugAllocator)                                                          \
    LIST_OF_PIR_PRINT_DEBUGGING_FLAGS(V)

enum class DebugFlag {
#define V(n) n,
    LIST_OF_PIR_DEBUGGING_FLAGS(V)
#undef V

        FIRST = ShowWarnings,
    LAST = PrintFinalPir
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
