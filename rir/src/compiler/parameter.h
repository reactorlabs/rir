#ifndef PIR_PARAMETER_H
#define PIR_PARAMETER_H

#include <stddef.h>

namespace rir {
namespace pir {

struct Parameter {
    static bool DEBUG_DEOPTS;
    static size_t DEOPT_CHAOS;
    static bool DEOPT_CHAOS_NO_RETRIGGER;
    static int DEOPT_CHAOS_SEED;
    static size_t MAX_INPUT_SIZE;

    static const unsigned PIR_WARMUP;
    static const unsigned PIR_OPT_TIME;
    static const unsigned PIR_REOPT_TIME;
    static const unsigned PIR_OPT_BC_SIZE;
    static const unsigned DEOPT_ABANDON;

    static size_t PROMISE_INLINER_MAX_SIZE;

    static size_t INLINER_MAX_SIZE;
    static size_t INLINER_MAX_INLINEE_SIZE;
    static size_t INLINER_INITIAL_FUEL;
    static size_t INLINER_INLINE_UNLIKELY;

    static size_t RECOMPILE_THRESHOLD;

    /// Controls whether we save RIR data in native R serialization (e.g. on quit())
    static bool RIR_PRESERVE;
    static unsigned RIR_SERIALIZE_CHAOS;

    static unsigned RIR_CHECK_PIR_TYPES;

    static unsigned PIR_LLVM_OPT_LEVEL;
    static unsigned PIR_OPT_LEVEL;

    static bool ENABLE_PIR2RIR;

    static bool ENABLE_OSR;

    /// Serialize LLVM bitcode. Enabled regardless of env var iff the compiler
    /// server is running.
    static bool DEBUG_SERIALIZE_LLVM;
};

} // namespace pir
} // namespace rir

#endif
