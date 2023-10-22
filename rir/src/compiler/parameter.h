#ifndef PIR_PARAMETER_H
#define PIR_PARAMETER_H

#include <stddef.h>
#include <vector>

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

    /// Enabled by default, but PIR_OSR=0 will disable
    static bool ENABLE_OSR;
    /// Enable OSR even during serialization, where it's known to break, and on
    /// the compiler client (not dry-run), where it's also known to break and
    /// the client shouldn't be serializing code anyways.
    ///
    /// Disabled by default, but PIR_OSR=1 will enable
    static bool FORCE_ENABLE_OSR;

    /// Log every time a closure is compiled (no OSR) and how long it takes.
    static bool PIR_MEASURE_COMPILED_CLOSURES;

    /// Serialize LLVM bitcode. Enabled regardless of env var iff the compiler
    /// server is running, otherwise enabled if PIR_PIR_DEBUG_SERIALIZE_LLVM is set
    static bool SERIALIZE_LLVM;

    static bool PIR_GRAPH_PRINT_RIR_OBJECTS;
    static const char* PIR_GRAPH_PRINT_RIR_OBJECTS_PATH;
    static unsigned PIR_GRAPH_PRINT_RIR_OBJECTS_FREQUENCY;

    static bool PIR_TRACE_SERIALIZATION;
    static unsigned PIR_TRACE_SERIALIZATION_MAX_RAW_PRINT_LENGTH;
    static std::vector<unsigned> PIR_TRACE_SERIALIZATION_EXCLUDE_FLAGS;
    static bool PIR_MEASURE_SERIALIZATION;
    static bool PIR_LOG_INTERNING;
    static bool PIR_WARN_INTERNING;
    static bool PIR_MEASURE_INTERNING;
    static bool PIR_TRACE_COMPILER_PEER;
    static bool PIR_LOG_COMPILER_PEER;
    static bool PIR_WARN_COMPILER_PEER;
    static bool PIR_MEASURE_CLIENT_SERVER;
};

} // namespace pir
} // namespace rir

#endif
