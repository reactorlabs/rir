#ifndef RIR_RECORD_STATS_H
#define RIR_RECORD_STATS_H

// ---------------------------------------------------------------------------
// Record-skip instrumentation. Counts, at runtime, how many type-record
// operations the recordless build avoids vs. a baseline that records the type
// after every value load.
//
// Model: in the baseline (recordless OFF, profiling ON) every value load
// (ldvar_ / ldvar_cached_* / ldvar_for_update_*) is immediately followed by a
// record_type_ instruction. Recordless replaces that with one of:
//   * record_type_       — RecordAlways: records on every execution
//   * record_type_once_  — RecordOnce: records on the first execution only,
//                          later executions are gated (SKIPPED)
//   * <nothing>          — NoRecord: the type is inferable from a source load,
//                          so no record instruction is emitted at all. There is
//                          no opcode to instrument, so we detect it in the load
//                          handler: if the next opcode is not a record
//                          instruction, this load's record was elided
//                          (SKIPPED).
//
// Compile-time toggle: define RIR_RECORD_STATS to enable (counters + an at-exit
// summary printed to stderr on every process exit). Leave it undefined for
// perf/production builds so evalRirCode stays byte-identical and the hot loop
// pays zero overhead.

//#define RIR_RECORD_STATS

#ifdef RIR_RECORD_STATS
#include <cstdint>
#endif

namespace rir {

#ifdef RIR_RECORD_STATS

struct RecordSkipStats {
    // Recordings that actually happen, counted at the record_type_* handlers:
    uint64_t typeAlways = 0;   // record_type_       executed (recorded)
    uint64_t typeOnceRec = 0;  // record_type_once_  1st hit  (recorded)
    uint64_t typeOnceSkip = 0; // record_type_once_  gated    (SKIPPED)
    // Counted at the value-load handlers by classifying the *next* opcode, so
    // we can attribute recordings to ldvar leaves vs. inner expression nodes:
    uint64_t ldvarRec = 0; // load followed by a record that fires (recorded)
    uint64_t ldvarOnceSkip = 0; // load followed by a gated record_type_once_
    uint64_t noRecordSkip =
        0; // load w/o a following record (elided, inferable)
    // Prints the summary (when the RIR_RECORD_STATS env var is set) at exit.
    ~RecordSkipStats();
};

// Single process-wide instance; its destructor emits the summary at exit.
extern RecordSkipStats g_recStats;

#define REC_STAT(stmt) stmt

#else

#define REC_STAT(stmt) ((void)0)

#endif

} // namespace rir

#endif // RIR_RECORD_STATS_H
