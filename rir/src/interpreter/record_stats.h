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
    // leaves: ldvar reads plus opaque value results (call / [[ / for /
    // replacement-fn) — both are tree leaves whose type is observed directly.
    // Counted at the leaf record_type_* handlers + the ldvar classify.
    uint64_t leafAlwaysRec = 0; // RecordAlways leaf — recorded every execution
    uint64_t leafOnceRec = 0;   // RecordOnce leaf — first-hit recorded
    uint64_t leafOnceSkip = 0;  // RecordOnce leaf — gated (SKIPPED)
    uint64_t noRecordSkip = 0;  // NoRecord leaf — elided, no opcode (SKIPPED)
    // inner nodes (counted at the inner record_type_* handlers; exptree only).
    // "skip" here = suppressed via shouldNotRecord (the expression-tree
    // elision).
    uint64_t innerRec = 0;        // record_type_inner_         — recorded
    uint64_t innerSkip = 0;       // record_type_inner_         — suppressed
    uint64_t innerNotifyRec = 0;  // record_type_inner_notify_  — recorded
    uint64_t innerNotifySkip = 0; // record_type_inner_notify_  — suppressed
    // Untracked records — every execution of a plain record_type_ opcode that
    // came from recordTypeUntracked() (sites excluded from the optimization:
    // loop bounds, super-assign target, default args, statement results, [[
    // ...). Always record, never skipped. RecordAlways leaves that the
    // post-pass also left as plain record_type_ are NOT counted here — they go
    // to leafAlwaysRec, recovered at runtime via
    // TypeFeedback::isStatsUntracked.
    uint64_t untrackedRec = 0;
    // Prints the summary at exit.
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
