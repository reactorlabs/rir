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
// Inner nodes (the expression-tree elision) are a second, independent axis and
// have their own table: an operator whose result is a function of its operands'
// recorded feedback records only when a child signals that its per-execution
// signature changed. Baseline has no such notion — it records every operator
// result — so the "should" column for those is simply every execution.
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
    // leaves: ldvar reads plus opaque value results (call / `[` / `[[` / `:` /
    // for / replacement-fn) — both are tree leaves whose type is observed
    // directly. Counted at the leaf record_type_* handlers + the ldvar
    // classify. `[` and `:` were once elidable inner nodes and are counted here
    // now: their result depends on operand *values*, not operand types, so it
    // cannot be inferred from the operands' feedback.
    uint64_t leafAlwaysRec = 0; // RecordAlways leaf — recorded every execution
    uint64_t leafOnceRec = 0;   // RecordOnce leaf — first-hit recorded
    uint64_t leafOnceSkip = 0;  // RecordOnce leaf — gated (SKIPPED)
    uint64_t noRecordSkip = 0;  // NoRecord leaf — elided, no opcode (SKIPPED)
    // Subset of the "recorded" counts above: a record opcode that ran but
    // updated nothing, because the value's per-execution signature matched the
    // previous one — so every flag update and the seen scan were provably
    // redundant and doRecordAndSign returned early. These still count as
    // recorded (the instruction did execute and the slot was read), but no
    // state changed and the slot's cache line stayed clean. Reported as an
    // extra line, not a table row, since it overlaps the rows above.
    uint64_t sigUnchangedNoOp = 0;
    // inner nodes (counted at the inner record_type_* handlers; exptree only).
    // "skip" here = suppressed, i.e. the slot's `dirty` bit was clear: every
    // operand had the same per-execution signature as last time, so the result
    // is the one already absorbed. (This used to be a permanent
    // shouldNotRecord latch that only an object operand could clear; it is now
    // per-execution and re-armable, so a node can go back to being suppressed
    // once its operands settle.)
    uint64_t innerRec = 0;        // record_type_inner_         — recorded
    uint64_t innerSkip = 0;       // record_type_inner_         — suppressed
    uint64_t innerNotifyRec = 0;  // record_type_inner_notify_  — recorded
    uint64_t innerNotifySkip = 0; // record_type_inner_notify_  — suppressed
    // Untracked records — every execution of a plain record_type_ opcode that
    // came from recordTypeUntracked() (sites excluded from the optimization:
    // loop bounds, super-assign target read-for-update, default args
    // ...). Always record, never skipped. RecordAlways leaves that the
    // post-pass also left as plain record_type_ are NOT counted here — they go
    // to leafAlwaysRec, recovered at runtime via
    // TypeFeedback::isStatsUntracked. Note `[`/`[[` are NOT untracked: they are
    // recordTypeOpaqueResult() leaves (see leafAlwaysRec above), since they can
    // be defs, NoRecord sources, or notify a parent.
    uint64_t untrackedRec = 0;
    // Force-behavior (FB) recording — a separate feedback dimension
    // (ObservedValues::stateBeforeLastForce) piggybacked on the same slots but
    // driven by its own dispatch in interp.cpp. In the baseline there is a
    // single recordForceBehavior function used by every load; recordless
    // splits the ldvar_cached_ family into per-FB-kind opcodes/wrappers, but
    // still needs the original generic dispatcher for non-cached loads
    // (ldvar_, ldvar_for_update_*, ldvar_super_, ldddvar_).
    uint64_t fbGenericRec = 0;  // recordForceBehavior (generic) — recorded
    uint64_t fbGenericSkip = 0; // recordForceBehavior (generic) — skipped
                                // (once-gated-and-fired)
    uint64_t fbGenericBail = 0; // recordForceBehavior (generic) — the opcode
                                // after the load matched none of the
                                // recognized value-type record opcodes
                                // (NoRecord elision, ldddvar_, ...), so no
                                // record follows at all. Reported separately,
                                // not as a table row.
    uint64_t fbAlwaysRec = 0;   // recordForceBehaviorAlways (ldvar_cached_) —
                                // unconditional, never skipped
    uint64_t fbRecordOnceRec = 0;  // recordForceBehaviorRecordOnce
                                   // (ldvar_cached_fbRecordOnce_) — first hit
    uint64_t fbRecordOnceSkip = 0; // recordForceBehaviorRecordOnce — gated
                                   // (already fired)
    uint64_t fbNoRecordSkip =
        0; // ldvar_cached_noRecordFB_ (FBValue/Infer) —
           // FB recording skipped entirely at compile time
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
