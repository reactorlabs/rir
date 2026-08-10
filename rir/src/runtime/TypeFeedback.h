#ifndef RIR_RUNTIME_FEEDBACK
#define RIR_RUNTIME_FEEDBACK

#include "R/r.h"
#include "Rinternals.h"
#include "bc/recordless.h"
#include "common.h"
#include "interpreter/profiler.h"
#include "interpreter/record_stats.h"
#include "recording_hooks.h"
#include "runtime/RirRuntimeObject.h"
#include <array>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <iostream>
#include <memory>
#include <ostream>
#include <set>
#include <unordered_map>
#include <variant>
#include <vector>

namespace rir {

struct Code;
struct Function;

// Compile-time decision for the force-behavior recording strategy at each
// type-feedback slot. See DefUseAnalysis::classifyUse for how each value is
// chosen. Placed here so both the analysis and the persistent TypeFeedback
// can refer to it.
//
//   FBValue — value is statically known to be a value (post-stvar reach,
//             RecordOnce on a local, for-loop iter var). Runtime FB recording
//             is skipped; the JIT can treat the slot's force-behavior as
//             "value" without any runtime info.
//   Infer   — runtime FB recording is also skipped, but the JIT must infer
//             the force-behavior from the def's slot / useDef chain. Kept
//             distinct from FBValue so the JIT can tell them apart.
//   Always  — record FB unconditionally at runtime (RecordAlways path).
//   EnvBit  — record FB gated by the per-invocation env bitmap
//             (record_type_once_promise_).
//   RecordOnce — record FB once per invocation, gated by the per-code
//             `fired` bitmap (RecordOnce of a formal / outer-controlled var
//             with no local stvar reach). The first recording is the highest
//             lattice point the variable reaches (the value may still be an
//             unforced promise on the first iteration); later iterations are
//             equal or more precise, so one recording suffices.
enum class ForceBehaviorKind : uint8_t {
    FBValue,
    Infer,
    Always,
    EnvBit,
    RecordOnce
};

inline const char* forceBehaviorKindName(ForceBehaviorKind k) {
    switch (k) {
    case ForceBehaviorKind::FBValue:
        return "FBValue";
    case ForceBehaviorKind::Infer:
        return "Infer";
    case ForceBehaviorKind::Always:
        return "Always";
    case ForceBehaviorKind::EnvBit:
        return "EnvBit";
    case ForceBehaviorKind::RecordOnce:
        return "RecordOnce";
    }
    return "?";
}
class TypeFeedback;

enum class FeedbackKind : uint8_t {
    Call,
    Test,
    Type,
};

class FeedbackIndex {
  private:
    static constexpr unsigned IdxBits = 24;
    static constexpr unsigned Undefined = (1 << IdxBits) - 1;

    FeedbackIndex(FeedbackKind kind_, uint32_t idx_) : kind(kind_), idx(idx_) {}
    friend struct std::hash<FeedbackIndex>;

  public:
    FeedbackKind kind;
    uint32_t idx : IdxBits;

    FeedbackIndex() : kind(FeedbackKind::Call), idx(Undefined) {}

    static FeedbackIndex call(uint32_t idx) {
        return FeedbackIndex(FeedbackKind::Call, idx);
    }
    static FeedbackIndex test(uint32_t idx) {
        return FeedbackIndex(FeedbackKind::Test, idx);
    }
    static FeedbackIndex type(uint32_t idx) {
        return FeedbackIndex(FeedbackKind::Type, idx);
    }

    bool isUndefined() const { return idx == Undefined; }

    const char* name() const;

    uint32_t asInteger() const { return *((uint32_t*)this); }

    bool operator==(const FeedbackIndex& other) const {
        return idx == other.idx && kind == other.kind;
    }

    friend std::ostream& operator<<(std::ostream& out,
                                    const FeedbackIndex& index) {
        out << index.name() << "#";
        if (index.isUndefined()) {
            out << "unknown";
        } else {
            out << index.idx;
        }
        return out;
    }
};

static_assert(sizeof(FeedbackIndex) == sizeof(uint32_t),
              "Size needs to fit inside in integer for the llvm transition");

#pragma pack(push)
#pragma pack(1)

struct ObservedCallees {
    friend TypeFeedback;

    static constexpr unsigned CounterBits = 29;
    static constexpr unsigned CounterOverflow = (1 << CounterBits) - 1;
    static constexpr unsigned TargetBits = 2;
    static constexpr unsigned MaxTargets = (1 << TargetBits) - 1;

    // numTargets is sized such that the largest number it can hold is
    // MaxTargets. If it is set to MaxTargets then the targets array is full. We
    // do not distinguish between the case where we have seen MaxTarget
    // different targets and the case where we have seen more than that.
    // Effectively this means we have seen MaxTargets or more.
    uint32_t numTargets : TargetBits;
    uint32_t taken : CounterBits;
    uint32_t invalid : 1;
    std::array<unsigned, MaxTargets> targets;

    SEXP getTarget(const Function* function, size_t pos) const;
    void print(std::ostream& out, const Function* function) const;

  private:
    void record(Function* function, SEXP callee,
                bool invalidateWhenFull = false);
};

static_assert(sizeof(ObservedCallees) == 4 * sizeof(uint32_t),
              "Size needs to fit inside a record_ bc immediate args");

inline bool fastVeceltOk(SEXP vec) {
    return !Rf_isObject(vec) &&
           (ATTRIB(vec) == R_NilValue || (TAG(ATTRIB(vec)) == R_DimSymbol &&
                                          CDR(ATTRIB(vec)) == R_NilValue));
}

struct ObservedTest {
    friend TypeFeedback;

    enum { None, OnlyTrue, OnlyFalse, Both };
    uint32_t seen : 2;
    uint32_t unused : 30;

    ObservedTest() : seen(0), unused(0) {}

    void print(std::ostream& out) const;

  private:
    inline void record(const SEXP e) {
        REC_HOOK(uint32_t old = seen);

        if (e == R_TrueValue) {
            if (seen == None)
                seen = OnlyTrue;
            else if (seen != OnlyTrue)
                seen = Both;
        } else if (e == R_FalseValue) {
            if (seen == None)
                seen = OnlyFalse;
            else if (seen != OnlyFalse)
                seen = Both;
        } else {
            seen = Both;
        }

        REC_HOOK(recording::recordSCChanged(old != seen));
    }
};
static_assert(sizeof(ObservedTest) == sizeof(uint32_t),
              "Size needs to fit inside a record_ bc immediate args");

struct ObservedValues {
    friend TypeFeedback;
    friend RuntimeProfiler;

    enum StateBeforeLastForce {
        unknown,
        value,
        evaluatedPromise,
        promise,
    };

    static constexpr unsigned MaxTypes = 3;
    // byte 0: existing flags
    uint8_t numTypes : 2;
    uint8_t stateBeforeLastForce : 2;
    uint8_t notScalar : 1;
    uint8_t attribs : 1;
    uint8_t object : 1;
    uint8_t notFastVecelt : 1;
    // byte 1: expression-tree flags (5 bits spare)
    uint8_t isLeaf : 1;
    uint8_t shouldNotRecord : 1;
    uint8_t hasPropagatedNotification : 1;
    // bytes 2-4: type observations
    std::array<uint8_t, MaxTypes> seen;
    // bytes 5-7: implicit padding to 8-byte align the pointer below
    ObservedValues* parent;
    // total: 1+1+3+3(pad)+8 = 16 bytes

    ObservedValues() {
        // implicitly happens when writing bytecode stream...
        memset(this, 0, sizeof(ObservedValues));
    }

    void reset() { *this = ObservedValues(); }

    void print(std::ostream& out) const;

  private:
    // Shared core: update all observed-value flags from e.

    __attribute__((always_inline)) void doRecord(SEXP e) {
        REC_HOOK(uint32_t old; memcpy(&old, this, sizeof(old)));

        // Set attribs flag for every object even if the SEXP does not
        // have attributes. The assumption used to be that e having no
        // attributes implies that it is not an object, but this is not
        // the case in some very specific cases:
        //     > df <- data.frame(x=ts(c(41,42,43)), y=c(61,62,63))
        //     > mf <- model.frame(df)
        //     > .Internal(inspect(mf[["x"]]))
        //     @56546cb06390 14 REALSXP g0c3 [OBJ,NAM(2)] (len=3, tl=0) 41,42,43

        notScalar = notScalar || (TYPEOF(e) != S4SXP && XLENGTH(e) != 1);
        object = object || Rf_isObject(e);
        attribs = attribs || object || ATTRIB(e) != R_NilValue;
        notFastVecelt = notFastVecelt || !fastVeceltOk(e);

        uint8_t type = TYPEOF(e);
        if (numTypes < MaxTypes) {
            int i = 0;
            for (; i < numTypes; ++i) {
                if (seen[i] == type)
                    break;
            }
            if (i == numTypes)
                seen[numTypes++] = type;
        }

        REC_HOOK(recording::recordSCChanged(memcmp(&old, this, sizeof(old))));
    }

    // Used by record_type_ / record_type_once_: plain leaves (no parent, no
    // dependents). Leaves are never suppressed (the compiler sets
    // shouldNotRecord = !isLeaf, and the post-pass routes every suppressible
    // inner node to inner_/inner_notify_), so there is no skip check and no
    // notify — just doRecord.
    __attribute__((__always_inline__)) void record(SEXP e) { doRecord(e); }

  public:
    // Inner-node record: skip if suppressed, else doRecord. Any un-suppression
    // of related nodes (own parent and/or NoRecord dependents' parents) is done
    // by the caller (a TypeFeedback record_type_inner_notify method) via
    // notifyRelatedNodes — a standalone inner node (record_type_inner_) skips
    // that entirely, as it has no parent and no dependents to notify.
    __attribute__((__always_inline__)) void recordInner(SEXP e) {
        if (shouldNotRecord)
            return;
        doRecord(e);
    }
};

enum class Opcode : uint8_t;

class FeedbackOrigin {
    FeedbackIndex index_;
    Function* function_ = nullptr;

  public:
    FeedbackOrigin() {}
    FeedbackOrigin(rir::Function* fun, FeedbackIndex idx);

    bool hasSlot() const;
    FeedbackIndex index() const { return index_; }
    uint32_t idx() const { return index_.idx; }
    Function* function() const { return function_; }
    void function(Function* fun);

    bool operator==(const FeedbackOrigin& other) const {
        return index_ == other.index_ && function_ == other.function_;
    }

    friend std::ostream& operator<<(std::ostream& out,
                                    const FeedbackOrigin& origin) {
        out << (void*)origin.function_ << "[" << origin.index_ << "]";
        return out;
    }
};

struct DeoptReason {
  public:
    enum Reason : uint32_t {
        Unknown,
        Typecheck,
        DeadCall,
        CallTarget,
        ForceAndCall,
        EnvStubMaterialized,
        DeadBranchReached,
    };

    DeoptReason::Reason reason;
    FeedbackOrigin origin;

    DeoptReason(const FeedbackOrigin& origin, DeoptReason::Reason reason);

    bool operator==(const DeoptReason& other) const {
        return reason == other.reason && origin == other.origin;
    }

    friend std::ostream& operator<<(std::ostream& out,
                                    const DeoptReason& reason) {
        switch (reason.reason) {
        case Typecheck:
            out << "Typecheck";
            break;
        case DeadCall:
            out << "DeadCall";
            break;
        case CallTarget:
            out << "CallTarget";
            break;
        case ForceAndCall:
            out << "ForceAndCall";
            break;
        case EnvStubMaterialized:
            out << "EnvStubMaterialized";
            break;
        case DeadBranchReached:
            out << "DeadBranchReached";
            break;
        case Unknown:
            out << "Unknown";
            break;
        }
        out << "@" << reason.origin;
        return out;
    }

    static DeoptReason unknown() { return DeoptReason({}, Unknown); }

    void record(SEXP val) const;

    DeoptReason() = delete;

  private:
    friend struct std::hash<rir::DeoptReason>;
};
static_assert(sizeof(DeoptReason) == 4 * sizeof(uint32_t),
              "Size needs to fit inside a record_deopt_ bc immediate args");

#define TYPEFEEDBACK_MAGIC (unsigned)0xfeedbac0

class TypeFeedback : public RirRuntimeObject<TypeFeedback, TYPEFEEDBACK_MAGIC> {
  private:
    friend Function;

    // Sentinel: typeDeps_[i] == NoDep means slot i records directly (no
    // source).
    static constexpr uint32_t NoDep = UINT32_MAX;

    Function* owner_;
    size_t callees_size_;
    size_t tests_size_;
    size_t types_size_;
    ObservedCallees* callees_;
    ObservedTest* tests_;
    ObservedValues* types_;
    // Parallel to types_: typeDeps_[i] is NoDep or the source slot index whose
    // recorded type should be copied into slot i before JIT compilation.
    uint32_t* typeDeps_;
    // Parallel to types_: forceBehaviorKinds_[i] is the compile-time FB
    // recording decision for slot i (default ForceBehaviorKind::Always).
    uint8_t* forceBehaviorKinds_;
    // Reverse of typeDeps_: source slot -> its NoRecord dependent slots. Built
    // in-memory at compile time (buildNoRecordReverseMap); empty when the
    // function has no NoRecord uses. Used at runtime so that when a source
    // records an object it can enable the parents of its dependents — those
    // dependents emit no record opcode and so cannot notify their parents
    // themselves (the "two-step notification"). Not serialized (like parent
    // pointers, it is reconstructed by the compiler).
    std::vector<std::vector<uint32_t>> noRecordSourceToDeps_;
#ifdef RIR_RECORD_STATS
    // Stats only: which slots were emitted via the compiler's
    // recordTypeUntracked() — the genuinely untracked sites (loop bounds,
    // super-assign target, default args, statement results, [[ ...). Lets the
    // record_type_ handler tell a true untracked record apart from a
    // RecordAlways leaf the post-pass also left as a plain record_type_.
    // Reconstructed at compile time (setStatsUntrackedSlots); never serialized.
    std::set<uint32_t> statsUntrackedSlots_;
#endif
    // All the data are stored in this array: callees, tests, types, typeDeps,
    // and forceBehaviorKinds in this order. The constructor sets the above
    // pointers to point at the appropriate locations.
    uint8_t slots_[];

    explicit TypeFeedback(const std::vector<ObservedCallees>& callees,
                          const std::vector<ObservedTest>& tests,
                          const std::vector<ObservedValues>& types,
                          const std::vector<uint32_t>& typeDeps,
                          const std::vector<uint8_t>& forceBehaviorKinds);

  public:
    static TypeFeedback*
    create(const std::vector<ObservedCallees>& callees,
           const std::vector<ObservedTest>& tests,
           const std::vector<ObservedValues>& types,
           const std::vector<uint32_t>& typeDeps = {},
           const std::vector<uint8_t>& forceBehaviorKinds = {});

    static TypeFeedback* empty();
    static TypeFeedback* deserialize(SEXP refTable, R_inpstream_t inp);

    class Builder {
        unsigned ncallees_ = 0;
        unsigned ntests_ = 0;
        unsigned ntypes_ = 0;
        std::vector<uint32_t> typeDeps_;
        // Parallel to typeDeps_: compile-time force-behavior decision per
        // slot. Default Always (entry pushed by addType()).
        std::vector<uint8_t> forceBehaviorKinds_;

      public:
        unsigned typeCount() const { return ntypes_; }
        uint32_t addCallee();
        uint32_t addTest();
        uint32_t addType();
        // Roll back type slot allocations to a saved count. Used to reset
        // after loop-peel body compilation so the main body re-allocates the
        // same slot indices, making peel and main share feedback slots.
        void resetTypesTo(unsigned n);
        // Record that the type slot `slot` should be populated from `source`
        // before JIT compilation rather than being recorded at runtime.
        void setTypeDep(uint32_t slot, uint32_t source);
        // Remember the compile-time force-behavior decision for `slot`.
        // Only call for non-Always slots — the JIT reads this to reconstruct
        // the slot's force-behavior state.
        void setForceBehaviorKind(uint32_t slot, ForceBehaviorKind kind);
        TypeFeedback* build();
    };

    ObservedCallees& callees(uint32_t idx);
    ObservedTest& test(uint32_t idx);
    // Defined here (not in the .cpp) and force-inlined: it's on the hot
    // recordForceBehavior / record_type_ path, called once per recorded load.
    __attribute__((always_inline)) ObservedValues& types(uint32_t idx) {
        return types_[idx];
    }

    void record_callee(uint32_t idx, Function* function, SEXP callee,
                       bool invalidateWhenFull = false) {
        callees(idx).record(function, callee, invalidateWhenFull);
        REC_HOOK(recording::recordSC(callees(idx), idx, owner_));
    }

    void record_test(uint32_t idx, const SEXP e) {
        test(idx).record(e);
        REC_HOOK(recording::recordSC(test(idx), idx, owner_));
    }

    __attribute__((noinline)) void record_type(uint32_t idx, const SEXP e) {
        types(idx).record(e);
        REC_HOOK(recording::recordSC(types(idx), idx, owner_));
    }

    // Standalone inner node: no parent (it is a root) and no NoRecord
    // dependents (not a source). Nothing to un-suppress, so it skips the
    // notify machinery entirely — just skipIfSuppressed + doRecord.
    __attribute__((noinline)) void record_type_inner(uint32_t idx,
                                                     const SEXP e) {
        types(idx).recordInner(e);
        REC_HOOK(recording::recordSC(types(idx), idx, owner_));
    }
    // Inner node that must un-suppress a related node when it sees an object:
    // a non-root inner node un-suppresses its own parent; a root inner node
    // that is a NoRecord source un-suppresses its dependents' parents. (A
    // non-root inner node is never a source — it always has a parent, so it is
    // never a variable's def.) notifyRelatedNodes handles own-parent AND any
    // dependents together, once — the branch that does not apply is a no-op —
    // so one opcode covers both.
    __attribute__((noinline)) void record_type_inner_notify(uint32_t idx,
                                                            const SEXP e) {
        ObservedValues& slot = types(idx);
        slot.recordInner(e); // skipIfSuppressed + doRecord
        notifyRelatedNodes(slot, slot.object,
                           idx); // own parent and/or dependents' parents
        REC_HOOK(recording::recordSC(slot, idx, owner_));
    }
    // A simple leaf that must un-suppress related nodes when it sees an object:
    // either a *source* (a variable with forward NoRecord uses — propagate to
    // its dependents' parents), a leaf *with a parent* (un-suppress its own
    // parent), or both. notifyRelatedNodes handles own-parent AND dependents
    // together, once — so a single opcode family covers all cases; the usually-
    // empty branch is a no-op. (This is why there is no separate _dep_ opcode:
    // a source with no parent is just this handler with an empty parent slot.)
    // A simple leaf that is neither stays plain record_type_ /
    // record_type_once_ and pays nothing.
    __attribute__((noinline)) void record_type_leaf_notify(uint32_t idx,
                                                           const SEXP e) {
        ObservedValues& slot = types(idx);
        slot.doRecord(e); // doRecord only (notify handled below)
        notifyRelatedNodes(slot, slot.object,
                           idx); // own parent + any dependents
        REC_HOOK(recording::recordSC(slot, idx, owner_));
    }

    // Un-suppress the slot's own parent (if any) and the parents of all its
    // NoRecord dependents — done together, once. record_type_ and
    // record_type_once_ (plain simple leaves) don't call this, so they never
    // pay the parent-check / dependent-list lookup.
    __attribute__((__always_inline__)) void
    notifyRelatedNodes(ObservedValues& slot, bool isObject, uint32_t idx) {
        if (!isObject || slot.hasPropagatedNotification)
            return; // only an object un-suppresses, and only once
        slot.hasPropagatedNotification = true;
        if (slot.parent) // this node's own parent (leaf-with-parent case)
            slot.parent->shouldNotRecord = false;
        for (uint32_t d : noRecordSourceToDeps_[idx]) { // dependents' parents
            if (ObservedValues* p = types_[d].parent)
                p->shouldNotRecord = false;
        }
    }

    // Build noRecordSourceToDeps_ (source slot -> its NoRecord dependent slots)
    // from typeDeps_, once at compile time. Always sized to types_size_ so the
    // _dep methods can index it unconditionally.
    void buildNoRecordReverseMap() {
        noRecordSourceToDeps_.assign(types_size_, {});
        for (size_t d = 0; d < types_size_; d++) {
            uint32_t s = typeDeps_[d];
            if (s != NoDep && s < types_size_)
                noRecordSourceToDeps_[s].push_back((uint32_t)d);
        }
    }

    void record_type(uint32_t idx, std::function<void(ObservedValues&)> f) {
        ObservedValues& slot = types(idx);
        f(slot);
    }

#ifdef RIR_RECORD_STATS
    // Record the slots emitted via recordTypeUntracked() (stats attribution).
    void setStatsUntrackedSlots(const std::set<uint32_t>& slots) {
        statsUntrackedSlots_ = slots;
    }
    bool isStatsUntracked(uint32_t idx) const {
        return statsUntrackedSlots_.count(idx) != 0;
    }
#endif

    size_t callees_size() { return callees_size_; }
    size_t tests_size() { return tests_size_; }
    size_t types_size() { return types_size_; }

    bool hasTypeDep(uint32_t idx) const { return typeDep(idx) != NoDep; }

    // Returns the source slot for slot `idx`, or NoDep if it records directly.
    uint32_t typeDep(uint32_t idx) const { return typeDeps_[idx]; }

    // Compile-time force-behavior decision for slot `idx`. Slots default to
    // Always; setForceBehaviorKind is only called for the others.
    ForceBehaviorKind forceBehaviorKind(uint32_t idx) const {
        return static_cast<ForceBehaviorKind>(forceBehaviorKinds_[idx]);
    }
    bool hasForceBehaviorKind(uint32_t idx) const {
        return forceBehaviorKind(idx) != ForceBehaviorKind::Always;
    }

    // For each type slot that has a dependency, copy the source slot's
    // ObservedValues into it. Call this before JIT compilation so that
    // unrecorded slots have the same type info as their source.
    void propagateDeps();

    void print(std::ostream& out) const;

    void serialize(SEXP refTable, R_outpstream_t out) const;

    bool isValid(const FeedbackIndex& index) const;

    Function* owner() const { return owner_; }
};

#pragma pack(pop)

} // namespace rir

namespace std {
template <>
struct hash<rir::FeedbackIndex> {
    std::size_t operator()(const rir::FeedbackIndex& v) const {
        return hash_combine(hash_combine(0, v.kind), v.idx);
    }
};

template <>
struct hash<rir::FeedbackOrigin> {
    std::size_t operator()(const rir::FeedbackOrigin& v) const {
        return hash_combine(hash_combine(0, v.index()), v.function());
    }
};

template <>
struct hash<rir::DeoptReason> {
    std::size_t operator()(const rir::DeoptReason& v) const {
        return hash_combine(hash_combine(0, v.origin), v.reason);
    }
};
} // namespace std

#endif
