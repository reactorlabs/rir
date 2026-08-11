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
    // byte 1: runtime gate for inner nodes. Set by a child whose
    // per-execution signature changed, cleared by this node when it records.
    // See doRecordAndSign / markRelatedDirty. There is no stored "isLeaf" /
    // "shouldNotRecord": nothing reads them at runtime (PirType::merge, the
    // JIT-side consumer, never looks at them) and the opcode already says
    // whether a slot is a leaf or an elidable inner node.
    // 7 bits spare.
    uint8_t dirty : 1;
    // bytes 2-4: type observations
    std::array<uint8_t, MaxTypes> seen;
    // byte 5: signature of the value seen on the PREVIOUS execution.
    // See signatureOf() below for the layout; SigAlwaysDirty (0) means "no
    // signature", i.e. a value that must re-record on every execution.
    //
    // A whole byte rather than 7 bits packed alongside `dirty`, which costs
    // one byte per slot and buys three things:
    //  * no read-modify-write. Sharing a byte with `dirty` meant every update
    //    had to load, mask, or and store just to preserve a neighbouring bit.
    //  * no range guard. TYPEOF reads a 5-bit field (TYPE_BITS), so it is
    //    always 0..31, and the widest encoding fits a byte — every
    //    representable type encodes injectively. At 7 bits the encoding
    //    saturated, so the admission test needed an extra `type <= 30`
    //    comparison to stop a wrapped signature aliasing a different type's,
    //    which would make two distinct types compare equal and so wrongly
    //    suppress a parent.
    //  * sizeof becomes 8, so `types_[idx]` is a scaled load rather than a
    //    multiply by 7 — and that indexing happens on every record, plus
    //    again per parent/dependent in markRelatedDirty.
    uint8_t lastSig;
    // Expression-tree parent, as an index into the owning TypeFeedback's
    // types_ rather than a pointer: every edge is within one array, and 2
    // bytes instead of 8 is most of the difference between a 13- and an
    // 8-byte slot. BIASED BY ONE so that an all-zero slot means "no parent" —
    // the array is memcpy'd from a vector and there is a memset in the
    // constructor, so a 0xFFFF sentinel would be one forgotten initializer
    // away from silently designating slot 0 as everyone's parent.
    uint16_t parentPlus1;
    // total (packed): 1+1+3+1+2 = 8 bytes

    // ---- lastSig encoding ------------------------------------------------
    // Low bits first:
    //   bit 0     hasDim
    //   bit 1     isScalar
    //   bits 2..  TYPEOF + 1   (biased, so a real signature is never 0)
    static constexpr uint8_t SigHasDim = 1 << 0;
    static constexpr uint8_t SigScalar = 1 << 1;
    static constexpr unsigned SigTypeShift = 2;

    // "No signature": a value that cannot be summarised — an object, richer
    // attributes than a lone `dim`, S4, or length 0. Reserved as 0, which the
    // bias above keeps out of the real range. Compared specially so that it
    // never matches even itself: two consecutive unsummarisable values must
    // both re-record, since nothing says they were alike.
    static constexpr uint8_t SigAlwaysDirty = 0;

    // The `type + 1` bias is what keeps 0 reserved for SigAlwaysDirty. Without
    // it, TYPEOF 0 (NILSXP) with neither flag set would encode to 0 and be
    // read back as the sentinel. That would be conservative rather than
    // unsound — such a value would simply always re-record and never suppress
    // its parent — and it is probably unreachable anyway, since NILSXP has no
    // length and the `len != 0` admission test diverts it first. The bias
    // costs nothing and makes "0 is not a real signature" true by
    // construction, instead of resting on an argument about NILSXP that a
    // later change to the admission test could quietly invalidate. With it,
    // the real range starts at (0 + 1) << 2 == 4.
    //
    // TYPEOF reads a 5-bit field, so type <= 31 and the widest encoding is
    // ((31 + 1) << 2) | 3 = 131 — inside the byte, hence no range guard.
    // always_inline, not merely constexpr: this sits in the record hot path
    // and must fold into its caller, never become a call.
    __attribute__((always_inline)) static constexpr uint8_t
    signatureOf(int type, bool isScalar, bool hasDim) {
        return (uint8_t)(((type + 1) << SigTypeShift) |
                         (isScalar ? SigScalar : 0) | (hasDim ? SigHasDim : 0));
    }

    bool hasParent() const { return parentPlus1 != 0; }
    uint32_t parentSlot() const {
        assert(hasParent());
        return (uint32_t)parentPlus1 - 1;
    }
    // Callers must check canReference() first; an unrepresentable edge has to
    // be dropped at compile time (see setTypeFeedbackParents), not truncated.
    void setParent(uint32_t idx) {
        assert(canReference(idx));
        parentPlus1 = (uint16_t)(idx + 1);
    }
    static bool canReference(uint32_t idx) { return idx + 1 <= UINT16_MAX; }

    ObservedValues() {
        // implicitly happens when writing bytecode stream...
        // All-zero is the correct initial state throughout: no types seen, no
        // signature, not dirty, and (thanks to the bias) no parent.
        memset(this, 0, sizeof(ObservedValues));
    }

    void reset() { *this = ObservedValues(); }

    void print(std::ostream& out) const;

  private:
    // Shared core: update all observed-value flags from e.
    //
    // Used by the paths that need no signature: plain leaves (record_type_ /
    // record_type_once_, which have no parent and no NoRecord dependents) and
    // standalone inner nodes (record_type_inner_). They pay nothing for the
    // signature machinery. The paths that DO need it call doRecordAndSign
    // instead, which is a separate function rather than a flag on this one so
    // that neither path carries the other's work.
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

    // doRecord + the per-execution signature, from a SINGLE inspection of e.
    // Returns true when the signature differs from the previous execution's,
    // i.e. when a parent consuming this value may now produce a different
    // result and must re-record.
    //
    // This deliberately duplicates doRecord's flag updates rather than calling
    // it. Doing both from one pass is the entire point: e is read once, into
    // locals, before any store. Two reasons that matters and cannot be left to
    // the compiler:
    //  * XLENGTH is not a field access. It expands to XLENGTH_EX, an
    //    out-of-line call reaching ALTREP_LENGTH, and is not marked pure, so
    //    two of them cannot be folded — the pre-merge code really did emit two
    //    ALTREP_LENGTH calls per record here.
    //  * the flag stores are uint8_t, i.e. char-typed, so they may alias e as
    //    far as the compiler knows; any field of e read after a store has to
    //    be re-loaded.
    //
    // Unlike the accumulated flags, the signature describes THIS execution
    // only. That is what makes it work: the accumulated state is monotone and
    // stops changing while the live value keeps varying underneath it, so
    // watching it would miss e.g. the first `(int, int)` pair after both
    // operands have already been seen as int and double separately.
    //
    // Soundness rests on the result's ObservedValues being a function of its
    // operands' signatures — never on what that function is. Operators for
    // which it is not (`:` and `[`, whose result length comes from operand
    // *values*) are not inner nodes at all.
    __attribute__((always_inline)) bool doRecordAndSign(SEXP e) {
        REC_HOOK(uint32_t old; memcpy(&old, this, sizeof(old)));

        const int type = TYPEOF(e);
        const SEXP attr = ATTRIB(e);
        const bool isObj = Rf_isObject(e);
        const bool isS4 = type == S4SXP;
        const bool hasAttr = attr != R_NilValue;
        // XLENGTH is meaningless on S4, and must not be called on it.
        const R_xlen_t len = isS4 ? 1 : XLENGTH(e);
        // fastVeceltOk(e), spelled out so it reuses the loads above.
        const bool fastOk =
            !isObj &&
            (!hasAttr || (TAG(attr) == R_DimSymbol && CDR(attr) == R_NilValue));

        // fastOk admits exactly the values whose only possible attribute is
        // `dim` — the widest class for which the result stays a function of
        // the operands' signatures. copyMostAttrib skips names/dim/dimnames,
        // so a dim-only operand has nothing for its length-gated path to copy,
        // leaving only R's explicit dim propagation, decidable from
        // hasDim + isScalar. Everything else (objects, richer attributes, S4,
        // length 0) collapses to the sentinel, which never compares equal and
        // so re-records unconditionally. Length 0 must be in that set because
        // R's dim selection branches on `ny != 0` / `nx == 0`, and one
        // isScalar bit cannot separate length 0 from length many
        // (matrix + integer(0) drops dim, matrix + 1:4 keeps it).
        //
        // Given fastOk, a non-empty ATTRIB can only be `dim`, so hasAttr
        // doubles as hasDim.
        const uint8_t sig = (!isS4 && fastOk && len != 0)
                                ? signatureOf(type, len == 1, hasAttr)
                                : SigAlwaysDirty;

        // Hoisted above the updates: an unchanged non-sentinel signature makes
        // every one of them provably redundant, so the whole record collapses
        // to the loads above plus this compare. In steady state that leaves
        // the slot untouched — no read-modify-write on the flag byte, no seen
        // scan, no store — which matters as much for not dirtying the cache
        // line as for the instructions saved.
        //
        // Why each update is a no-op when sig == lastSig != SigAlwaysDirty:
        //   seen           same sig => same type, already added by the first
        //                  execution at this signature (and if numTypes had
        //                  saturated, the scan would not add it either)
        //   notScalar      same type => same isS4; same isScalar bit =>
        //                  same (len != 1)
        //   object         non-sentinel => fastOk => isObj false
        //   attribs        object unchanged, and the hasDim bit pins hasAttr
        //   notFastVecelt  non-sentinel => fastOk => contributes false
        // The induction bottoms out because the first execution at a given
        // signature always takes the full path below.
        //
        // The sentinel is excluded deliberately: it says the value could not
        // be summarised, so nothing can be concluded from two consecutive
        // occurrences of it.
        if (sig != SigAlwaysDirty && sig == lastSig) {
            REC_HOOK(recording::recordSCChanged(0));
            REC_STAT(g_recStats.sigUnchangedNoOp++);
            return false;
        }

        // Same updates as doRecord, from the locals above. See the comment
        // there for why `attribs` also folds in `object`.
        notScalar = notScalar || (!isS4 && len != 1);
        object = object || isObj;
        attribs = attribs || object || hasAttr;
        notFastVecelt = notFastVecelt || !fastOk;

        if (numTypes < MaxTypes) {
            int i = 0;
            for (; i < numTypes; ++i) {
                if (seen[i] == (uint8_t)type)
                    break;
            }
            if (i == numTypes)
                seen[numTypes++] = (uint8_t)type;
        }

        // Before lastSig is written, so this hook still observes exactly the
        // fields it did when the signature lived in a separate function.
        REC_HOOK(recording::recordSCChanged(memcmp(&old, this, sizeof(old))));

        lastSig = sig;
        // Reaching here means the early-out did not fire, i.e. the signature
        // is the sentinel or differs from last time — either way, changed.
        return true;
    }

    // Used by record_type_ / record_type_once_: plain leaves (no parent, no
    // dependents). Leaves are never suppressed (the post-pass routes every
    // suppressible inner node to inner_/inner_notify_), so there is no skip
    // check, no signature to maintain, and nothing to notify — just doRecord.
    __attribute__((__always_inline__)) void record(SEXP e) { doRecord(e); }

  public:
    // Inner-node record: skip unless a child marked us dirty this execution,
    // else record and re-arm. Not dirty means every operand had the same
    // signature as last execution, so the result is the one we already
    // absorbed — and since the accumulated state only ever grows, having
    // absorbed it once is permanent.
    //
    // For a standalone inner node (record_type_inner_): a root with no
    // NoRecord dependents, so nothing to notify and no signature to keep.
    __attribute__((__always_inline__)) void recordInner(SEXP e) {
        if (!dirty)
            return;
        dirty = false;
        doRecord(e);
    }

    // Same, for an inner node that must propagate (record_type_inner_notify_).
    // Returns true only if it both recorded AND its signature changed: a
    // suppressed node's own value did not change either, so it has nothing to
    // tell its parent.
    __attribute__((__always_inline__)) bool recordInnerAndSign(SEXP e) {
        if (!dirty)
            return false;
        dirty = false;
        return doRecordAndSign(e);
    }
};

// The struct is #pragma pack(1) and the feedback array is both sized and
// serialized by sizeof(ObservedValues), so any field that does not fit in the
// existing bits costs a byte per slot and changes the on-disk layout.
// Power of two on purpose: types_[idx] is then a scaled index rather than a
// multiply, and it is indexed on every record.
static_assert(sizeof(ObservedValues) == 8, "ObservedValues must stay 8 bytes");

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
    // Inner node that must mark a related node dirty when its own value
    // changes: a non-root inner node marks its own parent; a root inner node
    // that is a NoRecord source marks its dependents' parents. (A non-root
    // inner node is never a source — it always has a parent, so it is never a
    // variable's def.) markRelatedDirty handles own-parent AND any dependents
    // together — the branch that does not apply is a no-op — so one opcode
    // covers both.
    __attribute__((noinline)) void record_type_inner_notify(uint32_t idx,
                                                            const SEXP e) {
        ObservedValues& slot = types(idx);
        // Only propagate when we actually recorded and our own value changed:
        // if we were skipped, our operands were unchanged, so our value is
        // unchanged too and there is nothing for our parent to re-record.
        if (slot.recordInnerAndSign(e))
            markRelatedDirty(slot, idx);
        REC_HOOK(recording::recordSC(slot, idx, owner_));
    }
    // A simple leaf that must mark related nodes dirty when its per-execution
    // signature changes: either a *source* (a variable with forward NoRecord
    // uses — propagate to its dependents' parents), a leaf *with a parent*
    // (mark its own parent), or both. markRelatedDirty handles own-parent AND
    // dependents together — so a single opcode family covers all cases; the
    // usually-empty branch is a no-op. (This is why there is no separate _dep_
    // opcode: a source with no parent is just this handler with an empty
    // parent slot.)
    // A simple leaf that is neither stays plain record_type_ /
    // record_type_once_ and pays nothing.
    __attribute__((noinline)) void record_type_leaf_notify(uint32_t idx,
                                                           const SEXP e) {
        ObservedValues& slot = types(idx);
        // Leaves always record; the signature comes from the same pass.
        if (slot.doRecordAndSign(e))
            markRelatedDirty(slot, idx); // own parent + any dependents
        REC_HOOK(recording::recordSC(slot, idx, owner_));
    }

    // Mark everything that consumes this slot's value as needing to re-record:
    // its own parent (if any) and the parents of all its NoRecord dependents
    // (which emit no opcode of their own, so they cannot signal for
    // themselves). Called only when the signature actually changed.
    //
    // Unlike the object-gated un-suppression this replaces, it is neither
    // one-shot nor permanent: it fires on every signature change and the
    // consumer clears it after recording. An object operand keeps signature 0,
    // which always compares unequal, so its parent records on every execution
    // for as long as the object keeps showing up — and stops once it does not,
    // which the permanent latch could never do.
    __attribute__((__always_inline__)) void
    markRelatedDirty(ObservedValues& slot, uint32_t idx) {
        if (slot.hasParent()) // this node's own parent (leaf-with-parent case)
            types_[slot.parentSlot()].dirty = true;
        for (uint32_t d : noRecordSourceToDeps_[idx]) { // dependents' parents
            if (types_[d].hasParent())
                types_[types_[d].parentSlot()].dirty = true;
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
