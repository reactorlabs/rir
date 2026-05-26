#ifndef RIR_RUNTIME_FEEDBACK
#define RIR_RUNTIME_FEEDBACK

#include "R/r.h"
#include "Rinternals.h"
#include "common.h"
#include "interpreter/profiler.h"
#include "recording_hooks.h"
#include "runtime/RirRuntimeObject.h"
#include <array>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <iostream>
#include <memory>
#include <ostream>
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
enum class ForceBehaviorKind : uint8_t { FBValue, Infer, Always, EnvBit };

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
    uint8_t numTypes : 2;
    uint8_t stateBeforeLastForce : 2;
    uint8_t notScalar : 1;
    uint8_t attribs : 1;
    uint8_t object : 1;
    uint8_t notFastVecelt : 1;

    std::array<uint8_t, MaxTypes> seen;

    ObservedValues() {
        // implicitly happens when writing bytecode stream...
        memset(this, 0, sizeof(ObservedValues));
    }

    void reset() { *this = ObservedValues(); }

    void print(std::ostream& out) const;

  private:
    inline void record(SEXP e) {
        REC_HOOK(uint32_t old; memcpy(&old, this, sizeof(old)));

        // Set attribs flag for every object even if the SEXP does  not
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
};

static_assert(sizeof(ObservedValues) == sizeof(uint32_t),
              "Size needs to fit inside a record_ bc immediate args");

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
    ObservedValues& types(uint32_t idx);

    void record_callee(uint32_t idx, Function* function, SEXP callee,
                       bool invalidateWhenFull = false) {
        callees(idx).record(function, callee, invalidateWhenFull);
        REC_HOOK(recording::recordSC(callees(idx), idx, owner_));
    }

    void record_test(uint32_t idx, const SEXP e) {
        test(idx).record(e);
        REC_HOOK(recording::recordSC(test(idx), idx, owner_));
    }

    void record_type(uint32_t idx, const SEXP e) {
        types(idx).record(e);
        REC_HOOK(recording::recordSC(types(idx), idx, owner_));
    }

    void record_type(uint32_t idx, std::function<void(ObservedValues&)> f) {
        ObservedValues& slot = types(idx);
        f(slot);
    }

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
