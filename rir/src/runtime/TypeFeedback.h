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
#include <variant>
#include <vector>

namespace rir {

struct Code;
struct Function;
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
    void mergeWith(const ObservedCallees& callee, Function* function);
    inline void addCallee(Function* function, SEXP callee);
    inline bool isEmpty() const {
        return !invalid && (numTargets == 0 && taken == 0 && targets.empty());
    }
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

        REC_HOOK(recording::recordSCChanged((old & 3) != (seen & 3)));
    }
    inline void mergeWith(const ObservedTest& test);
    inline bool isEmpty() const { return seen == None; }
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
        if (numTypes < MaxTypes)
            addType(type);
        REC_HOOK(recording::recordSCChanged(memcmp(&old, this, sizeof(old))));
    }

    inline void mergeWith(const ObservedValues& val) {
        if (stateBeforeLastForce < val.stateBeforeLastForce)
            stateBeforeLastForce = val.stateBeforeLastForce;
        notScalar |= val.notScalar;
        object |= val.object;
        attribs |= val.attribs;
        notFastVecelt |= val.notFastVecelt;
        for (unsigned i = 0; i < val.numTypes; ++i) {
            if (numTypes == MaxTypes)
                return;
            addType(val.seen[i]);
        }
    }

    inline void addType(const uint8_t& type) {
        int i = 0;
        for (; i < numTypes; ++i) {
            if (seen[i] == type)
                break;
        }
        if (i == numTypes)
            seen[numTypes++] = type;
    }

    inline bool isEmpty() const {
        uint32_t zero = 0;
        static_assert(sizeof(ObservedValues) == sizeof(uint32_t),
                      "Comparison assumes 4B size");
        return memcmp(this, &zero, sizeof(ObservedValues)) == 0;
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

struct Context;

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

    void record(SEXP val, const Context& context) const;

    DeoptReason() = delete;

  private:
    friend struct std::hash<rir::DeoptReason>;
};
static_assert(sizeof(DeoptReason) == 4 * sizeof(uint32_t),
              "Size needs to fit inside a record_deopt_ bc immediate args");

#define TYPEFEEDBACK_MAGIC (unsigned)0xfeedbac0

class TypeFeedback : public RirRuntimeObject<TypeFeedback, TYPEFEEDBACK_MAGIC> {
  private:
    size_t callees_size_;
    size_t tests_size_;
    size_t types_size_;
    ObservedCallees* callees_;
    ObservedTest* tests_;
    ObservedValues* types_;

    // Total of runs of RIR code for this TypeFeedback
    // i.e. how many function invocations were recording to this TypeFeedback
    unsigned recordingCount_ = 0;

    // All the data are stored in this array: callees, tests and types in this
    // order. The constructors sets the above pointers to point at the
    // appropriate locations.
    uint8_t slots_[];

    // Constructs TypeFeedback with information in their vectors
    explicit TypeFeedback(const std::vector<ObservedCallees>& callees,
                          const std::vector<ObservedTest>& tests,
                          const std::vector<ObservedValues>& types);

    // Construct TypeFeedback without setting any feedback information,
    // caller should fill the slots
    explicit TypeFeedback(size_t callees, size_t tests, size_t types);

  public:
    static TypeFeedback* create(const std::vector<ObservedCallees>& callees,
                                const std::vector<ObservedTest>& tests,
                                const std::vector<ObservedValues>& types);

    static TypeFeedback* empty();
    static TypeFeedback* deserialize(SEXP refTable, R_inpstream_t inp);

    class Builder {
        unsigned ncallees_ = 0;
        unsigned ntests_ = 0;
        unsigned ntypes_ = 0;

      public:
        uint32_t addCallee();
        uint32_t addTest();
        uint32_t addType();
        TypeFeedback* build();
    };

    ObservedCallees& callees(uint32_t idx);
    ObservedTest& test(uint32_t idx);
    ObservedValues& types(uint32_t idx);

    size_t recordingCount() const { return recordingCount_; }
    void increaseRecordingCount() {
        if (recordingCount_ < UINT_MAX)
            ++recordingCount_;
    }

    void record_callee(uint32_t idx, Function* function, SEXP callee,
                       bool invalidateWhenFull = false) {
        callees(idx).record(function, callee, invalidateWhenFull);
    }

    void record_test(uint32_t idx, const SEXP e) { test(idx).record(e); }

    void record_type(uint32_t idx, const SEXP e) { types(idx).record(e); }

    void record_type(uint32_t idx, std::function<void(ObservedValues&)> f) {
        ObservedValues& slot = types(idx);
        f(slot);
    }

    void record_callee_inc(TypeFeedback* inclusive, uint32_t idx,
                           Function* function, SEXP callee,
                           bool invalidateWhenFull = false);

    void record_test_inc(TypeFeedback* inclusive, uint32_t idx, const SEXP e);

    void record_type_inc(TypeFeedback* inclusive, uint32_t idx, const SEXP e);

    void record_type_inc(TypeFeedback* inclusive, uint32_t idx,
                         std::function<void(ObservedValues&)> f);

    size_t callees_size() { return callees_size_; }
    size_t tests_size() { return tests_size_; }
    size_t types_size() { return types_size_; }

    void print(std::ostream& out) const;

    void serialize(SEXP refTable, R_outpstream_t out) const;

    bool isValid(const FeedbackIndex& index) const;

    static constexpr bool disabled() { return false; }
    TypeFeedback* emptyCopy() const;
    TypeFeedback* copy() const;
    void mergeWith(const TypeFeedback* tf, Function* function);
    void fillWith(const TypeFeedback* tf);
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
