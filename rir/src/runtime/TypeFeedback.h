#ifndef RIR_RUNTIME_FEEDBACK
#define RIR_RUNTIME_FEEDBACK

#include "R/r.h"
#include "Rinternals.h"
#include "common.h"
#include "interpreter/profiler.h"
#include "runtime/RirRuntimeObject.h"
#include <array>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <iostream>
#include <memory>
#include <ostream>
#include <random>
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
    bool record(Function* function, SEXP callee,
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
    inline bool record(const SEXP e) {
        uint32_t old;
        memcpy(&old, this, sizeof(ObservedTest));

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

        return memcmp(&old, this, sizeof(ObservedTest));
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
    inline bool record(SEXP e) {
        uint32_t old;
        memcpy(&old, this, sizeof(ObservedValues));

        // Set attribs flag for every object even if the SEXP does  not
        // have attributes. The assumption used to be that e having no
        // attributes implies that it is not an object, but this is not
        // the case in some very specific cases:
        //     > df <- data.frame(x=ts(c(41,42,43)), y=c(61,62,63))
        //     > mf <- model.frame(df)
        //     > .Internal(inspect(mf[["x"]]))
        //     @56546cb06390 14 REALSXP g0c3 [OBJ,NAM(2)] (len=3, tl=0) 41,42,43

        notScalar = notScalar || XLENGTH(e) != 1;
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

        return memcmp(&old, this, sizeof(ObservedValues));
    }

    inline bool random_type() {
        static std::default_random_engine gen;
        static std::bernoulli_distribution bdistr(0.5);
        static std::uniform_int_distribution<> tdistr(0, 25);

        uint32_t old;
        memcpy(&old, this, sizeof(ObservedValues));

        notScalar = notScalar || bdistr(gen);
        object = object || bdistr(gen);
        attribs = attribs || object || bdistr(gen);
        notFastVecelt =
            notFastVecelt || !(!object && (!attribs || bdistr(gen)));

        uint8_t type = tdistr(gen);
        // 11 and 12 are not valid sexptypes anymore (were used for factors
        // before). We give more probability to realsxp and strsxp
        if (type == 11) {
            type = 14; // REALSXP
        } else if (type == 12) {
            type = 16; // STRSXP
        }

        if (numTypes < MaxTypes) {
            int i = 0;
            for (; i < numTypes; ++i) {
                if (seen[i] == type)
                    break;
            }
            if (i == numTypes)
                seen[numTypes++] = type;
        }

        return memcmp(&old, this, sizeof(ObservedValues));
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

    size_t version_;
    Function* owner_;
    size_t callees_size_;
    size_t tests_size_;
    size_t types_size_;
    ObservedCallees* callees_;
    ObservedTest* tests_;
    ObservedValues* types_;
    // All the data are stored in this array: callees, tests and types in this
    // order. The constructors sets the above pointers to point at the
    // appropriate locations.
    uint8_t slots_[];

    static const double fuzz_type_feedback;

    explicit TypeFeedback(const std::vector<ObservedCallees>& callees,
                          const std::vector<ObservedTest>& tests,
                          const std::vector<ObservedValues>& types);

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

    void record_callee(uint32_t idx, Function* function, SEXP callee,
                       bool invalidateWhenFull = false) {
        if (callees(idx).record(function, callee, invalidateWhenFull)) {
            version_++;
        }
    }

    void record_test(uint32_t idx, const SEXP e) {
        if (test(idx).record(e)) {
            version_++;
        }
    }

    void record_type(uint32_t idx, const SEXP e) {
        static std::default_random_engine gen;
        static std::uniform_real_distribution<> distr(0., 1.);

        if (types(idx).record(e)) {
            version_++;
        }

        if (fuzz_type_feedback >= 0 && distr(gen) >= fuzz_type_feedback) {
            if (types(idx).random_type()) {
                version_++;
            }
        }
    }

    void record_type(uint32_t idx, std::function<void(ObservedValues&)> f) {
        ObservedValues& slot = types(idx);
        uint32_t o, n;
        memcpy(&o, &slot, sizeof(ObservedValues));
        f(slot);
        memcpy(&n, &slot, sizeof(ObservedValues));
        if (memcmp(&o, &n, sizeof(ObservedValues))) {
            version_++;
        }
    }

    void print(std::ostream& out) const;

    void serialize(SEXP refTable, R_outpstream_t out) const;

    bool isValid(const FeedbackIndex& index) const;

    Function* owner() const { return owner_; }

    // Type feedback is versioned. Each time new feedback
    // in any of the slot is recorded, its version increased.
    // The new is important, if we record already known
    // information, the version is left unchnaged.
    size_t version() const { return version_; }
    void version(size_t version) { version_ = version; }
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
