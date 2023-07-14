#ifndef RIR_RUNTIME_FEEDBACK
#define RIR_RUNTIME_FEEDBACK

#include "R/r.h"
#include "common.h"
#include <array>
#include <cstddef>
#include <cstdint>
#include <iostream>
#include <memory>
#include <ostream>
#include <variant>
#include <vector>

namespace rir {

struct Code;
struct Function;
struct TypeFeedbackSlot;

#pragma pack(push)
#pragma pack(1)

struct ObservedCallees {
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

    void record(Code* caller, SEXP callee, bool invalidateWhenFull = false);
    SEXP getTarget(const Function* function, size_t pos) const;
    void print(std::ostream& out, const Function* function) const;
};

static_assert(sizeof(ObservedCallees) == 4 * sizeof(uint32_t),
              "Size needs to fit inside a record_ bc immediate args");

inline bool fastVeceltOk(SEXP vec) {
    return !Rf_isObject(vec) &&
           (ATTRIB(vec) == R_NilValue || (TAG(ATTRIB(vec)) == R_DimSymbol &&
                                          CDR(ATTRIB(vec)) == R_NilValue));
}

struct ObservedTest {
    enum { None, OnlyTrue, OnlyFalse, Both };
    uint32_t seen : 2;
    uint32_t unused : 30;

    ObservedTest() : seen(0), unused(0) {}

    inline void record(SEXP e) {
        if (e == R_TrueValue) {
            if (seen == None)
                seen = OnlyTrue;
            else if (seen != OnlyTrue)
                seen = Both;
            return;
        }
        if (e == R_FalseValue) {
            if (seen == None)
                seen = OnlyFalse;
            else if (seen != OnlyFalse)
                seen = Both;
            return;
        }
        seen = Both;
    }

    void print(std::ostream& out) const;
};
static_assert(sizeof(ObservedTest) == sizeof(uint32_t),
              "Size needs to fit inside a record_ bc immediate args");

struct ObservedValues {

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

    inline void record(SEXP e) {

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
    }
};
static_assert(sizeof(ObservedValues) == sizeof(uint32_t),
              "Size needs to fit inside a record_ bc immediate args");

enum class Opcode : uint8_t;

// FIXME: rename to FeedbackPosition
struct FeedbackOrigin {
  private:
    // it has to be uint32_t as it it being used in the LLVM lowring code
    // which relies on it being 32bit
    uint32_t idx_ = 0;
    Function* function_ = nullptr;

  public:
    FeedbackOrigin() {}
    FeedbackOrigin(rir::Function* fun, uint32_t idx);

    bool isValid() const;
    TypeFeedbackSlot* slot() const;
    uint32_t idx() const { return idx_; }
    Function* function() const { return function_; }
    void function(Function* fun) { function_ = fun; }

    bool operator==(const FeedbackOrigin& other) const {
        return idx_ == other.idx_ && function_ == other.function_;
    }

    friend std::ostream& operator<<(std::ostream& out,
                                    const FeedbackOrigin& origin) {
        out << (void*)origin.function_ << "#" << origin.idx_;
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

    static DeoptReason unknown() {
        return DeoptReason(FeedbackOrigin(0, 0), Unknown);
    }

    void record(SEXP val) const;

    DeoptReason() = delete;

  private:
    friend struct std::hash<rir::DeoptReason>;
};
static_assert(sizeof(DeoptReason) == 4 * sizeof(uint32_t),
              "Size needs to fit inside a record_deopt_ bc immediate args");

enum class TypeFeedbackKind : uint8_t { Call, Test, Type };

struct TypeFeedbackSlot {
  private:
    union Feedback {
        ObservedCallees callees;
        ObservedValues type;
        ObservedTest test;
    };

    Feedback feedback_;

    TypeFeedbackSlot(TypeFeedbackKind kind, Feedback feedback)
        : feedback_(feedback), kind(kind) {}

  public:
    TypeFeedbackSlot(ObservedCallees callees)
        : feedback_({.callees = callees}), kind(TypeFeedbackKind::Call) {}

    TypeFeedbackSlot(ObservedTest test)
        : feedback_({.test = test}), kind(TypeFeedbackKind::Test) {}

    TypeFeedbackSlot(ObservedValues values)
        : feedback_({.type = values}), kind(TypeFeedbackKind::Type) {}

    TypeFeedbackKind kind;

    void print(std::ostream& out, const Function* function) const;

    ObservedCallees& callees() {
        assert(kind == TypeFeedbackKind::Call);
        return feedback_.callees;
    }

    ObservedTest& test() {
        assert(kind == TypeFeedbackKind::Test);
        return feedback_.test;
    }

    ObservedValues& type() {
        assert(kind == TypeFeedbackKind::Type);
        return feedback_.type;
    }
};

class TypeFeedback {
  private:
    friend Function;

    typedef std::vector<TypeFeedbackSlot> FeedbackSlots;

    Function* owner_;
    FeedbackSlots slots_;

    TypeFeedback(FeedbackSlots&& slots) : owner_(nullptr), slots_(slots) {}

  public:
    static TypeFeedback empty() { return TypeFeedback({}); }
    static TypeFeedback* deserialize(SEXP refTable, R_inpstream_t inp);

    class Builder {
        std::vector<TypeFeedbackSlot> slots_;

      public:
        uint32_t addCallee() {
            slots_.push_back(ObservedCallees());
            return slots_.size() - 1;
        }

        uint32_t addTest() {
            slots_.push_back(ObservedTest());
            return slots_.size() - 1;
        }

        uint32_t addType() {
            slots_.push_back(ObservedValues());
            return slots_.size() - 1;
        }

        TypeFeedback build() { return TypeFeedback(std::move(slots_)); }
    };

    TypeFeedbackSlot& operator[](size_t idx);
    ObservedCallees& callees(uint32_t idx);
    ObservedTest& test(uint32_t idx);
    ObservedValues& types(uint32_t idx);

    void print(std::ostream& out) const;

    TypeFeedbackSlot& record(uint32_t idx, SEXP callee);

    uint32_t size() const { return slots_.size(); }
    void serialize(SEXP refTable, R_outpstream_t out) const;
};

#pragma pack(pop)

} // namespace rir

namespace std {
template <>
struct hash<rir::FeedbackOrigin> {
    std::size_t operator()(const rir::FeedbackOrigin& v) const {
        return hash_combine(hash_combine(0, v.idx()), v.function());
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
