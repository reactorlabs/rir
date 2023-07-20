#ifndef RIR_RUNTIME_FEEDBACK
#define RIR_RUNTIME_FEEDBACK

#include "R/r.h"
#include "Rinternals.h"
#include "common.h"
#include "runtime/RirRuntimeObject.h"
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
class TypeFeedback;

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

    inline void record(const SEXP e) {
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

inline const char* kind_as_name(TypeFeedbackKind kind) {
    switch (kind) {
    case TypeFeedbackKind::Call:
        return "Call";
        break;
    case TypeFeedbackKind::Test:
        return "Test";
        break;
    case TypeFeedbackKind::Type:
        return "Type";
        break;
    }
}

struct TypeFeedbackSlot {
  private:
    union Feedback {
        ObservedCallees callees;
        ObservedValues type;
        ObservedTest test;
    };

    const TypeFeedbackKind kind_;
    Feedback feedback_;

    TypeFeedbackSlot(TypeFeedbackKind kind, const Feedback&& feedback)
        : kind_(kind), feedback_(feedback) {}

  public:
    static TypeFeedbackSlot createCallees() {
        return TypeFeedbackSlot{TypeFeedbackKind::Call,
                                Feedback{.callees = ObservedCallees()}};
    }

    static TypeFeedbackSlot createTest() {
        return TypeFeedbackSlot{TypeFeedbackKind::Test,
                                Feedback{.test = ObservedTest()}};
    }

    static TypeFeedbackSlot createType() {
        return TypeFeedbackSlot{TypeFeedbackKind::Type,
                                Feedback{.type = ObservedValues()}};
    }

    TypeFeedbackKind kind() { return kind_; }

    ObservedCallees& callees() {
        assert(kind_ == TypeFeedbackKind::Call);
        return feedback_.callees;
    }

    ObservedTest& test() {
        assert(kind_ == TypeFeedbackKind::Test);
        return feedback_.test;
    }

    ObservedValues& type() {
        assert(kind_ == TypeFeedbackKind::Type);
        return feedback_.type;
    }

    void print(std::ostream& out, const Function* function) const;
};

#define TYPEFEEDBACK_MAGIC (unsigned)0xfeedbac0

class TypeFeedback : public RirRuntimeObject<TypeFeedback, TYPEFEEDBACK_MAGIC> {
  private:
    friend Function;

    Function* owner_;
    uint32_t size_;
    TypeFeedbackSlot slots_[];

    explicit TypeFeedback(std::vector<TypeFeedbackSlot>&& slots)
        : RirRuntimeObject(0, 0), owner_(nullptr), size_(slots.size()) {
        if (size_) {
            memcpy(&slots_, slots.data(), size_ * sizeof(TypeFeedbackSlot));
        }
    }

  public:
    static TypeFeedback* create(std::vector<TypeFeedbackSlot>&& slots) {
        size_t dataSize = slots.size() * sizeof(TypeFeedbackSlot);
        size_t objSize = sizeof(TypeFeedback) + dataSize;

        SEXP store = Rf_allocVector(EXTERNALSXP, objSize);
        TypeFeedback* res = new (INTEGER(store)) TypeFeedback(std::move(slots));
        return res;
    }

    static TypeFeedback* empty();
    static TypeFeedback* deserialize(SEXP refTable, R_inpstream_t inp);

    class Builder {
        std::vector<TypeFeedbackSlot> slots_;

      public:
        uint32_t addCallee();
        uint32_t addTest();
        uint32_t addType();
        TypeFeedback* build();
    };

    TypeFeedbackSlot& operator[](size_t idx);
    ObservedCallees& callees(uint32_t idx);
    ObservedTest& test(uint32_t idx);
    ObservedValues& types(uint32_t idx);

    void print(std::ostream& out) const;

    TypeFeedbackSlot& record(uint32_t idx, SEXP callee);

    uint32_t size() const { return size_; }
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
