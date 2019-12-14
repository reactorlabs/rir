#ifndef RIR_RUNTIME_FEEDBACK
#define RIR_RUNTIME_FEEDBACK

#include "R/r.h"
#include "common.h"
#include <array>
#include <cstdint>

namespace rir {

struct Code;

#pragma pack(push)
#pragma pack(1)

struct ObservedCallees {
    static constexpr unsigned CounterBits = 30;
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

    void record(Code* caller, SEXP callee);
    SEXP getTarget(const Code* code, size_t pos) const;

    std::array<unsigned, MaxTargets> targets;
};

inline bool fastVeceltOk(SEXP vec) {
    return !isObject(vec) &&
           (ATTRIB(vec) == R_NilValue || (TAG(ATTRIB(vec)) == R_DimSymbol &&
                                          CDR(ATTRIB(vec)) == R_NilValue));
}

struct ObservedType {
    uint8_t sexptype : 5;
    uint8_t scalar : 1;
    uint8_t object : 1;
    uint8_t attribs : 1;

    ObservedType() {}
    explicit ObservedType(SEXP s)
        : sexptype((uint8_t)TYPEOF(s)), scalar(IS_SIMPLE_SCALAR(s, TYPEOF(s))),
          object(isObject(s)), attribs(object || !fastVeceltOk(s)) {
        assert(!object || attribs);
    }

    bool operator==(const ObservedType& other) {
        return memcmp(this, &other, sizeof(ObservedType)) == 0;
    }

    ObservedType operator|(const ObservedType& other) {
        assert(sexptype == other.sexptype);
        ObservedType t;
        t.sexptype = sexptype;
        t.scalar = scalar && other.scalar;
        t.object = object || other.object;
        t.attribs = attribs || other.attribs;
        return t;
    }

    bool isObj() const { return object; }
};
static_assert(sizeof(ObservedCallees) == 4 * sizeof(uint32_t),
              "Size needs to fit inside a record_ bc immediate args");

struct ObservedTest {
    enum { None, OnlyTrue, OnlyFalse, Both };
    uint32_t seen : 2;
    uint32_t unused : 30;

    ObservedTest() : seen(0), unused(0) {}

    RIR_INLINE void record(SEXP e) {
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
        if (seen != Both)
            seen = Both;
    }
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
    uint8_t unused : 4;

    std::array<ObservedType, MaxTypes> seen;

    ObservedValues()
        : numTypes(0), stateBeforeLastForce(StateBeforeLastForce::unknown),
          unused(0) {}

    RIR_INLINE void record(SEXP e) {
        ObservedType type(e);
        if (numTypes < MaxTypes) {
            int i = 0;
            for (; i < numTypes; ++i) {
                if (seen[i] == type)
                    break;
                if (seen[i].sexptype == type.sexptype) {
                    seen[i] = seen[i] | type;
                    return;
                }
            }
            if (i == numTypes)
                seen[numTypes++] = type;
        }
    }
};
static_assert(sizeof(ObservedValues) == sizeof(uint32_t),
              "Size needs to fit inside a record_ bc immediate args");

enum class TypeChecks : uint32_t {
    // Must be bigger than smallest sexptype
    LogicalNonObject = 3326,
    LogicalNonObjectWrapped = 3327,
    LogicalSimpleScalar = 3328,
    LogicalSimpleScalarWrapped = 3329,
    IntegerNonObject = 3330,
    IntegerNonObjectWrapped = 3331,
    IntegerSimpleScalar = 3332,
    IntegerSimpleScalarWrapped = 3333,
    RealNonObject = 3334,
    RealNonObjectWrapped = 3335,
    RealSimpleScalar = 3336,
    RealSimpleScalarWrapped = 3337,
    NotObject = 3338,
    NotObjectWrapped = 3339,
    NoAttribsExceptDim = 3340,
    NoAttribsExceptDimWrapped = 3341,
};

enum class Opcode : uint8_t;

struct DeoptReason {
    enum Reason : uint32_t {
        Typecheck,
        Valuecheck,
        Calltarget,
    };
    Reason reason;
    Code* srcCode;
    uint32_t originOffset;
};
static_assert(sizeof(DeoptReason) == 4 * sizeof(uint32_t),
              "Size needs to fit inside a record_deopt_ bc immediate args");

#pragma pack(pop)

} // namespace rir
#endif
