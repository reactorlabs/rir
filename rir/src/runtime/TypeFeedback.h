#ifndef RIR_RUNTIME_FEEDBACK
#define RIR_RUNTIME_FEEDBACK

#include "R/r.h"
#include "common.h"
#include <array>
#include <cstdint>
#include <iostream>

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
static_assert(sizeof(ObservedCallees) == 4 * sizeof(uint32_t),
              "Size needs to fit inside a record_ bc immediate args");

inline bool fastVeceltOk(SEXP vec) {
    return !isObject(vec) &&
           (ATTRIB(vec) == R_NilValue || (TAG(ATTRIB(vec)) == R_DimSymbol &&
                                          CDR(ATTRIB(vec)) == R_NilValue));
}

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
    uint8_t scalar : 1;
    uint8_t object : 1;
    uint8_t attribs : 1;
    uint8_t unused : 1;

    std::array<uint8_t, MaxTypes> seen;

    ObservedValues()
        : numTypes(0), stateBeforeLastForce(StateBeforeLastForce::unknown),
          scalar(1), object(0), attribs(0), unused(0) {}

    void reset() { *this = ObservedValues(); }

    void print(std::ostream& out) const {
        if (numTypes) {
            for (size_t i = 0; i < numTypes; ++i) {
                out << Rf_type2char(seen[i]);
                if (i != (unsigned)numTypes - 1)
                    out << ", ";
            }
            out << " (" << (object ? "o" : "") << (attribs ? "a" : "")
                << (scalar ? "s" : "") << ")";
            if (stateBeforeLastForce !=
                ObservedValues::StateBeforeLastForce::unknown) {
                out << " | "
                    << ((stateBeforeLastForce ==
                         ObservedValues::StateBeforeLastForce::value)
                            ? "value"
                            : (stateBeforeLastForce ==
                               ObservedValues::StateBeforeLastForce::
                                   evaluatedPromise)
                                  ? "evaluatedPromise"
                                  : "promise");
            }
        } else {
            out << "<?>";
        }
    };

    RIR_INLINE void record(SEXP e) {
        scalar = scalar && IS_SIMPLE_SCALAR(e, TYPEOF(e));
        object = object || isObject(e);
        attribs = attribs || ATTRIB(e) != R_NilValue;

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

struct DeoptReason {
    enum Reason : uint32_t {
        None,
        Typecheck,
        Calltarget,
        EnvStubMaterialized,
        DeadBranchReached,
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
