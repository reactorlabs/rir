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

    void print(std::ostream& out) const {
        if (numTypes) {
            for (size_t i = 0; i < numTypes; ++i) {
                out << Rf_type2char(seen[i]);
                if (i != (unsigned)numTypes - 1)
                    out << ", ";
            }
            out << " (" << (object ? "o" : "") << (attribs ? "a" : "")
                << (notFastVecelt ? "v" : "") << (!notScalar ? "s" : "") << ")";
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

        // Set attribs flag for every object even if the SEXP does not
        // have attributes. The assumption used to be that e having no
        // attributes implies that it is not an object, but this is not
        // the case in some very specific cases:
        //     > df <- data.frame(x=ts(c(41,42,43)), y=c(61,62,63))
        //     > mf <- model.frame(df)
        //     > .Internal(inspect(mf[["x"]]))
        //     @56546cb06390 14 REALSXP g0c3 [OBJ,NAM(2)] (len=3, tl=0) 41,42,43

        notScalar = notScalar || XLENGTH(e) != 1;
        object = object || isObject(e);
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

struct DeoptReason {
    enum Reason : uint32_t {
        None,
        Typecheck,
        DeadCall,
        Calltarget,
        EnvStubMaterialized,
        DeadBranchReached,
    };
    Reason reason;
    Code* srcCode;
    uint32_t originOffset;

    DeoptReason(rir::Code* src, uint32_t originOffset,
                DeoptReason::Reason reason = DeoptReason::None) {
        // assert((uintptr_t)origin.second > (uintptr_t)origin.first);
        this->srcCode = src;
        this->originOffset = originOffset;
        this->reason = reason;
    }

    DeoptReason(rir::Code* src, Opcode* originOffset,
                DeoptReason::Reason reason = DeoptReason::None)
        : DeoptReason(src, (uintptr_t)originOffset - (uintptr_t)src, reason) {}

    Opcode* opcode() const {
        return (Opcode*)((uintptr_t)srcCode + (uintptr_t)originOffset);
    }
};
static_assert(sizeof(DeoptReason) == 4 * sizeof(uint32_t),
              "Size needs to fit inside a record_deopt_ bc immediate args");

#pragma pack(pop)

} // namespace rir
#endif
