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

struct ObservedType {
    uint8_t sexptype : 5;
    uint8_t scalar : 1;
    uint8_t object : 1;
    uint8_t attribs : 1;
    ObservedType() {}
    explicit ObservedType(R_bcstack_t* s);
    explicit ObservedType(SEXP s);
    bool operator==(const ObservedType& other) {
        return memcmp(this, &other, sizeof(ObservedType)) == 0;
    }

    bool isObj() const { return object; }
};
static_assert(sizeof(ObservedCallees) == 4 * sizeof(uint32_t),
              "Size needs to fit inside a record_ bc immediate args");

struct ObservedValues {
    static constexpr unsigned MaxTypes = 3;
    uint8_t numTypes;

    std::array<ObservedType, MaxTypes> seen;

    ObservedValues() : numTypes(0) {}

    RIR_INLINE void record(R_bcstack_t* e) {
        ObservedType type(e);
        record(type);
    }

    RIR_INLINE void record(SEXP e) {
        ObservedType type(e);
        record(type);
    }

    void record(ObservedType type) {
        if (numTypes < MaxTypes) {
            int i = 0;
            for (; i < numTypes; ++i)
                if (seen[i] == type)
                    break;
            if (i == numTypes)
                seen[numTypes++] = type;
        }
    }
};
static_assert(sizeof(ObservedValues) == sizeof(uint32_t),
              "Size needs to fit inside a record_ bc immediate args");

#pragma pack(pop)

enum class TypeChecks : uint32_t {
    // Must be bigger than smallest sexptype
    IntegerNonObject = 3330,
    IntegerSimpleScalar = 3331,
    RealNonObject = 3332,
    RealSimpleScalar = 3334,
};

} // namespace rir
#endif
