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

struct CallFeedback {
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

    RIR_INLINE void record(Code* caller, SEXP callee);
    SEXP getTarget(size_t pos);

    std::array<SEXP, MaxTargets> targets;
};

struct RecordedType {
    uint8_t sexptype : 5;
    uint8_t scalar : 1;
    uint8_t object : 1;
    uint8_t attribs : 1;
    RecordedType() {}
    explicit RecordedType(SEXP s);
    bool operator==(const RecordedType& other) {
        return memcmp(this, &other, sizeof(RecordedType)) == 0;
    }

    bool isObj() const { return object; }
};
static_assert(sizeof(CallFeedback) == 7 * sizeof(uint32_t),
              "Size needs to fit inside a record_ bc immediate args");

struct TypeFeedback {
    static constexpr unsigned MaxTypes = 3;
    uint8_t numTypes;

    std::array<RecordedType, MaxTypes> seen;

    TypeFeedback() : numTypes(0) {}

    void record(SEXP e) {
        RecordedType type(e);
        if (numTypes < MaxTypes) {
            int i = 0;
            for (; i < numTypes; ++i)
                if (seen[i] == type)
                    break;
            if (i == numTypes)
                seen[numTypes++] = type;
        }
    }

    bool observedObject() {
        for (const auto& record : seen) {
            if (record.isObj()) {
                return true;
            }
        }
        return false;
    }
};
static_assert(sizeof(TypeFeedback) == sizeof(uint32_t),
              "Size needs to fit inside a record_ bc immediate args");

#pragma pack(pop)

} // namespace rir

#endif
