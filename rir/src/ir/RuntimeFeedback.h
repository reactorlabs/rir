#ifndef RIR_RUNTIME_FEEDBACK
#define RIR_RUNTIME_FEEDBACK

#include "R/r.h"
#include <cstdint>

namespace rir {

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
    SEXP targets[MaxTargets];

    void record(SEXP callee) {
        if (taken < CounterOverflow)
            taken++;
        if (numTargets < MaxTargets) {
            int i = 0;
            for (; i < numTargets; ++i)
                if (targets[i] == callee)
                    break;
            if (i == numTargets)
                targets[numTargets++] = callee;
        }
    }
};

static_assert(sizeof(CallFeedback) == 7 * sizeof(uint32_t),
              "Size needs to fit inside a record_ bc immediate args");

#pragma pack(pop)

} // namespace rir

#endif
