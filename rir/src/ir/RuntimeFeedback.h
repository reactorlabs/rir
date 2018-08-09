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

    uint32_t taken : CounterBits;
    uint32_t numTargets : TargetBits;
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
