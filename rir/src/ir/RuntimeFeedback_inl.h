#ifndef RIR_RUNTIME_FEEDBACK_INL_H
#define RIR_RUNTIME_FEEDBACK_INL_H

#include "RuntimeFeedback.h"
#include "runtime/Code.h"

namespace rir {

void ObservedCalles::record(Code* caller, SEXP callee) {
    if (taken < CounterOverflow)
        taken++;
    if (numTargets < MaxTargets) {
        int i = 0;
        for (; i < numTargets; ++i)
            if (targets[i] == callee)
                break;
        if (i == numTargets) {
            caller->addExtraPoolEntry(callee);
            targets[numTargets++] = callee;
        }
    }
}

} // namespace rir

#endif
