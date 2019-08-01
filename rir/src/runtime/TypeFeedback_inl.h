#ifndef RIR_RUNTIME_FEEDBACK_INL_H
#define RIR_RUNTIME_FEEDBACK_INL_H

#include "Code.h"
#include "TypeFeedback.h"

namespace rir {

ObservedType::ObservedType(SEXP s)
    : sexptype((uint8_t)TYPEOF(s)), scalar(IS_SIMPLE_SCALAR(s, TYPEOF(s))),
      object(OBJECT(s)), attribs(ATTRIB(s) != R_NilValue) {}

void ObservedCallees::record(Code* caller, SEXP callee) {
    if (taken < CounterOverflow)
        taken++;
    if (numTargets < MaxTargets) {
        int i = 0;
        for (; i < numTargets; ++i)
            if (caller->getExtraPoolEntry(targets[i]) == callee)
                break;
        if (i == numTargets) {
            auto idx = caller->addExtraPoolEntry(callee);
            targets[numTargets++] = idx;
        }
    }
}

} // namespace rir

#endif
