#include "TypeFeedback.h"
#include "R/r.h"
#include "runtime/Code.h"

#include <cassert>

namespace rir {

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

SEXP ObservedCallees::getTarget(const Code* code, size_t pos) const {
    assert(pos < numTargets);
    return code->getExtraPoolEntry(targets[pos]);
}

std::string getDeoptReasonExplanation(DeoptReason::Reason reason) {
    switch (reason) {
    case DeoptReason::None:
        assert(false);
    case DeoptReason::Typecheck:
        return "a typecheck failed";
    case DeoptReason::Calltarget:
        return "actual call target didn't match the assumed one";
    case DeoptReason::EnvStubMaterialized:
        return "we materialized a stub environment";
    case DeoptReason::DeadBranchReached:
        return "we reached a branch that we speculated was dead";
    }
}

} // namespace rir
