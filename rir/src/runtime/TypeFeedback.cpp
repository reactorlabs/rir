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

FeedbackOrigin::FeedbackOrigin(rir::Code* src, Opcode* p)
    : offset_((uintptr_t)p - (uintptr_t)src), srcCode_(src) {
    if (p) {
        assert(p >= src->code());
        assert(p < src->endCode());
        assert(pc() == p);
    }
}

DeoptReason::DeoptReason(const FeedbackOrigin& origin,
                         DeoptReason::Reason reason)
    : reason(reason), origin(origin) {
    switch (reason) {
    case DeoptReason::Typecheck:
    case DeoptReason::DeadCall:
    case DeoptReason::CallTarget:
    case DeoptReason::ForceAndCall:
    case DeoptReason::DeadBranchReached: {
        assert(pc());
        auto o = *pc();
        assert(o == Opcode::record_call_ || o == Opcode::record_type_ ||
               o == Opcode::record_test_);
        break;
    }
    case DeoptReason::Unknown:
    case DeoptReason::EnvStubMaterialized:
        break;
    }
}

} // namespace rir
