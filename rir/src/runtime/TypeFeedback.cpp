#include "TypeFeedback.h"

#include "R/Symbols.h"
#include "R/r.h"
#include "runtime/Code.h"
#include "runtime/Function.h"

#include <cassert>

namespace rir {

void ObservedCallees::record(Code* caller, SEXP callee,
                             bool invalidateWhenFull) {
    if (!Rf_isFunction(callee)) {
        std::cerr << "Warning: skipping recording non-callee " << callee
                  << " of type " << Rf_type2char(TYPEOF(callee)) << std::endl;
        return;
    }

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
    } else {
        if (invalidateWhenFull)
            invalid = true;
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

void DeoptReason::record(SEXP val) const {
    srcCode()->function()->registerDeoptReason(reason);

    switch (reason) {
    case DeoptReason::Unknown:
        break;
    case DeoptReason::DeadBranchReached: {
        assert(*pc() == Opcode::record_test_);
        ObservedTest* feedback = (ObservedTest*)(pc() + 1);
        feedback->seen = ObservedTest::Both;
        break;
    }
    case DeoptReason::Typecheck: {
        assert(*pc() == Opcode::record_type_);
        if (val == symbol::UnknownDeoptTrigger)
            break;
        ObservedValues* feedback = (ObservedValues*)(pc() + 1);
        feedback->record(val);
        if (TYPEOF(val) == PROMSXP) {
            if (PRVALUE(val) == R_UnboundValue &&
                feedback->stateBeforeLastForce < ObservedValues::promise)
                feedback->stateBeforeLastForce = ObservedValues::promise;
            else if (feedback->stateBeforeLastForce <
                     ObservedValues::evaluatedPromise)
                feedback->stateBeforeLastForce =
                    ObservedValues::evaluatedPromise;
        }
        break;
    }
    case DeoptReason::DeadCall:
    case DeoptReason::ForceAndCall:
    case DeoptReason::CallTarget: {
        assert(*pc() == Opcode::record_call_);
        if (val == symbol::UnknownDeoptTrigger)
            break;
        ObservedCallees* feedback = (ObservedCallees*)(pc() + 1);
        feedback->record(srcCode(), val, true);
        assert(feedback->taken > 0);
        break;
    }
    case DeoptReason::EnvStubMaterialized: {
        break;
    }
    }
}

} // namespace rir
