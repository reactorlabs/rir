#include "TypeFeedback.h"

#include "R/Symbols.h"
#include "R/r.h"
#include "runtime/Code.h"
#include "runtime/Function.h"

#include <cassert>

namespace rir {

void ObservedCallees::record(Code* caller, SEXP callee) {
    if (taken < CounterOverflow)
        taken++;

    // We collapse closures pointing to the same dispatch table on the same
    // slot. That should only happen on the first slot (0). If the first slot is
    // not matched it is pointless to try matching on further slots, since the
    // body is already not stable (hence not monomorphic).
    // Further elements are saved only to check for type stability

    if (numTargets < MaxTargets) {

        int i = 0;

        // fastcase
        if (numTargets == 1) {
            auto storedCallee = caller->getExtraPoolEntry(targets[0]);
            if (storedCallee == callee)
                return;

            if (TYPEOF(storedCallee) == CLOSXP && TYPEOF(callee) == CLOSXP &&
                BODY(storedCallee) == BODY(callee)) {
                if (CLOENV(storedCallee) != CLOENV(callee))
                    stableEnv = false;
                return;
            }
            stableEnv = false;
            i++; // skip first element in the for loop below
        }

        for (; i < numTargets; ++i) {
            auto storedCallee = caller->getExtraPoolEntry(targets[i]);
            if (storedCallee == callee)
                break;
        }

        // not found, will add
        if (i == numTargets) {
            if (numTargets == 0)
                stableEnv = true;

            targets[numTargets++] = caller->addExtraPoolEntry(callee);
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
        feedback->record(srcCode(), val);
        assert(feedback->taken > 0);
        break;
    }
    case DeoptReason::EnvStubMaterialized: {
        break;
    }
    }
}

} // namespace rir
