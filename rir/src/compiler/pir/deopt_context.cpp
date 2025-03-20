#include "deopt_context.h"
#include "runtime/LazyEnvironment.h"
#include "utils/Pool.h"

namespace rir {
namespace pir {

DeoptContext::DeoptContext()
    : ContinuationContext(), reason_({}, DeoptReason::Unknown) {}

DeoptContext::DeoptContext(Opcode* pc, size_t envSize, SEXP actualEnv,
                           LazyEnvironment* env, bool leaked, R_bcstack_t* base,
                           size_t stackSize, const DeoptReason& reason,
                           SEXP deoptTrigger)
    : ContinuationContext(pc, actualEnv, leaked, base, stackSize),
      reason_(reason) {
    switch (reason.reason) {
    case DeoptReason::Typecheck:
    case DeoptReason::Typecheck2:
        typeCheckTrigger_ = PirType(deoptTrigger);
        break;
    case DeoptReason::DeadBranchReached:
        if (deoptTrigger == R_TrueValue || deoptTrigger == R_FalseValue)
            deadBranchTrigger_ = deoptTrigger;
        break;
    case DeoptReason::CallTarget:
        callTargetTrigger_ = deoptTrigger;
        R_PreserveObject(callTargetTrigger_);
        break;
    case DeoptReason::DeadCall:
    case DeoptReason::ForceAndCall:
    case DeoptReason::Unknown:
    case DeoptReason::EnvStubMaterialized:
        break;
    }
    assert(!(actualEnv && env));
    envSize_ = envSize;
    assert(envSize_ <= MAX_ENV);
    if (env) {
        assert(!leakedEnv_);
        for (size_t i = 0; i < envSize_; ++i) {
            auto n = Pool::get(env->names[i]);
            env_.at(i) = {TYPEOF(n) == LISTSXP ? CAR(n) : n,
                          PirType(env->getArg(i)), env->missing[i]};
        }
    }
}
} // namespace pir
} // namespace rir
