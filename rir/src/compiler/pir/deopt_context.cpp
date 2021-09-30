#include "deopt_context.h"
#include "runtime/LazyEnvironment.h"
#include "utils/Pool.h"

namespace rir {
namespace pir {

DeoptContext::DeoptContext(Opcode* pc, LazyEnvironment* env,
                           const std::vector<PirType>& stack,
                           const DeoptReason& reason, SEXP deoptTrigger)
    : pc(pc), stack(stack), reason(reason), deoptTrigger(deoptTrigger) {
    for (size_t i = 0; i < env->nargs; ++i) {
        auto n = Pool::get(env->names[i]);
        this->env.push_back({TYPEOF(n) == LISTSXP ? CAR(n) : n,
                             PirType(env->getArg(i)), env->missing[i]});
    }
}
} // namespace pir
} // namespace rir
