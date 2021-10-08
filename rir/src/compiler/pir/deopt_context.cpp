#include "deopt_context.h"
#include "runtime/LazyEnvironment.h"
#include "utils/Pool.h"

namespace rir {
namespace pir {

DeoptContext::DeoptContext(Opcode* pc, LazyEnvironment* env,
                           const std::vector<PirType>& stack,
                           const DeoptReason& reason, SEXP deoptTrigger)
    : pc(pc), stackSize_(stack.size()), envSize_(env->nargs), reason_(reason),
      deoptTrigger_(deoptTrigger) {
    assert(stack.size() <= MAX_STACK);
    assert(env->nargs <= MAX_ENV);
    std::copy(stack.begin(), stack.end(), stack_.begin());
    for (size_t i = 0; i < envSize(); ++i) {
        auto n = Pool::get(env->names[i]);
        env_.at(i) = {TYPEOF(n) == LISTSXP ? CAR(n) : n,
                      PirType(env->getArg(i)), env->missing[i]};
    }
}
} // namespace pir
} // namespace rir
