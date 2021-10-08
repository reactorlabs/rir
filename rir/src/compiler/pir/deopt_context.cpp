#include "deopt_context.h"
#include "runtime/LazyEnvironment.h"
#include "utils/Pool.h"

namespace rir {
namespace pir {

DeoptContext::DeoptContext(Opcode* pc, LazyEnvironment* env, R_bcstack_t* base,
                           size_t stackSize, const DeoptReason& reason,
                           SEXP deoptTrigger)
    : pc(pc), stackSize_(stackSize), envSize_(env->nargs), reason_(reason),
      deoptTrigger_(deoptTrigger) {
    assert(stackSize <= MAX_STACK);
    assert(env->nargs <= MAX_ENV);
    for (size_t i = 0; i < stackSize; ++i) {
        auto v = (base + i)->u.sxpval;
        stack_.at(i) = PirType(v);
    }

    for (size_t i = 0; i < envSize(); ++i) {
        auto n = Pool::get(env->names[i]);
        env_.at(i) = {TYPEOF(n) == LISTSXP ? CAR(n) : n,
                      PirType(env->getArg(i)), env->missing[i]};
    }
}
} // namespace pir
} // namespace rir
