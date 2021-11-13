#include "continuation_context.h"
#include "runtime/LazyEnvironment.h"
#include "utils/Pool.h"

namespace rir {
namespace pir {

ContinuationContext::ContinuationContext(Opcode* pc, SEXP env, bool leaked,
                                         R_bcstack_t* base, size_t stackSize)
    : pc_(pc), stackSize_(stackSize), leakedEnv_(leaked) {
    assert(stackSize <= MAX_STACK);
    for (size_t i = 0; i < stackSize; ++i) {
        auto v = (base + i)->u.sxpval;
        stack_.at(i) = PirType(v);
    }

    if (!leaked && env) {
        auto l = Rf_length(FRAME(env));
        assert(l <= (int)MAX_ENV);
        leakedEnv_ = false;
        envSize_ = l;
        auto f = FRAME(env);
        size_t i = 0;
        while (f != R_NilValue) {
            auto n = TAG(f);
            assert(i < (size_t)l);
            env_.at(i) = {n, PirType(CAR(f)), MISSING(f)};
            f = CDR(f);
            i++;
        }
    }
}
} // namespace pir
} // namespace rir
