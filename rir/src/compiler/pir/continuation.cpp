#include "continuation.h"
#include "compiler/compiler.h"

namespace rir {
namespace pir {

Continuation::Continuation(Closure* closure, rir::Function* fun,
                           const ContinuationContext* continuationContext)
    : ClosureVersion(closure, fun, true, Compiler::defaultContext,
                     Properties()),
      continuationContext(continuationContext) {}

} // namespace pir
} // namespace rir
