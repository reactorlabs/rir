#include "continuation.h"
#include "compiler/compiler.h"

namespace rir {
namespace pir {

Continuation::Continuation(Closure* closure,
                           const ContinuationContext* continuationContext)
    : ClosureVersion(closure, nullptr, true, Compiler::defaultContext,
                     Properties()),
      continuationContext(continuationContext) {}

} // namespace pir
} // namespace rir
