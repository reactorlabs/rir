#include "continuation.h"
#include "compiler/compiler.h"

namespace rir {
namespace pir {

Continuation::Continuation(Closure* closure, const DeoptContext& deoptContext)
    : ClosureVersion(closure, nullptr, true, Compiler::defaultContext,
                     Properties()),
      deoptContext(deoptContext) {}

} // namespace pir
} // namespace rir
