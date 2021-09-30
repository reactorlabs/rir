#pragma once

#include "closure_version.h"
#include "deopt_context.h"
#include "pir.h"

namespace rir {
namespace pir {

class Continuation : public ClosureVersion {
  public:
    DeoptContext deoptContext;
    Continuation(Closure* closure, const DeoptContext& deoptContext);
    Continuation* isContinuation() override final { return this; }
};

} // namespace pir
} // namespace rir
