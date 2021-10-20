#pragma once

#include "closure_version.h"
#include "deopt_context.h"
#include "pir.h"

namespace rir {
namespace pir {

class Continuation : public ClosureVersion {
  public:
    const ContinuationContext* continuationContext;
    Continuation(Closure* closure,
                 const ContinuationContext* continuationContext);
    Continuation* isContinuation() override final { return this; }
};

} // namespace pir
} // namespace rir
