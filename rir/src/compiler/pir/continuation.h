#pragma once

#include "closure_version.h"
#include "deopt_context.h"
#include "pir.h"

namespace rir {
namespace pir {

class Continuation : public ClosureVersion {
  public:
    const ContinuationContext* continuationContext;
    Continuation(Closure* closure, rir::Function* fun,
                 const ContinuationContext* continuationContext);
    Continuation* isContinuation() override final { return this; }

    bool typeFeedbackCleanupHasRun = false;
};

} // namespace pir
} // namespace rir
