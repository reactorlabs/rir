#pragma once

#include "common.h"
#include "continuation_context.h"
#include "ir/BC_inc.h"
#include "runtime/LazyEnvironment.h"

#include <vector>

namespace rir {
namespace pir {

struct DeoptContext : public ContinuationContext {
  private:
    DeoptReason reason_;
    SEXP deoptTrigger_ = nullptr;

  public:
    const DeoptReason& reason() const { return reason_; }
    SEXP deoptTrigger() const { return deoptTrigger_; }

    DeoptContext(Opcode* pc, LazyEnvironment* env, R_bcstack_t* base,
                 size_t stackSize, const DeoptReason& reason,
                 SEXP deoptTrigger);

    const DeoptContext* asDeoptContext() const override final { return this; }
};

} // namespace pir
} // namespace rir
