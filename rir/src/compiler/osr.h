#pragma once

#include "pir/pir.h"

#include "runtime/Function.h"
#include "runtime/GenericDispatchTable.h"

namespace rir {
namespace pir {

class OSR {
  public:
    static Function* compile(SEXP closure, rir::Code* c,
                             const CallContext * callContext,
                             const ContinuationContext& ctx);
    static Function* deoptlessDispatch(SEXP closure, rir::Code* c,
                                       const CallContext * callContext,
                                       const DeoptContext& ctx);
};

typedef GenericDispatchTable<DeoptContext, Function, 5> DeoptlessDispatchTable;

} // namespace pir
} // namespace rir
