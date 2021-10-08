#pragma once

#include "pir/pir.h"

#include "runtime/Function.h"
#include "runtime/GenericDispatchTable.h"

namespace rir {
namespace pir {

class DeoptLess {
  public:
    static Function* dispatch(SEXP closure, rir::Code* c,
                              const DeoptContext& ctx);
};

typedef GenericDispatchTable<DeoptContext, Function, 3> DeoptlessDispatchTable;

} // namespace pir
} // namespace rir
