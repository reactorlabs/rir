#ifndef SAFE_BUILTINS_LIST_H
#define SAFE_BUILTINS_LIST_H

#include "R/r.h"

namespace rir {
namespace pir {

class CallSafeBuiltinsList {
  public:
    static bool contains(SEXP builtin);
};

} // namespace pir
} // namespace rir

#endif
