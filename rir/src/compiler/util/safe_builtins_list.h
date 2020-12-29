#ifndef SAFE_BUILTINS_LIST_H
#define SAFE_BUILTINS_LIST_H

#include "R/r.h"

namespace rir {
namespace pir {

class SafeBuiltinsList {
  public:
    static bool always(SEXP builtin);
    static bool idempotent(SEXP builtin);
    static bool nonObject(SEXP builtin);
    static bool nonObjectIdempotent(SEXP builtin);
    static bool always(int builtin);
    static bool idempotent(int builtin);
    static bool nonObject(int builtin);
    static bool nonObjectIdempotent(int builtin);
    static bool forInline(int builtin);
    static bool forInlineByName(SEXP name);
    static bool assumeStableInBaseEnv(SEXP name);
};

} // namespace pir
} // namespace rir

#endif
