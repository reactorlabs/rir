#ifndef RIR_INTERP_BUILTINS_H
#define RIR_INTERP_BUILTINS_H

#include "call_context.h"
#include "instance.h"

namespace rir {

SEXP tryFastSpecialCall(const CallContext& call, InterpreterInstance* ctx);
SEXP tryFastBuiltinCall(const CallContext& call, InterpreterInstance* ctx);

} // namespace rir

#endif
