#ifndef RIR_INTERP_BUILTINS_H
#define RIR_INTERP_BUILTINS_H

#include "interp_incl.h"

namespace rir {

SEXP tryFastSpecialCall(CallContext& call, InterpreterInstance* ctx);
SEXP tryFastBuiltinCall(CallContext& call, InterpreterInstance* ctx);
bool supportsFastBuiltinCall(SEXP blt, size_t nargs);

} // namespace rir

#endif
