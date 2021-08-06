#ifndef RIR_INTERP_BUILTINS_H
#define RIR_INTERP_BUILTINS_H

#include "interp_incl.h"

namespace rir {

SEXP tryFastSpecialCall(CallContext& call, InterpreterInstance* ctx);
SEXP tryFastBuiltinCall(CallContext& call, InterpreterInstance* ctx);
bool supportsFastBuiltinCall(SEXP blt, size_t nargs);

SEXP vapply(SEXP X, SEXP XX, SEXP fun, SEXP value, SEXP useNames, SEXP env);

} // namespace rir

#endif
