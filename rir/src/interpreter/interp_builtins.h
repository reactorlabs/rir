#ifndef RIR_INTERP_BUILTINS_H
#define RIR_INTERP_BUILTINS_H

#include "interp_data.h"

namespace rir {

SEXP tryFastSpecialCall(const CallContext& call, Context* ctx);
SEXP tryFastBuiltinCall(const CallContext& call, Context* ctx);
}

#endif
