#ifndef RIR_INTERP_BUILTINS_H
#define RIR_INTERP_BUILTINS_H

#include "interp_incl.h"

namespace rir {

R_bcstack_t tryFastSpecialCall(const CallContext& call,
                               InterpreterInstance* ctx);
R_bcstack_t tryFastBuiltinCall(const CallContext& call,
                               InterpreterInstance* ctx);

} // namespace rir

#endif
