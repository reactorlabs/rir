#ifndef RIR_SAFE_FORCE_H
#define RIR_SAFE_FORCE_H

#include "builtins.h"
#include "call_context.h"
#include "instance.h"

#include "interp_incl.h"

#include <R/r.h>

namespace rir {

// Will try to evaluate the promise if it definitely doesn't cause side effects
SEXP safeForcePromise(SEXP e);

} // namespace rir

#endif
