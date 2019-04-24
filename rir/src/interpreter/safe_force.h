#ifndef RIR_SAFE_FORCE_H
#define RIR_SAFE_FORCE_H

#include "../common.h"
#include <R/r.h>

namespace rir {

// Will try to evaluate the SEXP if it definitely doesn't cause side effects,
// rho can be nullptr if the environment is unknown
SEXP safeEval(SEXP e, SEXP rho, bool strong);
// Will try to evaluate the promise if it definitely doesn't cause side effects
SEXP safeForcePromise(SEXP e, bool strong);

} // namespace rir

#endif
