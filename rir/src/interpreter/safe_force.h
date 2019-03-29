#ifndef RIR_SAFE_FORCE_H
#define RIR_SAFE_FORCE_H

#include <R/r.h>

namespace rir {

// Will try to evaluate the promise if it definitely doesn't cause side effects
SEXP safeForcePromise(SEXP e);

} // namespace rir

#endif
