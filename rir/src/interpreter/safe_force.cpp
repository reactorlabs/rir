#include "safe_force.h"
#include "R/RList.h"
#include "R/Sexp.h"
#include "R/Symbols.h"
#include "R/r.h"

#include <assert.h>

namespace rir {

static RIR_INLINE SEXP safeEval(SEXP e, SEXP rho) {
    SEXPTYPE t = TYPEOF(e);
    if (t == LANGSXP || t == SYMSXP || t == PROMSXP || t == BCODESXP ||
        t == EXTERNALSXP) {
        return R_UnboundValue;
    } else {
        // Constant
        return e;
    }
}

SEXP safeForcePromise(SEXP e) {
    if (PRVALUE(e) != R_UnboundValue) {
        return PRVALUE(e);
    } else {
        return safeEval(PRCODE(e), PRENV(e));
    }
}

} // namespace rir
