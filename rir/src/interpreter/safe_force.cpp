#include "safe_force.h"
#include "R/RList.h"
#include "R/Sexp.h"
#include "R/Symbols.h"
#include "R/r.h"

#include <assert.h>

namespace rir {

SEXP safeEval(SEXP e, SEXP rho) {
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
    if (PRVALUE(e) == R_UnboundValue) {
        SEXP val = safeEval(PRCODE(e), PRENV(e));
        if (val != R_UnboundValue) {
            SET_PRVALUE(e, val);
            ENSURE_NAMEDMAX(val);
            SET_PRENV(e, R_NilValue);
        }
        return val;
    } else {
        return PRVALUE(e);
    }
}

} // namespace rir
