#include "safe_force.h"
#include "R/RList.h"
#include "R/Sexp.h"
#include "R/Symbols.h"
#include "R/r.h"

#include <assert.h>

namespace rir {

static SEXP safeEval(SEXP e, SEXP rho) {
    Match(e) {
        // Function application
        Case(LANGSXP) { return R_UnboundValue; }
        // Variable lookup
        Case(SYMSXP) { return R_UnboundValue; }
        Case(PROMSXP) {
            // TODO: Is this possible? Should it be assert(false)?
            return R_UnboundValue;
        }
        Case(BCODESXP) { return R_UnboundValue; }
        Case(EXTERNALSXP) { return R_UnboundValue; }
        // TODO : some code (eg. serialize.c:2154) puts closures into asts...
        //        not sure how we want to handle it...
        // Case(CLOSXP) {
        //     assert(false);
        // }

        // Constant
        Else({ return e; });
    }
    assert(false);
}

// Copied from gnuR
SEXP safeForcePromise(SEXP e) {
    if (PRVALUE(e) == R_UnboundValue) {
        SEXP val;
        if (PRSEEN(e)) {
            if (PRSEEN(e) == 1)
                Rf_errorcall(R_GlobalContext->call,
                             "promise already under evaluation: recursive "
                             "default argument reference or earlier problems?");
            else {
                /* set PRSEEN to 1 to avoid infinite recursion */
                SET_PRSEEN(e, 1);
                Rf_warningcall(R_GlobalContext->call,
                               "restarting interrupted promise evaluation");
            }
        }
        /* Mark the promise as under evaluation, don't push it onto the promise
         * stack because evaluation won't jump out */
        SET_PRSEEN(e, 1);
        val = safeEval(PRCODE(e), PRENV(e));
        SET_PRSEEN(e, 0);
        if (val != R_UnboundValue) {
            SET_PRVALUE(e, val);
            /* Also set the environment to R_NilValue to allow GC to
               reclaim the promise environment; this is also useful for
               fancy games with delayedAssign() */
            ENSURE_NAMEDMAX(val);
            SET_PRENV(e, R_NilValue);
        }
    }
    return PRVALUE(e);
}

} // namespace rir
