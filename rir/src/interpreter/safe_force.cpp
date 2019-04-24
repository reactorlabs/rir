#include "safe_force.h"
#include "../compiler/parameter.h"
#include "R/RList.h"
#include "R/Sexp.h"
#include "R/Symbols.h"
#include "R/r.h"

#include <assert.h>

namespace rir {

// #define DEBUG_SAFE_EVAL

SEXP safeEval(SEXP e, SEXP rho, bool strong) {
    assert((!strong || pir::Parameter::RIR_STRONG_SAFE_FORCE) &&
           "currently never strong safe forces unless explicitly enabled");
    if (e == R_UnboundValue) {
#ifdef DEBUG_SAFE_EVAL
        std::cout << "safe eval failed on unbound\n";
#endif
        return R_UnboundValue;
    } else if (e == R_MissingArg) {
#ifdef DEBUG_SAFE_EVAL
        std::cout << "safe eval missing\n";
#endif
        return e;
    }
    SEXPTYPE t = TYPEOF(e);
    if (t == LANGSXP || t == BCODESXP || t == EXTERNALSXP) {
#ifdef DEBUG_SAFE_EVAL
        std::cout << "safe eval failed: ";
        Rf_PrintValue(e);
#endif
        return R_UnboundValue;
    } else if (t == PROMSXP) {
        if (!strong)
            return R_UnboundValue;
#ifdef DEBUG_SAFE_EVAL
        std::cout << "safe eval promise -> ";
#endif
        return safeForcePromise(e, strong);
    } else if (t == SYMSXP) {
        if (!strong || rho == nullptr)
            return R_UnboundValue;
#ifdef DEBUG_SAFE_EVAL
        std::cout << "safe eval lookup " << CHAR(PRINTNAME(e));
#endif
        SEXP f = Rf_findVar(e, rho);
        if (f == R_UnboundValue) {
#ifdef DEBUG_SAFE_EVAL
            std::cout << " failed\n";
#endif
            return R_UnboundValue;
        }
#ifdef DEBUG_SAFE_EVAL
        std::cout << " -> ";
#endif
        return safeEval(f, rho, strong);
    } else {
        // Constant
#ifdef DEBUG_SAFE_EVAL
        std::cout << "safe eval constant: ";
        Rf_PrintValue(e);
#endif
        return e;
    }
}

SEXP safeForcePromise(SEXP e, bool strong) {
    if (PRVALUE(e) == R_UnboundValue) {
        SEXP val = safeEval(PRCODE(e), PRENV(e), strong);
        if (val != R_UnboundValue) {
            SET_PRVALUE(e, val);
            ENSURE_NAMEDMAX(val);
            SET_PRENV(e, R_NilValue);
        }
        return val;
    } else {
#ifdef DEBUG_SAFE_EVAL
        std::cout << "promise known: ";
        Rf_PrintValue(PRVALUE(e));
#endif
        return PRVALUE(e);
    }
}

} // namespace rir
