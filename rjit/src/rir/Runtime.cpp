#include "Runtime.h"
#include "RIntlns.h"
#include "../Precious.h"

#include <iostream>

extern "C" SEXP R_MakeWeakRefC(SEXP key, SEXP val, R_CFinalizer_t fin,
                               Rboolean onexit);

namespace rjit {
namespace rir {

void BCProm::val(SEXP wrapper, SEXP aVal) {
    _val = aVal;
    // Ensures the value is reachable for the gc
    SET_ATTRIB(wrapper, CONS_NR(aVal, R_NilValue));
}

SEXP mkBCProm(Function* fun, fun_idx_t idx, SEXP env) {
    SEXP s = Rf_allocVector(BCCodeType, 1 + sizeof(BCProm));
    *getBCProm(s) = BCProm(fun, idx, env);

    return s;
}

SEXP mkBCCls(Function* fun, SEXP formals, num_args_t nargs, bool eager,
             SEXP env) {
    SEXP s = Rf_allocVector(BCCodeType, sizeof(BCClosure));
    *getBCCls(s) = BCClosure(fun, formals, nargs, eager, env);

    Precious::add(s);
    return s;
}
}
}
