#ifndef RIR_INTERPRETER_C_H
#define RIR_INTERPRETER_C_H

#include <R/r.h>

#undef length

#include "call_support.h"
#include "interp_context.h"
#include "interp_data.h"

#ifdef __cplusplus
extern "C" {
#endif

SEXP evalRirCode(Code* c, Context* ctx, rir::EnvironmentProxy* ep);

SEXP rirExpr(SEXP f);

SEXP rirEval_f(SEXP f, SEXP env);

#ifdef __cplusplus
}
#endif

#endif // RIR_INTERPRETER_C_H


