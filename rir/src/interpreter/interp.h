#ifndef RIR_INTERPRETER_C_H
#define RIR_INTERPRETER_C_H

#include <R/r.h>

#undef length

#include "interp_context.h"
#include "interp_data.h"

#ifdef __cplusplus
extern "C" {
#endif

SEXP evalRirCode(Code* c, Context* ctx, SEXP env, unsigned numArgs);

SEXP rirExpr(SEXP f);

SEXP rirEval_f(SEXP f, SEXP env);

SEXP rirExpr(SEXP f);

#ifdef __cplusplus
}
#endif

#endif // RIR_INTERPRETER_C_H


