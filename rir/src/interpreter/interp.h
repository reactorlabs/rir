#ifndef RIR_INTERPRETER_C_H
#define RIR_INTERPRETER_C_H

#include <R/r.h>

#undef length

#include "interp_context.h"
#include "interp_data.h"

// If 1, when a function that has not yet been compiled by rir is to be called
// in the interpreter, it will be compiled first.
// Set to 0 if rir should handle the execution to GNU-R.
#define COMPILE_ON_DEMAND 1

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


