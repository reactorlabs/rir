#ifndef RIR_INTERPRETER_C_H
#define RIR_INTERPRETER_C_H

#include <R/r.h>

#undef length

#include "interp_context.h"
#include "interp_data.h"

#ifdef __cplusplus
extern "C" {
#endif

struct CallContext;
typedef rir::Code Code;

SEXP evalRirCodeExtCaller(Code* c, Context* ctx, SEXP* env);
SEXP evalRirCode(Code* c, Context* ctx, SEXP* env,
                 const CallContext* callContext);

SEXP rirExpr(SEXP f);

SEXP rirEval_f(SEXP f, SEXP env);

SEXP argsLazyCreation(void* rirDataWrapper);

#ifdef __cplusplus
}
#endif

#endif // RIR_INTERPRETER_C_H
