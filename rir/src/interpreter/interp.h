#ifndef RIR_INTERPRETER_C_H
#define RIR_INTERPRETER_C_H

#include <R/r.h>

#undef length

#include "interp_context.h"
#include "interp_data.h"

namespace rir {

struct CallContext;

SEXP evalRirCodeExtCaller(Code* c, Context* ctx, SEXP* env);
SEXP evalRirCode(Code* c, Context* ctx, SEXP* env,
                 const CallContext* callContext);

SEXP rirExpr(SEXP f);

SEXP rirEval_f(SEXP f, SEXP env);
SEXP rirApplyClosure(SEXP, SEXP, SEXP, SEXP);

SEXP argsLazyCreation(void* rirDataWrapper);

}

#endif // RIR_INTERPRETER_C_H
