#ifndef RIR_INTERPRETER_C_H
#define RIR_INTERPRETER_C_H

#include "R/r.h"
#include "call_context.h"
#include "instance.h"
#include "ir/BC_inc.h"

#include <R/r.h>

#undef length

#if defined(__GNUC__) && (!defined(NO_THREADED_CODE))
#define THREADED_CODE
#endif

const static uint32_t NO_DEOPT_INFO = (uint32_t)-1;

namespace rir {

static unsigned PIR_WARMUP =
    getenv("PIR_WARMUP") ? atoi(getenv("PIR_WARMUP")) : 3;

struct InterpreterInstance;
struct Code;
struct CallContext;
class Configurations;

void initializeRuntime();

/** Returns the global context for the interpreter - important to get access to
  the shared constant and source pools.

  TODO Even in multithreaded mode we probably want to have cp and src pools
  shared - it is not that we add stuff to them often.
 */
Configurations* pirConfigurations();

SEXP evalRirCodeExtCaller(Code* c, InterpreterInstance* ctx, SEXP env);
SEXP evalRirCode(Code* c, InterpreterInstance* ctx, SEXP env,
                 const CallContext* callContext);
SEXP evalRirCode(Code* c, InterpreterInstance* ctx, SEXP env,
                 const CallContext* callContext, Opcode* op,
                 R_bcstack_t* stack = nullptr);

SEXP rirExpr(SEXP f);

SEXP rirEval_f(SEXP f, SEXP env);
SEXP rirApplyClosure(SEXP, SEXP, SEXP, SEXP);

SEXP argsLazyCreation(void* rirDataWrapper);

SEXP createLegacyArgsListFromStackValues(const CallContext& call,
                                         bool eagerCallee,
                                         InterpreterInstance* ctx);
SEXP createEnvironment(std::vector<SEXP>* args, const SEXP parent,
                       const Opcode* pc, InterpreterInstance* ctx,
                       R_bcstack_t* localsBase, SEXP stub);

SEXP materialize(void* rirDataWrapper);
SEXP* keepAliveSEXPs(void* rirDataWrapper);

inline RCNTXT* getFunctionContext(size_t pos = 0,
                                  RCNTXT* cptr = R_GlobalContext) {
    while (cptr->nextcontext != NULL) {
        if (cptr->callflag & CTXT_FUNCTION) {
            if (pos == 0)
                return cptr;
            pos--;
        }
        cptr = cptr->nextcontext;
    }
    assert(false);
    return nullptr;
}

inline RCNTXT* findFunctionContextFor(SEXP e) {
    auto cptr = R_GlobalContext;
    while (cptr->nextcontext != NULL) {
        if (cptr->callflag & CTXT_FUNCTION && cptr->cloenv == e) {
            return cptr;
        }
        cptr = cptr->nextcontext;
    }
    return nullptr;
}
} // namespace rir

#endif // RIR_INTERPRETER_C_H
