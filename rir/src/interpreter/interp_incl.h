#ifndef RIR_INTERPRETER_INCL_C_H
#define RIR_INTERPRETER_INCL_C_H

#include "R/r.h"
#include "ir/BC_inc.h"

// Indicates an argument is missing
#define MISSING_ARG_IDX ((unsigned)-1)
// Indicates an argument does not correspond to a valid CodeObject
#define DOTS_ARG_IDX ((unsigned)-2)
// Maximum valid entry for a CodeObject offset/idx entry
#define MAX_ARG_IDX ((unsigned)-3)

const static uint32_t NO_DEOPT_INFO = (uint32_t)-1;

namespace rir {

struct InterpreterInstance;
struct Code;
struct CallContext;
class Configurations;

bool isValidClosureSEXP(SEXP closure);

void initializeRuntime();

/** Returns the global context for the interpreter - important to get access to
  the shared constant and source pools.

  TODO Even in multithreaded mode we probably want to have cp and src pools
  shared - it is not that we add stuff to them often.
 */
InterpreterInstance* globalContext();
Configurations* pirConfigurations();

SEXP evalRirCodeExtCaller(Code* c, InterpreterInstance* ctx, SEXP* env);
SEXP evalRirCode(Code* c, InterpreterInstance* ctx, SEXP* env,
                 const CallContext* callContext);

SEXP rirExpr(SEXP f);

SEXP rirEval_f(SEXP f, SEXP env);
SEXP rirApplyClosure(SEXP, SEXP, SEXP, SEXP);

SEXP argsLazyCreation(void* rirDataWrapper);

SEXP createLegacyArgsListFromStackValues(const CallContext& call,
                                         bool eagerCallee,
                                         InterpreterInstance* ctx);

SEXP createEnvironment(const std::vector<SEXP>* args, const SEXP parent,
                       const Opcode* pc, InterpreterInstance* ctx,
                       R_bcstack_t* localsBase, SEXP stub);

SEXP materialize(void* rirDataWrapper);
} // namespace rir

#endif
