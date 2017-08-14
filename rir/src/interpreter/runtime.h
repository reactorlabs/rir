#ifndef RIR_INTERPRETER_RUNTIME_H
#define RIR_INTERPRETER_RUNTIME_H

#include "../config.h"
#include "interp_data.h"
#include "interp_context.h"

// stuff from api the interpreter uses

C_OR_CPP Code* isValidPromiseSEXP(SEXP promise);
C_OR_CPP Function* isValidClosureSEXP(SEXP closure);
C_OR_CPP Function* isValidFunctionSEXP(SEXP wrapper);

/** Checks if given closure should be executed using RIR.

  If the given closure is RIR function, returns its Function object, otherwise returns nullptr.
 */

C_OR_CPP void printCode(Code* c);

C_OR_CPP void printFunction(Function* f);
C_OR_CPP void printFunctionFancy(SEXP f);

C_OR_CPP void initializeRuntime(CompilerCallback compiler,
                                OptimizerCallback optimizer);

/** Returns the global context for the interpreter - important to get access to
  the shared constant and source pools.

  TODO Even in multithreaded mode we probably want to have cp and src pools
  shared - it is not that we add stuff to them often.
 */
C_OR_CPP Context* globalContext();

#endif
