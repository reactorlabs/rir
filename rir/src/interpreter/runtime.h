#ifndef RIR_INTERPRETER_RUNTIME_H
#define RIR_INTERPRETER_RUNTIME_H

#include "../config.h"
#include "interp_context.h"
#include "interp_data.h"
#include "utils/configurations.h"

// stuff from api the interpreter uses

/** Checks if given closure should be executed using RIR.

  If the given closure is RIR function, returns its Function object, otherwise returns nullptr.
 */
typedef rir::Function Function;
typedef rir::DispatchTable DispatchTable;

bool isValidClosureSEXP(SEXP closure);

void initializeRuntime();

/** Returns the global context for the interpreter - important to get access to
  the shared constant and source pools.

  TODO Even in multithreaded mode we probably want to have cp and src pools
  shared - it is not that we add stuff to them often.
 */
Context* globalContext();
rir::Configurations* pirConfigurations();

#endif
