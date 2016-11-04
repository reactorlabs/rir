#ifndef RIR_INTERPRETER_RUNTIME_H
#define RIR_INTERPRETER_RUNTIME_H

#include "../config.h"
#include "interp_data.h"
#include "interp_context.h"

// stuff from api the interpreter uses

C_OR_CPP Code * isValidPromiseSEXP(SEXP promise);
C_OR_CPP Function * isValidClosureSEXP(SEXP closure);
C_OR_CPP Function * isValidFunctionSEXP(SEXP wrapper);
C_OR_CPP SEXP rir_createWrapperPromise(Code * code);


// functions from gnu-r

#if RIR_AS_PACKAGE == 1

C_OR_CPP int ddVal(SEXP symbol);
C_OR_CPP SEXP Rf_ddfindVar(SEXP symbol, SEXP rho);
C_OR_CPP SEXP mkPROMISE(SEXP expr, SEXP rho);
C_OR_CPP SEXP forcePromise(SEXP promise);

#else

C_OR_CPP int ddVal(SEXP symbol);
C_OR_CPP SEXP Rf_ddfindVar(SEXP symbol, SEXP rho);

// TODO we should change gnu-R and make mkPROMISE exposed, this is a hack so that I do not change too many things at once
C_OR_CPP SEXP hook_mkPROMISE(SEXP, SEXP);
#define mkPROMISE hook_mkPROMISE

// TODO the same as mkPROMISE
C_OR_CPP SEXP hook_forcePromise(SEXP promise);
#define forcePromise hook_forcePromise

typedef int (*callback_isValidFunction)(SEXP);
typedef SEXP (*callback_rirEval_f)(SEXP, SEXP);
typedef SEXP (*callback_rirExpr)(SEXP);

C_OR_CPP void initializeCallbacks(callback_isValidFunction, callback_isValidFunction, callback_rirEval_f, callback_rirExpr);

C_OR_CPP void initClosureContext(void*, SEXP, SEXP, SEXP, SEXP, SEXP);
C_OR_CPP void endClosureContext(void*, SEXP);


#endif


// rir runtime functions -------------------------------------------------------

C_OR_CPP Function * isValidClosureSEXP(SEXP wrapper);

/** Checks if given closure should be executed using RIR.

  If the given closure is RIR function, returns its Function object, otherwise returns nullptr.
 */
C_OR_CPP Function * isValidClosureSEXP(SEXP closure);

C_OR_CPP Code * isValidPromiseSEXP(SEXP promise);

C_OR_CPP void printCode(Code* c);

C_OR_CPP void printFunction(Function* f);
C_OR_CPP void printFunctionFancy(SEXP f);

C_OR_CPP SEXP rir_createWrapperAst(SEXP rirBytecode);

C_OR_CPP SEXP rir_createWrapperPromise(Code * code);

C_OR_CPP void initializeRuntime(CompilerCallback compiler,
                                OptimizerCallback optimizer);

/** Returns the global context for the interpreter - important to get access to
  the shared constant and source pools.

  TODO Even in multithreaded mode we probably want to have cp and src pools
  shared - it is not that we add stuff to them often.
 */
C_OR_CPP Context* globalContext();


#endif
