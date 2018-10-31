#include "runtime.h"
#include "api.h"
#include "interp.h"

#include <iomanip>

SEXP envSymbol;
SEXP callSymbol;
SEXP execName;
SEXP promExecName;
Context* globalContext_;
rir::Configurations* configurations;

/** Checks if given closure should be executed using RIR.

  If the given closure is RIR function, returns its Function object, otherwise
  returns nullptr.
 */
Function* isValidClosureSEXP(SEXP closure) {
    if (TYPEOF(closure) != CLOSXP) {
        return nullptr;
    }
    if (auto t = DispatchTable::check(BODY(closure))) {
        return t->first();
    }
    return nullptr;
}

void initializeRuntime() {
    envSymbol = Rf_install("environment");
    callSymbol = Rf_install(".Call");
    execName = Rf_mkString("rir_executeWrapper");
    R_PreserveObject(execName);
    promExecName = Rf_mkString("rir_executePromiseWrapper");
    R_PreserveObject(promExecName);
    // initialize the global context
    globalContext_ = context_create();
    registerExternalCode(rirEval_f, rir_compile, rirExpr);
    configurations = new rir::Configurations();
}

Context* globalContext() { return globalContext_; }
rir::Configurations* pirConfigurations() { return configurations; }
