#include "runtime.h"
#include "api.h"
#include "interp.h"

#include <iomanip>

namespace rir {

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
bool isValidClosureSEXP(SEXP closure) {
    if (TYPEOF(closure) != CLOSXP) {
        return false;
    }
    if (DispatchTable::check(BODY(closure))) {
        return true;
    }
    return false;
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
    registerExternalCode(rirEval_f, rirApplyClosure, rir_compile, rirExpr,
                         argsLazyCreation);
    configurations = new rir::Configurations();
}

Context* globalContext() { return globalContext_; }
Configurations* pirConfigurations() { return configurations; }
}
