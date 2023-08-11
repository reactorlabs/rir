#include "api.h"
#include "interp.h"
#include "profiler.h"
#include "serializeHash/serialize/serializeR.h"
#include "serializeHash/serialize/native/SerialRepr.h"

#include "compilerClientServer/CompilerClient.h"

namespace rir {

InterpreterInstance* globalContext_;

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
    // initialize the global context
    globalContext_ = new InterpreterInstance;
    context_init();
    registerExternalCode(rirEval, rirApplyClosure, rirForcePromise, rirCompile,
                         rirDecompile, rirPrint, rirDeserializeHook, rirSerializeHook,
                         materialize);
    pir::SerialRepr::initGlobals();
    RuntimeProfiler::initProfiler();
    CompilerClient::tryInit();
}

InterpreterInstance* globalContext() { return globalContext_; }

} // namespace rir
