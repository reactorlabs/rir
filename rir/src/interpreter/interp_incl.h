#ifndef RIR_INTERPRETER_INCL_C_H
#define RIR_INTERPRETER_INCL_C_H

#include "R/r.h"
#include "ir/BC_inc.h"
#include "runtime/ArglistOrder.h"

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

SEXP evalRirCodeExtCaller(Code* c, InterpreterInstance* ctx, SEXP env);
SEXP evalRirCode(Code* c, InterpreterInstance* ctx, SEXP env,
                 const CallContext* callContext);

SEXP rirEval(SEXP f, SEXP env);
SEXP rirApplyClosure(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP rirForcePromise(SEXP);

SEXP createLegacyArglist(ArglistOrder::CallId id, size_t length,
                         const R_bcstack_t* stackArgs, SEXP* heapArgs,
                         const Immediate* names, SEXP ast,
                         ArglistOrder* reordering, bool eagerCallee,
                         bool recreateOriginalPromargs,
                         InterpreterInstance* ctx);

SEXP createEnvironment(std::vector<SEXP>* args, const SEXP parent,
                       const Opcode* pc, InterpreterInstance* ctx, SEXP stub);

SEXP rirDecompile(SEXP s);

void serializeRir(SEXP s, SEXP refTable, R_outpstream_t out);
SEXP deserializeRir(SEXP refTable, R_inpstream_t inp);
// Will serialize and deserialize the SEXP, returning a deep copy.
SEXP copyBySerial(SEXP x);

SEXP materialize(SEXP rirDataWrapper);

SEXP evaluatePromise(SEXP e, InterpreterInstance* ctx, Opcode* pc);
inline SEXP evaluatePromise(SEXP e, InterpreterInstance* ctx) {
    return evaluatePromise(e, ctx, nullptr);
}
inline SEXP evaluatePromise(SEXP e) {
    return evaluatePromise(e, globalContext());
}

} // namespace rir

#endif
