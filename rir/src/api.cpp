/** Enables the use of R internals for us so that we can manipulate R structures
 * in low level.
 */

#include <cassert>

#include "api.h"

#include "ir/Compiler.h"
#include "interpreter/interp_context.h"
#include "interpreter/interp.h"
#include "ir/BC.h"

#include "utils/FunctionHandle.h"

#include "analysis/Signature.h"
#include "analysis_framework/analysis.h"
#include "optimization/cp.h"
#include "utils/Printer.h"

#include "ir/Optimizer.h"

using namespace rir;

REXPORT SEXP rir_disassemble(SEXP what) {

    Rprintf("%p\n", what);
    ::Function * f = TYPEOF(what) == CLOSXP ? isValidClosureSEXP(what) : isValidFunctionSEXP(what);

    if (f == nullptr)
        Rf_error("Not a rir compiled code");

    CodeEditor(what).print();
    return R_NilValue;
}

REXPORT SEXP rir_compile(SEXP what) {

    // TODO make this nicer
    if (TYPEOF(what) == CLOSXP) {
        SEXP body = BODY(what);
        if (TYPEOF(body) == BCODESXP) {
            R_PreserveObject(body);
            body = VECTOR_ELT(CDR(body), 0);
        }

        if (TYPEOF(body) == EXTERNALSXP)
            Rf_error("closure already compiled");

        SEXP result = Compiler::compileClosure(body, FORMALS(what));
        SET_CLOENV(result, CLOENV(what));
        Rf_copyMostAttrib(what, result);
        return result;
    } else {
        if (TYPEOF(what) == BCODESXP) {
            what = VECTOR_ELT(CDR(what), 0);
        }
        auto res = Compiler::compileExpression(what);
        return res.bc;
    }
}

REXPORT SEXP rir_markOptimize(SEXP what) {
    // TODO(mhyee): This is to mark a function for optimization.
    // However, now that we have vtables, does this still make sense? Maybe it
    // might be better to mark a specific version for optimization.
    // For now, we just mark the first version in the vtable.
    if (TYPEOF(what) != CLOSXP)
        return R_NilValue;
    SEXP b = BODY(what);
    assert(isValidDispatchTableSEXP(b) && "Expected a DispatchTable");
    DispatchTable* dt = sexp2dispatchTable(b);
    Function* fun = sexp2function(dt->entry[0]);
    fun->markOpt = true;
    return R_NilValue;
}

REXPORT SEXP rir_eval(SEXP what, SEXP env) {
    ::Function * f = isValidFunctionObject(what);
    if (f == nullptr)
        f = isValidClosureSEXP(what);
    if (f == nullptr)
        Rf_error("Not rir compiled code");
    return evalRirCode(bodyCode(f), globalContext(), env, 0);
}

REXPORT SEXP rir_body(SEXP cls) {
    ::Function * f = isValidClosureSEXP(cls);
    if (f == nullptr)
        Rf_error("Not a valid rir compiled function");
    return function2store(f);
}

REXPORT SEXP rir_analysis_signature(SEXP what) {
    if (TYPEOF(what) != CLOSXP)
        return R_NilValue;
    CodeEditor ce(what);
    SignatureAnalysis sa;
    sa.analyze(ce);
    return sa.finalState().exportToR();
}

// startup ---------------------------------------------------------------------

bool startup() {
    initializeRuntime(rir_compile, Optimizer::reoptimizeFunction);
    return true;
}

bool startup_ok = startup();
