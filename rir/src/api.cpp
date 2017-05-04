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

#include "analysis/analysis.h"
#include "analyzers/Signature.h"
#include "optimizers/cp.h"
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

REXPORT SEXP rir_compile(SEXP what, SEXP env) {

    // TODO make this nicer
    if (TYPEOF(what) == CLOSXP) {
        SEXP body = BODY(what);
        if (TYPEOF(body) == BCODESXP) {
            R_PreserveObject(body);
            body = VECTOR_ELT(CDR(body), 0);
        }

        if (TYPEOF(body) == EXTERNALSXP)
            Rf_error("closure already compiled");

        SEXP result = allocSExp(CLOSXP);
        PROTECT(result);
        auto res = Compiler::compileClosure(body, FORMALS(what), env);
        SET_FORMALS(result, res.formals);
        SET_CLOENV(result, CLOENV(what));
        SET_BODY(result, res.bc);
        Rf_copyMostAttrib(what, result);
        UNPROTECT(1);
        return result;
    } else {
        if (TYPEOF(what) == BCODESXP) {
            what = VECTOR_ELT(CDR(what), 0);
        }
        auto res = Compiler::compileExpression(what, env);
        return res.bc;
    }
}

REXPORT SEXP rir_markOptimize(SEXP what) {
    if (TYPEOF(what) != CLOSXP)
        return R_NilValue;
    SEXP b = BODY(what);
    isValidFunctionSEXP(b);
    Function* fun = (Function*)INTEGER(b);
    fun->markOpt = true;
    return R_NilValue;
}

REXPORT SEXP rir_eval(SEXP what, SEXP env) {
    ::Function * f = isValidFunctionObject(what);
    if (f == nullptr)
        f = isValidClosureSEXP(what);
    if (f == nullptr)
        Rf_error("Not rir compiled code");
    return evalRirCode(functionCode(f), globalContext(), env, 0);
}

REXPORT SEXP rir_body(SEXP cls) {
    ::Function * f = isValidClosureSEXP(cls);
    if (f == nullptr)
        Rf_error("Not a valid rir compiled function");
    return functionSEXP(f);
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
