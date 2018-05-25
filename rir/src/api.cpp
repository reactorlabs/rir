/** Enables the use of R internals for us so that we can manipulate R structures
 * in low level.
 */

#include <cassert>

#include "api.h"

#include "interpreter/interp.h"
#include "interpreter/interp_context.h"
#include "ir/Compiler.h"

#include "utils/Printer.h"

#include "ir/Optimizer.h"

using namespace rir;

REXPORT SEXP rir_disassemble(SEXP what, SEXP verbose) {
    if (!what || TYPEOF(what) != CLOSXP)
        Rf_error("Not a rir compiled code");
    DispatchTable* t = isValidDispatchTableObject(BODY(what));

    if (!t)
        Rf_error("Not a rir compiled code");

    Rprintf("* closure %p (vtable %p, env %p)\n", what, t, CLOENV(what));
    for (size_t entry = 0; entry < t->capacity(); ++entry) {
        if (!t->slot(entry))
            continue;
        Function* f = t->at(entry);
        Rprintf("= vtable slot <%d> (%p, invoked %u) =\n", entry, f,
                f->invocationCount);
        CodeEditor(f).print(LOGICAL(verbose)[0]);
    }

    return R_NilValue;
}

REXPORT SEXP rir_compile(SEXP what, SEXP env = NULL) {
    SEXP result;
    if (TYPEOF(what) == CLOSXP) {
        SEXP body = BODY(what);
        if (TYPEOF(body) == EXTERNALSXP)
            Rf_error("closure already compiled");
        result = Compiler::compileClosure(what);
        Rf_copyMostAttrib(what, result);
    } else {
        if (TYPEOF(what) == BCODESXP)
            what = VECTOR_ELT(CDR(what), 0);
        result = Compiler::compileExpression(what);
    }
    Optimizer::tryOptimize(result);
    return result;
}

REXPORT SEXP rir_eval(SEXP what, SEXP env) {
    ::Function* f = isValidFunctionObject(what);
    if (f == nullptr)
        f = isValidClosureSEXP(what);
    if (f == nullptr)
        Rf_error("Not rir compiled code");
    EnvironmentProxy ep(env);
    return evalRirCode(f->body(), globalContext(), &ep);
}

#include "compiler/pir_tests.h"

REXPORT SEXP pir_optimize(SEXP what, SEXP verbose) {
    bool debug = false;
    if (verbose && TYPEOF(verbose) == LGLSXP && Rf_length(verbose) > 0 &&
        LOGICAL(verbose)[0])
        debug = true;

    if (!Optimizer::tryOptimize(what, debug))
        Rf_error("Couldn't optimize in PIR.");

    return what;
}

REXPORT SEXP pir_tests() {
    PirTests::run();
    return R_NilValue;
}

// startup ---------------------------------------------------------------------

bool startup() {
    initializeRuntime(rir_compile, Optimizer::reoptimizeFunction);
    return true;
}

bool startup_ok = startup();
