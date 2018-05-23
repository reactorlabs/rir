/** Enables the use of R internals for us so that we can manipulate R structures
 * in low level.
 */

#include <cassert>

#include "api.h"

#include "interpreter/interp.h"
#include "interpreter/interp_context.h"
#include "ir/BC.h"
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

    // TODO make this nicer
    if (TYPEOF(what) == CLOSXP) {
        SEXP body = BODY(what);
        if (TYPEOF(body) == EXTERNALSXP)
            Rf_error("closure already compiled");

        SEXP result = Compiler::compileClosure(what);
        Rf_copyMostAttrib(what, result);
        return result;
    } else {
        if (TYPEOF(what) == BCODESXP) {
            what = VECTOR_ELT(CDR(what), 0);
        }
        SEXP result = Compiler::compileExpression(what);
        return result;
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
    DispatchTable* dt = DispatchTable::unpack(b);
    Function* fun = dt->first();
    fun->markOpt = true;
    return R_NilValue;
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

REXPORT SEXP rir_body(SEXP cls) {
    ::Function* f = isValidClosureSEXP(cls);
    if (f == nullptr)
        Rf_error("Not a valid rir compiled function");
    return f->container();
}

#include "compiler/pir_tests.h"
#include "compiler/translations/pir_2_rir.h"
#include "compiler/translations/rir_2_pir/rir_2_pir.h"

REXPORT SEXP pir_compile(SEXP what, SEXP verbose) {
    bool debug = false;
    if (verbose && TYPEOF(verbose) == LGLSXP && Rf_length(verbose) > 0 &&
        LOGICAL(verbose)[0])
        debug = true;

    if (!isValidClosureSEXP(what))
        Rf_error("not a compiled closure");
    assert(DispatchTable::unpack(BODY(what))->capacity() == 2 &&
           "fix, support for more than 2 slots needed...");
    if (DispatchTable::unpack(BODY(what))->slot(1) != nullptr)
        Rf_error("closure already compiled to pir");

    Protect p(what);

    // compile to pir
    pir::Module* m = new pir::Module;
    pir::Rir2PirCompiler cmp(m);
    cmp.setVerbose(debug);
    auto c = cmp.compileClosure(what);
    cmp.optimizeModule();

    if (debug)
        m->print();

    // compile back to rir
    pir::Pir2RirCompiler p2r;
    p2r.verbose = debug;
    p2r.compile(c, what);

    delete m;
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
