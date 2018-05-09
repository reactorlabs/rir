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

    Function* f = TYPEOF(what) == CLOSXP ? isValidClosureSEXP(what)
                                         : isValidFunctionSEXP(what);

    if (f == nullptr)
        Rf_error("Not a rir compiled code");

    Rprintf("%p  [invoked %ux]\n", what, f->invocationCount);

    CodeEditor(what).print(LOGICAL(verbose)[0]);
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

REXPORT SEXP pir_compile(SEXP what, bool debug = false) {
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
    cmp.setVerbose(false);
    cmp.compileClosure(what);
    cmp.optimizeModule();

    if (debug)
        m->print();

    // compile back to rir
    auto table = DispatchTable::unpack(BODY(what));
    auto oldFun = table->first();
    pir::Pir2RirCompiler p2r;
    auto fun = p2r(m->get(oldFun));
    p(fun->container());

    // TODO: probably start from 0?
    // fun->invocationCount = oldFun->invocationCount;
    // TODO: are these still needed / used?
    fun->envLeaked = oldFun->envLeaked;
    fun->envChanged = oldFun->envChanged;
    // TODO: signatures need a rework
    fun->signature = oldFun->signature;

    table->put(1, fun);

    if (debug) {
        Rprintf("orig:\n");
        printFunction(oldFun);
        Rprintf("new:\n");
        printFunction(fun);
    }

    delete m;
    return R_NilValue;
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
