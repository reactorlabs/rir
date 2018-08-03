/** Enables the use of R internals for us so that we can manipulate R structures
 * in low level.
 */

#include <cassert>

#include "api.h"

#include "compiler/pir_tests.h"
#include "compiler/translations/pir_2_rir.h"
#include "compiler/translations/rir_2_pir/rir_2_pir.h"
#include "interpreter/interp.h"
#include "interpreter/interp_context.h"
#include "ir/BC.h"
#include "ir/Compiler.h"

#include "utils/Printer.h"

#include <memory>

using namespace rir;

REXPORT SEXP rir_disassemble(SEXP what, SEXP verbose) {
    if (!what || TYPEOF(what) != CLOSXP)
        Rf_error("Not a rir compiled code");
    DispatchTable* t = isValidDispatchTableObject(BODY(what));

    if (!t)
        Rf_error("Not a rir compiled code");

    Rprintf("* closure %p (vtable %p, env %p)\n", what, t, CLOENV(what));
    for (size_t entry = 0; entry < t->capacity(); ++entry) {
        if (!t->available(entry))
            continue;
        Function* f = t->at(entry);
        Rprintf("= vtable slot <%d> (%p, invoked %u) =\n", entry, f,
                f->invocationCount);
        CodeEditor(f).print(LOGICAL(verbose)[0]);
    }

    return R_NilValue;
}

REXPORT SEXP rir_compile(SEXP what, SEXP env) {
    // TODO make this nicer
    if (TYPEOF(what) == CLOSXP) {
        SEXP body = BODY(what);
        if (TYPEOF(body) == EXTERNALSXP)
            return what;

        SEXP result = Compiler::compileClosure(what);
        PROTECT(result);

        Rf_copyMostAttrib(what, result);

        UNPROTECT(1);
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
    SEXP lenv;
    return evalRirCodeExtCaller(f->body(), globalContext(), &lenv);
}

REXPORT SEXP rir_body(SEXP cls) {
    ::Function* f = isValidClosureSEXP(cls);
    if (f == nullptr)
        Rf_error("Not a valid rir compiled function");
    return f->container();
}

REXPORT SEXP pir_compile(SEXP what, unsigned int verbose, SEXP dryRun_) {
    bool dryRun = false;
    if (dryRun_ && TYPEOF(dryRun_) == LGLSXP && Rf_length(dryRun_) > 0 &&
        LOGICAL(dryRun_)[0])
        dryRun = true;

    if (!isValidClosureSEXP(what))
        Rf_error("not a compiled closure");
    assert(DispatchTable::unpack(BODY(what))->capacity() == 2 &&
           "fix, support for more than 2 slots needed...");
    if (DispatchTable::unpack(BODY(what))->available(1))
        return what;

    Protect p(what);

    // compile to pir
    pir::Module* m = new pir::Module;
    pir::Rir2PirCompiler cmp(m);
    cmp.setVerbose(verbose);
    cmp.compileClosure(what,
                       [&](pir::Closure* c) {
                           cmp.optimizeModule();

                           // compile back to rir
                           pir::Pir2RirCompiler p2r;
                           p2r.verbose = verbose;
                           p2r.dryRun = dryRun;
                           p2r.compile(c, what);
                       },
                       [&]() {
                           if (verbose > 0)
                               std::cerr << "Compilation failed\n";
                       });

    delete m;
    return what;
}

REXPORT SEXP pir_tests() {
    PirTests::run();
    return R_NilValue;
}

// startup ---------------------------------------------------------------------

uint pir_verbose = (getenv("PIR_VERBOSE")) ? 
     std::stoul(getenv("PIR_VERBOSE"), nullptr, 0) : 0;

SEXP pirOpt(SEXP fun) { return pir_compile(fun, pir_verbose, R_FalseValue); }

bool startup() {
    auto pir = getenv("PIR_ENABLE");
    if (pir && std::string(pir).compare("on") == 0) {
        initializeRuntime(rir_compile, pirOpt);
    } else if (pir && std::string(pir).compare("force") == 0) {
        initializeRuntime(
            [](SEXP f, SEXP env) { return pirOpt(rir_compile(f, env)); },
            [](SEXP f) { return f; });
    } else if (pir && std::string(pir).compare("force_dryrun") == 0) {
        initializeRuntime(
            [](SEXP f, SEXP env) {
                return pir_compile(rir_compile(f, env), pir_verbose,
                                   R_TrueValue);
            },
            [](SEXP f) { return f; });
    } else {
        initializeRuntime(rir_compile, [](SEXP f) { return f; });
    }
    return true;
}

bool startup_ok = startup();
