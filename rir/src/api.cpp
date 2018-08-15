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
        f->body()->disassemble();
        for (auto c : *f) {
            if (c != f->body()) {
                Rprintf("\n [Prom %x]\n", (uintptr_t)c - (uintptr_t)f);
                c->disassemble();
            }
        }
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

REXPORT SEXP pir_debugFlags(
#define V(n) SEXP n,
    LIST_OF_PIR_DEBUGGING_FLAGS(V)
#undef V
        SEXP IHaveTooManyCommasDummy) {
    pir::DebugOptions opts;

#define V(n)                                                                   \
    if (Rf_asLogical(n))                                                       \
        opts.set(pir::DebugFlag::n);
    LIST_OF_PIR_DEBUGGING_FLAGS(V)
#undef V

    SEXP res = Rf_allocVector(INTSXP, 1);
    INTEGER(res)[0] = (int)opts.to_ulong();
    return res;
}

static pir::DebugOptions PirDebug(
    getenv("PIR_VERBOSE") ? std::stoul(getenv("PIR_VERBOSE"), nullptr, 0) : 0);

REXPORT SEXP pir_setDebugFlags(SEXP debugFlags) {
    if (TYPEOF(debugFlags) != INTSXP || Rf_length(debugFlags) < 1)
        Rf_error(
            "pir_setDebugFlags expects an integer vector as second parameter");
    PirDebug = pir::DebugOptions(INTEGER(debugFlags)[0]);
    return R_NilValue;
}

SEXP pirCompile(SEXP what, pir::DebugOptions debug) {
    debug = debug | PirDebug;

    if (!isValidClosureSEXP(what))
        Rf_error("not a compiled closure");
    assert(DispatchTable::unpack(BODY(what))->capacity() == 2 &&
           "fix, support for more than 2 slots needed...");
    if (DispatchTable::unpack(BODY(what))->available(1))
        return what;

    Protect p(what);

    // compile to pir
    pir::Module* m = new pir::Module;
    pir::Rir2PirCompiler cmp(m, debug);
    cmp.compileClosure(what,
                       [&](pir::Closure* c) {
                           cmp.optimizeModule();

                           // compile back to rir
                           pir::Pir2RirCompiler p2r(debug);
                           p2r.compile(c, what);
                       },
                       [&]() {
                           if (debug.includes(pir::DebugFlag::ShowWarnings))
                               std::cerr << "Compilation failed\n";
                       });

    delete m;
    return what;
}

REXPORT SEXP pir_compile(SEXP what, SEXP debugFlags) {
    if (TYPEOF(debugFlags) != INTSXP || Rf_length(debugFlags) < 1)
        Rf_error("pir_compile expects an integer vector as second parameter");
    return pirCompile(what, pir::DebugOptions(INTEGER(debugFlags)[0]));
}

REXPORT SEXP pir_tests() {
    PirTests::run();
    return R_NilValue;
}

// startup ---------------------------------------------------------------------

SEXP pirOpt(SEXP fun) { return pirCompile(fun, PirDebug); }

bool startup() {
    auto pir = getenv("PIR_ENABLE");
    if (pir && std::string(pir).compare("off") == 0) {
        initializeRuntime(rir_compile, [](SEXP f) { return f; });
    } else if (pir && std::string(pir).compare("force") == 0) {
        initializeRuntime(
            [](SEXP f, SEXP env) { return pirOpt(rir_compile(f, env)); },
            [](SEXP f) { return f; });
    } else if (pir && std::string(pir).compare("force_dryrun") == 0) {
        initializeRuntime(
            [](SEXP f, SEXP env) {
                return pirCompile(rir_compile(f, env),
                                  PirDebug | pir::DebugFlag::DryRun);
            },
            [](SEXP f) { return f; });
    } else {
        // default on
        initializeRuntime(rir_compile, pirOpt);
    }
    return true;
}

bool startup_ok = startup();
