/** Enables the use of R internals for us so that we can manipulate R structures
 * in low level.
 */

#include <cassert>

#include "api.h"

#include "compiler/pir_tests.h"
#include "compiler/translations/pir_2_rir.h"
#include "compiler/translations/rir_2_pir/rir_2_pir.h"
#include "interpreter/interp_incl.h"
#include "ir/BC.h"
#include "ir/Compiler.h"

#include <memory>
#include <string>

using namespace rir;

REXPORT SEXP rir_disassemble(SEXP what, SEXP verbose) {
    if (!what || TYPEOF(what) != CLOSXP)
        Rf_error("Not a rir compiled code");
    DispatchTable* t = DispatchTable::check(BODY(what));

    if (!t)
        Rf_error("Not a rir compiled code");

    std::cout << "* closure " << what << " (vtable " << t << ", env "
              << CLOENV(what) << ")\n";
    for (size_t entry = 0; entry < t->size(); ++entry) {
        Function* f = t->get(entry);
        std::cout << "= vtable slot <" << entry << "> (" << f << ", invoked "
                  << f->invocationCount() << ") =\n";
        std::cout << "# ";
        f->signature().print(std::cout);
        std::cout << "\n";
        f->disassemble(std::cout);
    }

    return R_NilValue;
}

REXPORT SEXP rir_compile(SEXP what, SEXP env) {
    if (TYPEOF(what) == CLOSXP) {
        SEXP body = BODY(what);
        if (TYPEOF(body) == EXTERNALSXP)
            return what;

        // Change the input closure inplace
        Compiler::compileClosure(what);

        return what;
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
    Function* fun = dt->baseline();
    fun->markOpt = true;
    return R_NilValue;
}

REXPORT SEXP rir_eval(SEXP what, SEXP env) {
    SEXP lenv;
    if (Function* f = Function::check(what))
        return evalRirCodeExtCaller(f->body(), globalContext(), &lenv);

    if (isValidClosureSEXP(what))
        return rirEval_f(BODY(what), env);

    Rf_error("Not rir compiled code");
}

REXPORT SEXP rir_body(SEXP cls) {
    if (!isValidClosureSEXP(cls))
        Rf_error("Not a valid rir compiled function");
    return DispatchTable::unpack(BODY(cls))->baseline()->container();
}

REXPORT SEXP pir_debugFlags(
#define V(n) SEXP n,
    LIST_OF_PIR_DEBUGGING_FLAGS(V)
#undef V
        SEXP IHaveTooManyCommasDummy) {
    pir::DebugOptions opts;

#define V(n)                                                                   \
    if (Rf_asLogical(n))                                                       \
        opts.flags.set(pir::DebugFlag::n);
    LIST_OF_PIR_DEBUGGING_FLAGS(V)
#undef V

    SEXP res = Rf_allocVector(INTSXP, 1);
    INTEGER(res)[0] = (int)opts.flags.to_i();
    return res;
}

static pir::DebugOptions::DebugFlags getInitialDebugFlags() {
    auto verb = getenv("PIR_DEBUG");
    if (!verb)
        return pir::DebugOptions::DebugFlags();
    std::istringstream in(verb);

    pir::DebugOptions::DebugFlags flags;
    while (!in.fail()) {
        std::string opt;
        std::getline(in, opt, ',');
        if (opt.empty())
            continue;

        bool success = false;

#define V(flag)                                                                \
    if (opt.compare(#flag) == 0) {                                             \
        success = true;                                                        \
        flags = flags | pir::DebugFlag::flag;                                  \
    }
        LIST_OF_PIR_DEBUGGING_FLAGS(V)
#undef V
        if (!success) {
            std::cerr << "Unknown PIR debug flag " << opt << "\n"
                      << "Valid flags are:\n";
#define V(flag) std::cerr << "- " << #flag << "\n";
            LIST_OF_PIR_DEBUGGING_FLAGS(V)
#undef V
            exit(1);
        }
    }
    return flags;
}

static std::regex getInitialDebugPassFilter() {
    auto filter = getenv("PIR_DEBUG_PASS_FILTER");
    if (filter)
        return std::regex(filter);
    return std::regex(".*");
}

static std::regex getInitialDebugFunctionFilter() {
    auto filter = getenv("PIR_DEBUG_FUNCTION_FILTER");
    if (filter)
        return std::regex(filter);
    return std::regex(".*");
}

static pir::DebugOptions PirDebug = {getInitialDebugFlags(),
                                     getInitialDebugPassFilter(),
                                     getInitialDebugFunctionFilter()};

REXPORT SEXP pir_setDebugFlags(SEXP debugFlags) {
    if (TYPEOF(debugFlags) != INTSXP || Rf_length(debugFlags) < 1)
        Rf_error(
            "pir_setDebugFlags expects an integer vector as second parameter");
    PirDebug.flags = pir::DebugOptions::DebugFlags(INTEGER(debugFlags)[0]);
    return R_NilValue;
}

SEXP pirCompile(SEXP what, const Assumptions& assumptions,
                const std::string& name, pir::DebugOptions debug) {

    if (!isValidClosureSEXP(what)) {
        Rf_error("not a compiled closure");
    }
    if (!DispatchTable::check(BODY(what))) {
        Rf_error("Cannot optimize compiled expression, only closure");
    }

    PROTECT(what);

    bool dryRun = debug.includes(pir::DebugFlag::DryRun);
    // compile to pir
    pir::Module* m = new pir::Module;
    pir::StreamLogger logger(debug);
    logger.title("Compiling " + name);
    pir::Rir2PirCompiler cmp(m, logger);
    cmp.compileClosure(what, name, assumptions,
                       [&](pir::ClosureVersion* c) {
                           logger.flush();
                           cmp.optimizeModule();

                           // compile back to rir
                           pir::Pir2RirCompiler p2r(logger);
                           auto fun = p2r.compile(c, dryRun);

                           // Install
                           if (dryRun)
                               return;

                           Protect p(fun->container());
                           DispatchTable::unpack(BODY(what))->insert(fun);
                       },
                       [&]() {
                           if (debug.includes(pir::DebugFlag::ShowWarnings))
                               std::cerr << "Compilation failed\n";
                       });

    delete m;
    UNPROTECT(1);
    return what;
}

// Used in test infrastructure for counting invocation of different versions
REXPORT SEXP rir_invocation_count(SEXP what) {
    if (!isValidClosureSEXP(what)) {
        Rf_error("not a compiled closure");
    }
    auto dt = DispatchTable::check(BODY(what));
    assert(dt);

    SEXP res = Rf_allocVector(INTSXP, dt->size());
    for (size_t i = 0; i < dt->size(); ++i)
        INTEGER(res)[i] = dt->get(i)->invocationCount();

    return res;
}

REXPORT SEXP pir_compile(SEXP what, SEXP name, SEXP debugFlags) {
    if (debugFlags != R_NilValue &&
        (TYPEOF(debugFlags) != INTSXP || Rf_length(debugFlags) < 1))
        Rf_error("pir_compile expects an integer vector as second parameter");
    std::string n;
    if (TYPEOF(name) == SYMSXP)
        n = CHAR(PRINTNAME(name));
    return pirCompile(what, rir::pir::Rir2PirCompiler::defaultAssumptions, n,
                      debugFlags == R_NilValue
                          ? PirDebug
                          : pir::DebugOptions(INTEGER(debugFlags)[0]));
}

REXPORT SEXP pir_tests() {
    PirTests::run();
    return R_NilValue;
}

SEXP rirOptDefaultOpts(SEXP closure, const Assumptions& assumptions,
                       SEXP name) {
    std::string n = "";
    if (TYPEOF(name) == SYMSXP)
        n = CHAR(PRINTNAME(name));
    // PIR can only optimize closures, not expressions
    if (isValidClosureSEXP(closure))
        return pirCompile(closure, assumptions, n, PirDebug);
    else
        return closure;
}

SEXP rirOptDefaultOptsDryrun(SEXP closure, const Assumptions& assumptions,
                             SEXP name) {
    std::string n = "";
    if (TYPEOF(name) == SYMSXP)
        n = CHAR(PRINTNAME(name));
    // PIR can only optimize closures, not expressions
    if (isValidClosureSEXP(closure))
        return pirCompile(closure, assumptions, n,
                          PirDebug | pir::DebugFlag::DryRun);
    else
        return closure;
}

bool startup() {
    initializeRuntime();
    return true;
}

bool startup_ok = startup();
