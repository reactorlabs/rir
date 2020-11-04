#include "builtins.h"

#include "compiler/parameter.h"
#include "interpreter/ArgsLazyData.h"
#include "interpreter/LazyEnvironment.h"
#include "interpreter/cache.h"
#include "interpreter/call_context.h"
#include "interpreter/interp.h"
#include "ir/Deoptimization.h"
#include "utils/Pool.h"

#include "R/Funtab.h"
#include "R/Symbols.h"
#include <R_ext/RS.h> /* for Memzero */

#include <llvm/IR/Attributes.h>

namespace rir {
namespace pir {

struct MatrixDimension {
    R_xlen_t row;
    R_xlen_t col;
};

static R_INLINE MatrixDimension getMatrixDim(SEXP mat) {
    SEXP attr = ATTRIB(mat);
    /* look for the common case of 'dim' as the only attribute first */
    SEXP dim =
        TAG(attr) == R_DimSymbol ? CAR(attr) : getAttrib(mat, R_DimSymbol);
    if (TYPEOF(dim) == INTSXP && LENGTH(dim) == 2)
        return {INTEGER(dim)[0], INTEGER(dim)[1]};
    assert(false);
    return {0, 0};
}

static SEXP forcePromiseImpl(SEXP prom) {
    SLOWASSERT(TYPEOF(prom) == PROMSXP);
    auto res = evaluatePromise(prom);
    return res;
}
NativeBuiltin NativeBuiltins::forcePromise = {"forcePromise",
                                              (void*)&forcePromiseImpl};

NativeBuiltin NativeBuiltins::consNr = {"consNr", (void*)&CONS_NR};

static SEXP createBindingCellImpl(SEXP val, SEXP name, SEXP rest) {
    SEXP res = CONS_NR(val, rest);
    SET_TAG(res, name);
    if (val == R_MissingArg)
        SET_MISSING(res, 2);
    INCREMENT_NAMED(val);
    return res;
}

NativeBuiltin NativeBuiltins::createBindingCell = {
    "createBindingCellImpl", (void*)&createBindingCellImpl};

static SEXP createMissingBindingCellImpl(SEXP val, SEXP name, SEXP rest) {
    SEXP res = CONS_NR(val, rest);
    SET_TAG(res, name);
    SET_MISSING(res, 2);
    INCREMENT_NAMED(val);
    return res;
}

NativeBuiltin NativeBuiltins::createMissingBindingCell = {
    "createMissingBindingCell", (void*)&createMissingBindingCellImpl};

SEXP createEnvironmentImpl(SEXP parent, SEXP arglist, int contextPos) {
    SLOWASSERT(TYPEOF(parent) == ENVSXP);
    SLOWASSERT(TYPEOF(arglist) == LISTSXP || arglist == R_NilValue);
    SEXP res = Rf_NewEnvironment(R_NilValue, arglist, parent);

    if (contextPos > 0) {
        if (auto cptr = getFunctionContext(contextPos - 1)) {
            cptr->cloenv = res;
            if (cptr->promargs == symbol::delayedArglist) {
                auto promargs = arglist;
                bool hasMissing = false;
                auto a = arglist;
                while (a != R_NilValue) {
                    if (CAR(a) == R_MissingArg)
                        hasMissing = true;
                    a = CDR(a);
                }

                if (hasMissing) {
                    // For the promargs we need to strip missing
                    // arguments from the list, otherwise nargs()
                    // reports the wrong value.
                    promargs = Rf_shallow_duplicate(arglist);
                    auto p = promargs;
                    auto a = arglist;
                    auto prev = p;
                    while (p != R_NilValue) {
                        if (MISSING(a))
                            SETCDR(prev, CDR(p));
                        prev = p;
                        p = CDR(p);
                        a = CDR(a);
                    }
                }
                cptr->promargs = promargs;
            }
        }
    }

    return res;
}

NativeBuiltin NativeBuiltins::createEnvironment = {
    "createEnvironment", (void*)&createEnvironmentImpl};

SEXP createStubEnvironmentImpl(SEXP parent, int n, Immediate* names,
                               int contextPos) {
    SLOWASSERT(TYPEOF(parent) == ENVSXP);
    SEXP res = LazyEnvironment::BasicNew(parent, n, names)->container();
    if (contextPos > 0) {
        if (auto cptr = getFunctionContext(contextPos - 1)) {
            cptr->cloenv = res;
        }
    }
    return res;
}

NativeBuiltin NativeBuiltins::createStubEnvironment = {
    "createStubEnvironment",
    (void*)&createStubEnvironmentImpl,
};

SEXP materializeEnvironmentImpl(SEXP environment) {
    auto lazyEnv = LazyEnvironment::check(environment);
    assert(lazyEnv);
    if (!lazyEnv->materialized())
        return materialize(environment);
    return environment;
}

NativeBuiltin NativeBuiltins::materializeEnvironment = {
    "materializeEnvironment",
    (void*)&materializeEnvironmentImpl,
};

SEXP ldvarForUpdateImpl(SEXP sym, SEXP env) {
    R_varloc_t loc = R_findVarLocInFrame(env, sym);
    bool isLocal = !R_VARLOC_IS_NULL(loc);
    SEXP res = nullptr;
    if (isLocal && CAR(loc.cell) != R_UnboundValue) {
        res = CAR(loc.cell);
    } else {
        res = Rf_findVar(sym, env);
    }
    if (res != R_NilValue) {
        if (isLocal)
            ENSURE_NAMED(res);
        else if (NAMED(res) < 2)
            SET_NAMED(res, 2);
    }
    return res;
}

NativeBuiltin NativeBuiltins::ldvarForUpdate = {
    "ldvarForUpdate",
    (void*)&ldvarForUpdateImpl,
};

SEXP ldvarImpl(SEXP a, SEXP b) {
    auto res = Rf_findVar(a, b);
    // std::cout << CHAR(PRINTNAME(a)) << "=";
    // Rf_PrintValue(res);
    ENSURE_NAMED(res);
    return res;
}

NativeBuiltin NativeBuiltins::ldvar = {
    "ldvar",
    (void*)&ldvarImpl,
};

SEXP ldvarCachedImpl(SEXP sym, SEXP env, SEXP* cache) {
    if (*cache != (SEXP)1) {
        R_varloc_t loc = R_findVarLocInFrame(env, sym);
        if (R_VARLOC_IS_NULL(loc)) {
            *cache = (SEXP)1;
        } else {
            *cache = loc.cell;
            if (CAR(*cache) != R_UnboundValue) {
                ENSURE_NAMED(*cache);
                return CAR(*cache);
            }
        }
    }
    auto res = Rf_findVar(sym, env);
    ENSURE_NAMED(res);
    return res;
}

NativeBuiltin NativeBuiltins::ldvarCacheMiss = {
    "ldvarCacheMiss",
    (void*)&ldvarCachedImpl,
};

void stvarImpl(SEXP a, SEXP val, SEXP c) { rirDefineVarWrapper(a, val, c); }
NativeBuiltin NativeBuiltins::stvar = {
    "stvar",
    (void*)&stvarImpl,
};

void stvarImplI(SEXP a, int val, SEXP c) { rirDefineVarWrapperI(a, val, c); }
NativeBuiltin NativeBuiltins::stvari = {
    "stvari",
    (void*)&stvarImplI,
};

void stargImpl(SEXP sym, SEXP val, SEXP env) {
    // In case there is a local binding we must honor missingness which
    // defineVar does not
    if (env != R_BaseEnv && env != R_BaseNamespace) {
        R_varloc_t loc = R_findVarLocInFrame(env, sym);
        if (!R_VARLOC_IS_NULL(loc) && !BINDING_IS_LOCKED(loc.cell) &&
            !IS_ACTIVE_BINDING(loc.cell)) {
            SEXP cur = CAR(loc.cell);
            if (cur != val) {
                INCREMENT_NAMED(val);
                SETCAR(loc.cell, val);
            } else {
                ENSURE_NAMED(val);
            }
            return;
        }
    }

    rirDefineVarWrapper(sym, val, env);
}
NativeBuiltin NativeBuiltins::starg = {
    "starg",
    (void*)&stargImpl,
};

void setCarImpl(SEXP x, SEXP y) {
    assert(x->sxpinfo.mark && "Use fastpath setCar");
    assert((!y->sxpinfo.mark || y->sxpinfo.gcgen < x->sxpinfo.gcgen) &&
           "use fast path setCar");
    SETCAR(x, y);
}

NativeBuiltin NativeBuiltins::setCar = {
    "setCar", (void*)&setCarImpl, nullptr, {llvm::Attribute::ArgMemOnly}};

void setCdrImpl(SEXP x, SEXP y) {
    assert(x->sxpinfo.mark && "Use fastpath setCdr");
    assert((!y->sxpinfo.mark || y->sxpinfo.gcgen < x->sxpinfo.gcgen) &&
           "use fast path setCdr");
    SETCDR(x, y);
}

NativeBuiltin NativeBuiltins::setCdr = {
    "setCdr", (void*)&setCdrImpl, nullptr, {llvm::Attribute::ArgMemOnly}};

void setTagImpl(SEXP x, SEXP y) {
    assert(x->sxpinfo.mark && "Use fastpath setTag");
    assert((!y->sxpinfo.mark || y->sxpinfo.gcgen < x->sxpinfo.gcgen) &&
           "use fast path setTag");
    SETCAR(x, y);
}

NativeBuiltin NativeBuiltins::setTag = {
    "setTag", (void*)&setTagImpl, nullptr, {llvm::Attribute::ArgMemOnly}};

void externalsxpSetEntryImpl(SEXP x, int i, SEXP y) {
    assert(x->sxpinfo.mark && "Use fastpath setEntry");
    assert((!y->sxpinfo.mark || y->sxpinfo.gcgen < x->sxpinfo.gcgen) &&
           "use fast path setEntry");
    EXTERNALSXP_SET_ENTRY(x, i, y);
}

NativeBuiltin NativeBuiltins::externalsxpSetEntry = {
    "externalsxpSetEntry",
    (void*)&externalsxpSetEntryImpl,
    nullptr,
    {llvm::Attribute::ArgMemOnly}};

void defvarImpl(SEXP var, SEXP value, SEXP env) {
    assert(TYPEOF(env) == ENVSXP);
    rirSetVarWrapper(var, value, ENCLOS(env));
}

NativeBuiltin NativeBuiltins::defvar = {
    "defvar",
    (void*)&defvarImpl,
};

SEXP chkfunImpl(SEXP sym, SEXP res) {
    switch (TYPEOF(res)) {
    case CLOSXP:
        jit(res, sym, globalContext());
        break;
    case SPECIALSXP:
    case BUILTINSXP:
        // special and builtin functions are ok
        break;
    default:
        Rf_error("attempt to apply non-function");
    }
    return res;
}

SEXP ldfunImpl(SEXP sym, SEXP env) {
    SEXP res = Rf_findFun(sym, env);

    // TODO something should happen here
    if (res == R_UnboundValue)
        assert(false && "Unbound var");
    if (res == R_MissingArg)
        assert(false && "Missing argument");

    chkfunImpl(sym, res);
    return res;
}

NativeBuiltin NativeBuiltins::ldfun = {
    "ldfun",
    (void*)&ldfunImpl,
};

NativeBuiltin NativeBuiltins::chkfun = {
    "chkfun",
    (void*)&chkfunImpl,
};

static void warnImpl(const char* w) { Rf_warning(w); }

NativeBuiltin NativeBuiltins::warn = {
    "warn",
    (void*)&warnImpl,
};

static void errorImpl(const char* e) { Rf_error(e); }

NativeBuiltin NativeBuiltins::error = {
    "error", (void*)&errorImpl, nullptr, {llvm::Attribute::NoReturn}};

static bool debugPrintCallBuiltinImpl = false;
static SEXP callBuiltinImpl(rir::Code* c, Immediate ast, SEXP callee, SEXP env,
                            size_t nargs) {
    auto ctx = globalContext();
    CallContext call(c, callee, nargs, ast, ostack_cell_at(ctx, nargs - 1), env,
                     Context(), ctx);
    if (debugPrintCallBuiltinImpl) {
        debugPrintCallBuiltinImpl = false;
        std::cout << "call builtin " << nargs << " with\n";
        Rf_PrintValue(callee);
        if (env)
            Rf_PrintValue(env);

        for (long i = 0; i < (long)nargs; ++i) {
            std::cout << i << " ";
            Rf_PrintValue(call.stackArg(i));
        }
        debugPrintCallBuiltinImpl = true;
    }
    SLOWASSERT(TYPEOF(callee) == BUILTINSXP);
    SLOWASSERT(TYPEOF(env) == ENVSXP || LazyEnvironment::check(env));
    SLOWASSERT(ctx);
    auto res = builtinCall(call, ctx);
    SLOWASSERT(res);
    return res;
}
NativeBuiltin NativeBuiltins::callBuiltin = {
    "callBuiltin",
    (void*)&callBuiltinImpl,
};

static SEXP callImplCached(CallContext& call, Immediate cache) {
    auto res = doCall(call, globalContext());
    if (cache != 0) {
        auto trg = dispatch(call, DispatchTable::unpack(BODY(call.callee)));
        Pool::patch(cache, trg->container());
    }
    ostack_popn(ctx, call.passedArgs - call.suppliedArgs);
    return res;
}

static SEXP callImplCached(rir::Code* c, Immediate ast, SEXP callee, SEXP env,
                           size_t nargs, unsigned long available,
                           Immediate cache) {
    auto ctx = globalContext();
    CallContext call(c, callee, nargs, ast, ostack_cell_at(ctx, nargs - 1), env,
                     Context(available), ctx);

    SLOWASSERT(env == symbol::delayedEnv || TYPEOF(env) == ENVSXP ||
               LazyEnvironment::check(env) || env == R_NilValue);
    SLOWASSERT(ctx);
    return callImplCached(call, cache);
}

static SEXP callImpl(rir::Code* c, Immediate ast, SEXP callee, SEXP env,
                     size_t nargs, unsigned long available) {
    return callImplCached(c, ast, callee, env, nargs, available, 0);
};

NativeBuiltin NativeBuiltins::call = {
    "call",
    (void*)&callImpl,
};

static SEXP namedCallImpl(rir::Code* c, Immediate ast, SEXP callee, SEXP env,
                          size_t nargs, Immediate* names,
                          unsigned long available) {
    auto ctx = globalContext();
    CallContext call(c, callee, nargs, ast, ostack_cell_at(ctx, nargs - 1),
                     names, env, Context(available), ctx);
    SLOWASSERT(env == symbol::delayedEnv || TYPEOF(env) == ENVSXP ||
               LazyEnvironment::check(env));
    SLOWASSERT(ctx);
    auto res = doCall(call, ctx);
    ostack_popn(ctx, call.passedArgs - call.suppliedArgs);
    return res;
}

NativeBuiltin NativeBuiltins::namedCall = {
    "namedCall",
    (void*)&namedCallImpl,
};

static SEXP dotsCallImpl(rir::Code* c, Immediate ast, SEXP callee, SEXP env,
                         size_t nargs, Immediate* names,
                         unsigned long available) {
    auto ctx = globalContext();
    auto given = Context(available);
    int pushed = 0;

    if (TYPEOF(callee) != SPECIALSXP) {
        nargs = expandDotDotDotCallArgs(
            ctx, nargs, names, env,
            given.includes(Assumption::StaticallyArgmatched));
        auto namesStore = ostack_at(ctx, nargs);
        if (namesStore == R_NilValue)
            names = nullptr;
        else
            names = (Immediate*)DATAPTR(namesStore);
        pushed = 1;
    }

    CallContext call(c, callee, nargs, ast, ostack_cell_at(ctx, nargs - 1),
                     names, env, given, ctx);
    SLOWASSERT(env == symbol::delayedEnv || TYPEOF(env) == ENVSXP ||
               LazyEnvironment::check(env));
    SLOWASSERT(ctx);
    auto res = doCall(call, ctx);
    ostack_popn(ctx, call.passedArgs + pushed);
    return res;
}

NativeBuiltin NativeBuiltins::dotsCall = {
    "dotsCall",
    (void*)&dotsCallImpl,
};

SEXP createPromiseImpl(SEXP expr, SEXP env) {
    SEXP res = Rf_mkPROMISE(expr, env);
    SET_PRVALUE(res, R_UnboundValue);
    return res;
}

NativeBuiltin NativeBuiltins::createPromise = {
    "createPromise",
    (void*)&createPromiseImpl,
};

SEXP createPromiseNoEnvEagerImpl(SEXP exp, SEXP value) {
    SLOWASSERT(TYPEOF(value) != PROMSXP);
    SEXP res = Rf_mkPROMISE(exp, R_EmptyEnv);
    ENSURE_NAMEDMAX(value);
    SET_PRVALUE(res, value);
    return res;
}

NativeBuiltin NativeBuiltins::createPromiseNoEnvEager = {
    "createPromiseNoEnvEager",
    (void*)&createPromiseNoEnvEagerImpl,
};

SEXP createPromiseNoEnvImpl(SEXP exp) { return Rf_mkPROMISE(exp, R_EmptyEnv); }

NativeBuiltin NativeBuiltins::createPromiseNoEnv = {
    "createPromiseNoEnv",
    (void*)&createPromiseNoEnvImpl,
};

SEXP createPromiseEagerImpl(SEXP exp, SEXP env, SEXP value) {
    SLOWASSERT(TYPEOF(value) != PROMSXP);
    SEXP res = Rf_mkPROMISE(exp, env);
    ENSURE_NAMEDMAX(value);
    SET_PRVALUE(res, value);
    return res;
}

NativeBuiltin NativeBuiltins::createPromiseEager = {
    "createPromiseEager",
    (void*)&createPromiseEagerImpl,
};

SEXP createClosureImpl(SEXP body, SEXP formals, SEXP env, SEXP srcref) {
    auto res = Rf_allocSExp(CLOSXP);
    SET_FORMALS(res, formals);
    SET_BODY(res, body);
    SET_CLOENV(res, env);
    Rf_setAttrib(res, Rf_install("srcref"), srcref);
    return res;
}

NativeBuiltin NativeBuiltins::createClosure = {
    "createClosure",
    (void*)&createClosureImpl,
};

SEXP newLglImpl(int i) {
    auto res = Rf_allocVector(LGLSXP, 1);
    LOGICAL(res)[0] = i;
    return res;
}

SEXP newIntImpl(int i) {
    auto res = Rf_allocVector(INTSXP, 1);
    INTEGER(res)[0] = i;
    return res;
}

SEXP newIntDebugImpl(int i, void* debug) {
    std::cout << (char*)debug << "\n";
    auto res = Rf_allocVector(INTSXP, 1);
    INTEGER(res)[0] = i;
    return res;
}

SEXP newLglFromRealImpl(double d) {
    auto res = Rf_allocVector(LGLSXP, 1);
    if (d != d)
        LOGICAL(res)[0] = NA_LOGICAL;
    else
        LOGICAL(res)[0] = d;
    return res;
}

SEXP newIntFromRealImpl(double d) {
    auto res = Rf_allocVector(INTSXP, 1);
    if (d != d)
        INTEGER(res)[0] = NA_INTEGER;
    else
        INTEGER(res)[0] = d;
    return res;
}

SEXP newRealImpl(double i) {
    auto res = Rf_allocVector(REALSXP, 1);
    REAL(res)[0] = i;
    return res;
}
SEXP newRealFromIntImpl(int i) {
    auto res = Rf_allocVector(REALSXP, 1);
    if (i == NA_INTEGER)
        REAL(res)[0] = NAN;
    else
        REAL(res)[0] = i;
    return res;
}

NativeBuiltin NativeBuiltins::newIntFromReal = {
    "newIntFromReal",
    (void*)&newIntFromRealImpl,
};
NativeBuiltin NativeBuiltins::newRealFromInt = {
    "newRealFromInt",
    (void*)&newRealFromIntImpl,
};
NativeBuiltin NativeBuiltins::newLglFromReal = {
    "newLglFromReal",
    (void*)&newLglFromRealImpl,
};
NativeBuiltin NativeBuiltins::newInt = {
    "newInt",
    (void*)&newIntImpl,
};
NativeBuiltin NativeBuiltins::newIntDebug = {
    "newIntDebug",
    (void*)&newIntDebugImpl,
};
NativeBuiltin NativeBuiltins::newReal = {
    "newReal",
    (void*)&newRealImpl,
};
NativeBuiltin NativeBuiltins::newLgl = {
    "newLgl",
    (void*)&newLglImpl,
};

#define OPERATION_FALLBACK(op)                                                 \
    do {                                                                       \
        static SEXP prim = NULL;                                               \
        static CCODE blt;                                                      \
        static int flag;                                                       \
        if (!prim) {                                                           \
            prim = Rf_findFun(Rf_install(op), R_GlobalEnv);                    \
            blt = getBuiltin(prim);                                            \
            flag = getFlag(prim);                                              \
        }                                                                      \
        if (flag < 2)                                                          \
            R_Visible = static_cast<Rboolean>(flag != 1);                      \
        res = blt(call, prim, arglist, env);                                   \
        if (flag < 2)                                                          \
            R_Visible = static_cast<Rboolean>(flag != 1);                      \
    } while (false)

static void createFakeSEXP(SEXPREC& res, SEXPTYPE t) {
    memset(&res, 0, sizeof(SEXPREC));
    res.attrib = R_NilValue;
    res.gengc_next_node = R_NilValue;
    res.gengc_prev_node = R_NilValue;
    res.sxpinfo.gcgen = 1;
    res.sxpinfo.mark = 1;
    res.sxpinfo.named = 2;
    res.sxpinfo.type = t;
}

static void createFakeCONS(SEXPREC& res, SEXP cdr) {
    createFakeSEXP(res, LISTSXP);
    res.u.listsxp.carval = R_NilValue;
    res.u.listsxp.tagval = R_NilValue;
    res.u.listsxp.cdrval = cdr;
}

#define FAKE_ARGS2(res, a1, a2)                                                \
    SEXPREC __a2__cell__;                                                      \
    createFakeCONS(__a2__cell__, R_NilValue);                                  \
    SEXPREC __a1__cell__;                                                      \
    createFakeCONS(__a1__cell__, &__a2__cell__);                               \
    __a1__cell__.u.listsxp.carval = a1;                                        \
    __a2__cell__.u.listsxp.carval = a2;                                        \
    res = &__a1__cell__

#define FAKE_ARGS3(res, a1, a2, a3)                                            \
    SEXPREC __a3__cell__;                                                      \
    createFakeCONS(__a3__cell__, R_NilValue);                                  \
    SEXPREC __a2__cell__;                                                      \
    createFakeCONS(__a2__cell__, &__a3__cell__);                               \
    SEXPREC __a1__cell__;                                                      \
    createFakeCONS(__a1__cell__, &__a2__cell__);                               \
    __a1__cell__.u.listsxp.carval = a1;                                        \
    __a2__cell__.u.listsxp.carval = a2;                                        \
    __a3__cell__.u.listsxp.carval = a3;                                        \
    res = &__a1__cell__

#define FAKE_ARGS4(res, a1, a2, a3, a4)                                        \
    SEXPREC __a4__cell__;                                                      \
    createFakeCONS(__a4__cell__, R_NilValue);                                  \
    SEXPREC __a3__cell__;                                                      \
    createFakeCONS(__a3__cell__, &__a4__cell__);                               \
    SEXPREC __a2__cell__;                                                      \
    createFakeCONS(__a2__cell__, &__a3__cell__);                               \
    SEXPREC __a1__cell__;                                                      \
    createFakeCONS(__a1__cell__, &__a2__cell__);                               \
    __a1__cell__.u.listsxp.carval = a1;                                        \
    __a2__cell__.u.listsxp.carval = a2;                                        \
    __a3__cell__.u.listsxp.carval = a3;                                        \
    __a4__cell__.u.listsxp.carval = a4;                                        \
    res = &__a1__cell__

static SEXP unopEnvImpl(SEXP argument, SEXP env, Immediate srcIdx,
                        UnopKind op) {
    SEXP res = nullptr;
    SEXP arglist = CONS_NR(argument, R_NilValue);
    SEXP call = src_pool_at(globalContext(), srcIdx);
    PROTECT(arglist);
    switch (op) {
    case UnopKind::MINUS:
        OPERATION_FALLBACK("-");
        break;
    case UnopKind::PLUS:
        OPERATION_FALLBACK("+");
        break;
    }
    UNPROTECT(1);
    SLOWASSERT(res);
    return res;
}

NativeBuiltin NativeBuiltins::unopEnv = {
    "unopEnv",
    (void*)&unopEnvImpl,
};

static SEXP unopImpl(SEXP argument, UnopKind op) {
    SEXP res = nullptr;
    SEXPREC arglistStruct;
    createFakeCONS(arglistStruct, R_NilValue);
    arglistStruct.u.listsxp.carval = argument;
    SEXP arglist = &arglistStruct;
    SEXP env = R_NilValue;
    SEXP call = R_NilValue;
    switch (op) {
    case UnopKind::MINUS:
        OPERATION_FALLBACK("-");
        break;
    case UnopKind::PLUS:
        OPERATION_FALLBACK("+");
        break;
    }
    SLOWASSERT(res);
    return res;
}

NativeBuiltin NativeBuiltins::unop = {
    "unop",
    (void*)&unopImpl,
};

static SEXP notEnvImpl(SEXP argument, SEXP env, Immediate srcIdx) {
    SEXP res = nullptr;
    SEXP arglist = CONS_NR(argument, R_NilValue);
    SEXP call = src_pool_at(globalContext(), srcIdx);
    PROTECT(arglist);
    OPERATION_FALLBACK("!");
    UNPROTECT(1);
    SLOWASSERT(res);
    return res;
}

NativeBuiltin NativeBuiltins::notEnv = {
    "notEnv",
    (void*)&notEnvImpl,
};

static SEXP notImpl(SEXP argument) {
    SEXP res = nullptr;
    SEXPREC arglistStruct;
    createFakeCONS(arglistStruct, R_NilValue);
    arglistStruct.u.listsxp.carval = argument;
    SEXP arglist = &arglistStruct;
    SEXP env = R_NilValue;
    SEXP call = R_NilValue;
    // Why we do not need a protect here?
    OPERATION_FALLBACK("!");
    SLOWASSERT(res);
    return res;
}

NativeBuiltin NativeBuiltins::notOp = {"not", (void*)&notImpl};

static SEXP binopEnvImpl(SEXP lhs, SEXP rhs, SEXP env, Immediate srcIdx,
                         BinopKind kind) {
    SEXP res = nullptr;
    SEXP arglist2 = CONS_NR(rhs, R_NilValue);
    SEXP arglist = CONS_NR(lhs, arglist2);
    SEXP call = src_pool_at(globalContext(), srcIdx);

    PROTECT(arglist);
    switch (kind) {
    case BinopKind::ADD:
        OPERATION_FALLBACK("+");
        break;
    case BinopKind::SUB:
        OPERATION_FALLBACK("-");
        break;
    case BinopKind::MUL:
        OPERATION_FALLBACK("*");
        break;
    case BinopKind::IDIV:
        OPERATION_FALLBACK("%/%");
        break;
    case BinopKind::DIV:
        OPERATION_FALLBACK("/");
        break;
    case BinopKind::EQ:
        OPERATION_FALLBACK("==");
        break;
    case BinopKind::NE:
        OPERATION_FALLBACK("!=");
        break;
    case BinopKind::GT:
        OPERATION_FALLBACK(">");
        break;
    case BinopKind::GTE:
        OPERATION_FALLBACK(">=");
        break;
    case BinopKind::LT:
        OPERATION_FALLBACK("<");
        break;
    case BinopKind::LTE:
        OPERATION_FALLBACK("<=");
        break;
    case BinopKind::LAND:
        OPERATION_FALLBACK("&&");
        break;
    case BinopKind::LOR:
        OPERATION_FALLBACK("||");
        break;
    case BinopKind::COLON:
        OPERATION_FALLBACK(":");
        break;
    case BinopKind::MOD:
        OPERATION_FALLBACK("%%");
        break;
    case BinopKind::POW:
        OPERATION_FALLBACK("^");
        break;
    }
    UNPROTECT(1);
    SLOWASSERT(res);
    return res;
}

NativeBuiltin NativeBuiltins::binopEnv = {
    "binopEnv",
    (void*)&binopEnvImpl,
};

bool debugBinopImpl = false;
static SEXP binopImpl(SEXP lhs, SEXP rhs, BinopKind kind) {
    SEXP res = nullptr;

    SEXP arglist;
    FAKE_ARGS2(arglist, lhs, rhs);
    SEXP env = R_NilValue;
    SEXP call = R_NilValue;

    if (debugBinopImpl) {
        debugBinopImpl = false;
        std::cout << "call binop " << (int)kind << " with\n";
        Rf_PrintValue(lhs);
        Rf_PrintValue(rhs);

        debugBinopImpl = true;
    }

    // Why we do not need a protect here?
    switch (kind) {
    case BinopKind::ADD:
        OPERATION_FALLBACK("+");
        break;
    case BinopKind::SUB:
        OPERATION_FALLBACK("-");
        break;
    case BinopKind::MUL:
        OPERATION_FALLBACK("*");
        break;
    case BinopKind::IDIV:
        OPERATION_FALLBACK("%/%");
        break;
    case BinopKind::DIV:
        OPERATION_FALLBACK("/");
        break;
    case BinopKind::EQ:
        OPERATION_FALLBACK("==");
        break;
    case BinopKind::NE:
        OPERATION_FALLBACK("!=");
        break;
    case BinopKind::GT:
        OPERATION_FALLBACK(">");
        break;
    case BinopKind::GTE:
        OPERATION_FALLBACK(">=");
        break;
    case BinopKind::LT:
        OPERATION_FALLBACK("<");
        break;
    case BinopKind::LTE:
        OPERATION_FALLBACK("<=");
        break;
    case BinopKind::LAND:
        OPERATION_FALLBACK("&&");
        break;
    case BinopKind::LOR:
        OPERATION_FALLBACK("||");
        break;
    case BinopKind::COLON:
        if (IS_SIMPLE_SCALAR(lhs, INTSXP)) {
            int from = *INTEGER(lhs);
            if (IS_SIMPLE_SCALAR(rhs, INTSXP)) {
                int to = *INTEGER(rhs);
                if (from != NA_INTEGER && to != NA_INTEGER) {
                    res = seq_int(from, to);
                }
            } else if (IS_SIMPLE_SCALAR(rhs, REALSXP)) {
                double to = *REAL(rhs);
                if (from != NA_INTEGER && to != NA_REAL && R_FINITE(to) &&
                    INT_MIN <= to && INT_MAX >= to && to == (int)to) {
                    res = seq_int(from, (int)to);
                }
            }
        } else if (IS_SIMPLE_SCALAR(lhs, REALSXP)) {
            double from = *REAL(lhs);
            if (IS_SIMPLE_SCALAR(rhs, INTSXP)) {
                int to = *INTEGER(rhs);
                if (from != NA_REAL && to != NA_INTEGER && R_FINITE(from) &&
                    INT_MIN <= from && INT_MAX >= from && from == (int)from) {
                    res = seq_int((int)from, to);
                }
            } else if (IS_SIMPLE_SCALAR(rhs, REALSXP)) {
                double to = *REAL(rhs);
                if (from != NA_REAL && to != NA_REAL && R_FINITE(from) &&
                    R_FINITE(to) && INT_MIN <= from && INT_MAX >= from &&
                    INT_MIN <= to && INT_MAX >= to && from == (int)from &&
                    to == (int)to) {
                    res = seq_int((int)from, (int)to);
                }
            }
        }

        if (res != NULL) {
            R_Visible = (Rboolean) true;
        } else {
            OPERATION_FALLBACK(":");
        }
        break;
    case BinopKind::MOD:
        OPERATION_FALLBACK("%%");
        break;
    case BinopKind::POW:
        OPERATION_FALLBACK("^");
        break;
    }
    SLOWASSERT(res);

    if (debugBinopImpl) {
        debugBinopImpl = false;
        std::cout << "call binop " << (int)kind << " got\n";
        Rf_PrintValue(res);

        debugBinopImpl = true;
    }

    return res;
}

NativeBuiltin NativeBuiltins::binop = {
    "binop",
    (void*)&binopImpl,
};

SEXP colonImpl(int from, int to) {
    if (from != NA_INTEGER && to != NA_INTEGER) {
        return seq_int(from, to);
    }
    Rf_errorcall(
        // TODO: pass srcid
        R_NilValue, "NA/NaN argument");
    return nullptr;
}

NativeBuiltin NativeBuiltins::colon = {
    "colon",
    (void*)&colonImpl,
};

int isMissingImpl(SEXP symbol, SEXP environment) {
    // TODO: Send the proper src
    return rir::isMissing(symbol, environment, nullptr, nullptr);
}

NativeBuiltin NativeBuiltins::isMissing = {
    "isMissing",
    (void*)&isMissingImpl,
};

int astestImpl(SEXP val) {
    int cond = NA_LOGICAL;
    if (XLENGTH(val) > 1)
        Rf_warningcall(
            // TODO: pass srcid
            R_NilValue, "the condition has length > 1 and only the first "
                        "element will be used");

    if (XLENGTH(val) > 0) {
        switch (TYPEOF(val)) {
        case LGLSXP:
            cond = LOGICAL(val)[0];
            break;
        case INTSXP:
            cond = INTEGER(val)[0]; // relies on NA_INTEGER == NA_LOGICAL
            break;
        default:
            cond = Rf_asLogical(val);
        }
    }

    if (cond == NA_LOGICAL) {
        const char* msg =
            XLENGTH(val) ? (isLogical(val)
                                ? ("missing value where TRUE/FALSE needed")
                                : ("argument is not interpretable as logical"))
                         : ("argument is of length zero");
        Rf_errorcall(
            // TODO: pass srcid
            R_NilValue, msg);
    }

    return cond;
}

NativeBuiltin NativeBuiltins::asTest = {
    "astest",
    (void*)&astestImpl,
};

int asLogicalImpl(SEXP a) {
    if (!Rf_isNumber(a)) {
      Rf_errorcall(R_NilValue, "argument has the wrong type for && or ||");
    }
    return Rf_asLogical(a);
}

NativeBuiltin NativeBuiltins::asLogicalBlt = {"aslogical",
                                              (void*)&asLogicalImpl};

size_t lengthImpl(SEXP e) { return Rf_length(e); }

NativeBuiltin NativeBuiltins::length = {
    "length", (void*)&lengthImpl, nullptr, {llvm::Attribute::ReadOnly}};

void deoptImpl(Code* c, SEXP cls, DeoptMetadata* m, R_bcstack_t* args) {
    if (!pir::Parameter::DEOPT_CHAOS) {
        if (cls) {
            // TODO: this version is still reachable from static call inline
            // caches. Thus we need to preserve it forever. We need some
            // dependency management here.
            Pool::insert(c->container());
            // remove the deoptimized function. Unless on deopt chaos,
            // always recompiling would just blow testing time...
            auto dt = DispatchTable::unpack(BODY(cls));
            dt->remove(c);
        } else {
            // In some cases we don't know the callee here, so we can't properly
            // remove the deoptimized code. But we can kill the native code,
            // this will cause a fallback to rir, which will then be able to
            // deoptimize properly.
            // TODO: find a way to always know the closure in native code!
            c->nativeCode = nullptr;
        }
    }
    assert(m->numFrames >= 1);
    size_t stackHeight = 0;
    for (size_t i = 0; i < m->numFrames; ++i) {
        stackHeight += m->frames[i].stackSize + 1;
    }

    c->registerDeopt();
    SEXP env =
        ostack_at(ctx, stackHeight - m->frames[m->numFrames - 1].stackSize - 1);
    CallContext call(c, cls, /* nargs */ -1,
                     src_pool_at(globalContext(), c->src), args,
                     (Immediate*)nullptr, env, Context(), globalContext());

    deoptFramesWithContext(globalContext(), &call, m, R_NilValue,
                           m->numFrames - 1, stackHeight,
                           (RCNTXT*)R_GlobalContext);
    assert(false);
}

NativeBuiltin NativeBuiltins::deopt = {
    "deopt", (void*)&deoptImpl, nullptr, {llvm::Attribute::NoReturn}};
NativeBuiltin NativeBuiltins::recordDeopt = {
    "recordDeopt",
    (void*)&recordDeoptReason,
};

void assertFailImpl(const char* msg) {
    std::cout << "Assertion in jitted code failed: '" << msg << "'\n";
    asm("int3");
}
NativeBuiltin NativeBuiltins::assertFail = {
    "assertFail", (void*)&assertFailImpl, nullptr, {llvm::Attribute::NoReturn}};

void printValueImpl(SEXP v) { Rf_PrintValue(v); }
NativeBuiltin NativeBuiltins::printValue = {
    "printValue",
    (void*)printValueImpl,
};

static SEXP tryFastVeceltInt(SEXP vec, R_xlen_t i, bool subset2) {
    if (i == NA_INTEGER)
        return nullptr;
    if (subset2 || fastVeceltOk(vec)) {
        switch (TYPEOF(vec)) {
        case REALSXP:
            if (XLENGTH(vec) <= i)
                break;
            return ScalarReal(REAL_ELT(vec, i));
        case INTSXP:
            if (XLENGTH(vec) <= i)
                break;
            return ScalarInteger(INTEGER_ELT(vec, i));
        case LGLSXP:
            if (XLENGTH(vec) <= i)
                break;
            return ScalarLogical(LOGICAL_ELT(vec, i));
        case CPLXSXP:
            if (XLENGTH(vec) <= i)
                break;
            return ScalarComplex(COMPLEX_ELT(vec, i));
        case RAWSXP:
            if (XLENGTH(vec) <= i)
                break;
            return ScalarRaw(RAW(vec)[i]);
        case VECSXP:
            if (XLENGTH(vec) <= i)
                break;
            SEXP elt = VECTOR_ELT(vec, i);
            RAISE_NAMED(elt, NAMED(vec));
            if (subset2) {
                return elt;
            } else {
                SEXP t = allocVector(VECSXP, 1);
                SET_VECTOR_ELT(t, 0, elt);
                return t;
            }
        }
    }
    return nullptr;
}

static SEXP tryFastVeceltSexp(SEXP vector, SEXP index, bool subset2) {
    if (!fastVeceltOk(vector))
        return nullptr;

    if (IS_SIMPLE_SCALAR(index, INTSXP)) {
        auto i = *INTEGER(index);
        if (i > 0 && i != NA_INTEGER)
            return tryFastVeceltInt(vector, i - 1, subset2);
    } else if (IS_SIMPLE_SCALAR(index, REALSXP)) {
        auto ri = *REAL(index);
        if (ri == ri && ri >= 1.0) {
            return tryFastVeceltInt(vector, *REAL(index) - 1, subset2);
        }
    }

    return nullptr;
}

SEXP extract11Impl(SEXP vector, SEXP index, SEXP env, Immediate srcIdx) {
    SEXP res = tryFastVeceltSexp(vector, index, false);
    if (res)
        return res;

    if (isObject(vector)) {
        SEXP call = src_pool_at(globalContext(), srcIdx);
        SEXP args = CONS_NR(vector, CONS_NR(index, R_NilValue));
        PROTECT(args);
        res = dispatchApply(call, vector, args, symbol::Bracket, env,
                            globalContext());
        if (!res)
            res = do_subset_dflt(call, symbol::Bracket, args, env);
        UNPROTECT(1);
    } else {
        SEXP args;
        FAKE_ARGS2(args, vector, index);
        res = do_subset_dflt(R_NilValue, symbol::Bracket, args, env);
    }
    return res;
}

NativeBuiltin NativeBuiltins::extract11 = {
    "extract1_1D",
    (void*)&extract11Impl,
};

SEXP extract21Impl(SEXP vector, SEXP index, SEXP env, Immediate srcIdx) {
    SEXP res = tryFastVeceltSexp(vector, index, true);
    if (res)
        return res;

    SEXP args = CONS_NR(vector, CONS_NR(index, R_NilValue));
    PROTECT(args);
    if (isObject(vector)) {
        SEXP call = src_pool_at(globalContext(), srcIdx);
        res = dispatchApply(call, vector, args, symbol::DoubleBracket, env,
                            globalContext());
        if (!res)
            res = do_subset2_dflt(call, symbol::DoubleBracket, args, env);
    } else {
        res = do_subset2_dflt(R_NilValue, symbol::DoubleBracket, args, env);
    }
    UNPROTECT(1);
    return res;
}

NativeBuiltin NativeBuiltins::extract21 = {
    "extract2_1D",
    (void*)&extract21Impl,
};

SEXP extract21iImpl(SEXP vector, int index, SEXP env, Immediate srcIdx) {
    SEXP res = nullptr;
    if (index > 0)
        res = tryFastVeceltInt(vector, index - 1, true);
    if (res)
        return res;

    SEXP args = CONS_NR(vector, CONS_NR(ScalarInteger(index), R_NilValue));
    PROTECT(args);
    if (isObject(vector)) {
        SEXP call = src_pool_at(globalContext(), srcIdx);
        res = dispatchApply(call, vector, args, symbol::DoubleBracket, env,
                            globalContext());
        if (!res)
            res = do_subset2_dflt(call, symbol::DoubleBracket, args, env);
    } else {
        res = do_subset2_dflt(R_NilValue, symbol::DoubleBracket, args, env);
    }
    UNPROTECT(1);
    return res;
}

NativeBuiltin NativeBuiltins::extract21i = {
    "extract2_1Di",
    (void*)&extract21iImpl,
};

SEXP extract21rImpl(SEXP vector, double index, SEXP env, Immediate srcIdx) {
    SEXP res = nullptr;
    if (index < R_XLEN_T_MAX && index >= 1.0)
        res = tryFastVeceltInt(vector, index - 1.0, true);
    if (res)
        return res;

    SEXP args = CONS_NR(vector, CONS_NR(ScalarReal(index), R_NilValue));
    PROTECT(args);
    if (isObject(vector)) {
        SEXP call = src_pool_at(globalContext(), srcIdx);
        res = dispatchApply(call, vector, args, symbol::DoubleBracket, env,
                            globalContext());
        if (!res)
            res = do_subset2_dflt(call, symbol::DoubleBracket, args, env);
    } else {
        res = do_subset2_dflt(R_NilValue, symbol::DoubleBracket, args, env);
    }
    UNPROTECT(1);
    return res;
}

NativeBuiltin NativeBuiltins::extract21r = {
    "extract2_1Dr",
    (void*)&extract21rImpl,
};

SEXP extract12Impl(SEXP vector, SEXP index1, SEXP index2, SEXP env,
                   Immediate srcIdx) {
    SEXP args = CONS_NR(vector, CONS_NR(index1, CONS_NR(index2, R_NilValue)));
    PROTECT(args);
    SEXP res = nullptr;
    if (isObject(vector)) {
        SEXP call = src_pool_at(globalContext(), srcIdx);
        res = dispatchApply(call, vector, args, symbol::Bracket, env,
                            globalContext());
        if (!res)
            res = do_subset_dflt(call, symbol::Bracket, args, env);
    } else {
        res = do_subset_dflt(R_NilValue, symbol::Bracket, args, env);
    }
    UNPROTECT(1);
    return res;
}

NativeBuiltin NativeBuiltins::extract12 = {
    "extract1_2D",
    (void*)&extract12Impl,
};

SEXP extract13Impl(SEXP vector, SEXP index1, SEXP index2, SEXP index3, SEXP env,
                   Immediate srcIdx) {
    SEXP res = nullptr;
    SEXP args = CONS_NR(
        vector, CONS_NR(index1, CONS_NR(index2, CONS_NR(index3, R_NilValue))));
    PROTECT(args);
    if (isObject(vector)) {
        SEXP call = src_pool_at(globalContext(), srcIdx);
        res = dispatchApply(call, vector, args, symbol::Bracket, env,
                            globalContext());
        if (!res)
            res = do_subset_dflt(call, symbol::Bracket, args, env);
    } else {
        res = do_subset_dflt(R_NilValue, symbol::Bracket, args, env);
    }
    UNPROTECT(1);
    return res;
}

NativeBuiltin NativeBuiltins::extract13 = {
    "extract1_3D",
    (void*)&extract13Impl,
};

SEXP extract22Impl(SEXP vector, SEXP index1, SEXP index2, SEXP env,
                   Immediate srcIdx) {
    SEXP args = CONS_NR(vector, CONS_NR(index1, CONS_NR(index2, R_NilValue)));
    PROTECT(args);
    SEXP res = nullptr;
    if (isObject(vector)) {
        SEXP call = src_pool_at(globalContext(), srcIdx);
        res = dispatchApply(call, vector, args, symbol::DoubleBracket, env,
                            globalContext());
        if (!res)
            res = do_subset2_dflt(call, symbol::DoubleBracket, args, env);
    } else {
        res = do_subset2_dflt(R_NilValue, symbol::DoubleBracket, args, env);
    }
    UNPROTECT(1);
    return res;
}

NativeBuiltin NativeBuiltins::extract22 = {
    "extract2_2D",
    (void*)&extract22Impl,
};

SEXP extract22iiImpl(SEXP vector, int index1, int index2, SEXP env,
                     Immediate srcIdx) {

    if (!isObject(vector) && isMatrix(vector) && index1 != NA_INTEGER &&
        index2 != NA_INTEGER && index1 >= 1 && index2 >= 1) {
        auto p1 = (R_xlen_t)(index1 - 1);
        auto p2 = (R_xlen_t)(index2 - 1);

        auto n = getMatrixDim(vector);
        if (p1 < n.row && p2 < n.col) {
            auto pos = n.row * p2 + p1;
            SEXP res = tryFastVeceltInt(vector, pos, true);
            if (res)
                return res;
        }
    }

    SEXP args =
        CONS_NR(vector, CONS_NR(ScalarInteger(index1),
                                CONS_NR(ScalarInteger(index2), R_NilValue)));
    PROTECT(args);
    SEXP res = nullptr;
    if (isObject(vector)) {
        SEXP call = src_pool_at(globalContext(), srcIdx);
        res = dispatchApply(call, vector, args, symbol::DoubleBracket, env,
                            globalContext());
        if (!res)
            res = do_subset2_dflt(call, symbol::DoubleBracket, args, env);
    } else {
        res = do_subset2_dflt(R_NilValue, symbol::DoubleBracket, args, env);
    }
    UNPROTECT(1);
    return res;
}

NativeBuiltin NativeBuiltins::extract22ii = {
    "extract2_2Dii",
    (void*)&extract22iiImpl,
};

SEXP extract22rrImpl(SEXP vector, double index1, double index2, SEXP env,
                     Immediate srcIdx) {

    if (!isObject(vector) && isMatrix(vector) && index1 == index1 &&
        index2 == index2 && index1 >= 1 && index2 >= 1) {
        auto p1 = (R_xlen_t)(index1 - 1);
        auto p2 = (R_xlen_t)(index2 - 1);

        auto n = getMatrixDim(vector);
        if (p1 < n.row && p2 < n.col) {
            auto pos = n.row * p2 + p1;
            SEXP res = tryFastVeceltInt(vector, pos, true);
            if (res)
                return res;
        }
    }

    SEXP args =
        CONS_NR(vector, CONS_NR(ScalarReal(index1),
                                CONS_NR(ScalarReal(index2), R_NilValue)));
    PROTECT(args);
    SEXP res = nullptr;
    if (isObject(vector)) {
        SEXP call = src_pool_at(globalContext(), srcIdx);
        res = dispatchApply(call, vector, args, symbol::DoubleBracket, env,
                            globalContext());
        if (!res)
            res = do_subset2_dflt(call, symbol::DoubleBracket, args, env);
    } else {
        res = do_subset2_dflt(R_NilValue, symbol::DoubleBracket, args, env);
    }
    UNPROTECT(1);
    return res;
}

NativeBuiltin NativeBuiltins::extract22rr = {
    "extract2_2Drr",
    (void*)&extract22rrImpl,
};

static SEXP rirCallTrampoline_(RCNTXT& cntxt, Code* code, R_bcstack_t* args,
                               SEXP env, SEXP callee) {
    if ((SETJMP(cntxt.cjmpbuf))) {
        if (R_ReturnedValue == R_RestartToken) {
            cntxt.callflag = CTXT_RETURN; /* turn restart off */
            R_ReturnedValue = R_NilValue; /* remove restart token */
            code->registerInvocation();
            return code->nativeCode(code, args, env, callee);
        } else {
            return R_ReturnedValue;
        }
    }
    return code->nativeCode(code, args, env, callee);
}

void initClosureContext(SEXP ast, RCNTXT* cntxt, SEXP rho, SEXP sysparent,
                        SEXP arglist, SEXP op) {
    /*  If we have a generic function we need to use the sysparent of
       the generic as the sysparent of the method because the method
       is a straight substitution of the generic.  */

    auto global = (RCNTXT*)R_GlobalContext;
    if (global->callflag == CTXT_GENERIC)
        Rf_begincontext(cntxt, CTXT_RETURN, ast, rho, global->sysparent,
                        arglist, op);
    else
        Rf_begincontext(cntxt, CTXT_RETURN, ast, rho, sysparent, arglist, op);
}

static void endClosureContext(RCNTXT* cntxt, SEXP result) {
    cntxt->returnValue = result;
    Rf_endcontext(cntxt);
}

static SEXP nativeCallTrampolineImpl(SEXP callee, Immediate target,
                                     Immediate astP, SEXP env, size_t nargs,
                                     unsigned long available) {
    SLOWASSERT(env == symbol::delayedEnv || TYPEOF(env) == ENVSXP ||
               env == R_NilValue || LazyEnvironment::check(env));

    auto fun = Function::unpack(Pool::get(target));

    auto ctx = globalContext();
    CallContext call(fun->body(), callee, nargs, astP,
                     ostack_cell_at(ctx, nargs - 1), env, Context(available),
                     ctx);

    auto fail = !call.givenContext.smaller(fun->context());
    if (fail) {
        inferCurrentContext(call, fun->nargs(), ctx);
        fail = !call.givenContext.smaller(fun->context());
    }
    if (!fun->body()->nativeCode)
        fail = true;

    auto dt = DispatchTable::unpack(BODY(callee));

    fun->registerInvocation();
    if (fail || RecompileHeuristic(dt, fun, 6)) {
        if (fail || RecompileCondition(dt, fun, Context(available))) {
            fun->unregisterInvocation();
            return callImplCached(call, target);
        }
    }

    auto t = R_BCNodeStackTop;

    auto missing = fun->nargs() - nargs;
    for (size_t i = 0; i < missing; ++i)
        ostack_push(globalContext(), R_MissingArg);

    R_bcstack_t* args = ostack_cell_at(ctx, nargs + missing - 1);
    auto ast = cp_pool_at(globalContext(), astP);

    ArgsLazyData lazyArgs(nargs, args, nullptr, globalContext());
    SEXP arglist = (SEXP)&lazyArgs;

    assert(fun->signature().envCreation ==
           FunctionSignature::Environment::CalleeCreated);

    RCNTXT cntxt;

    // This code needs to be protected, because its slot in the dispatch table
    // could get overwritten while we are executing it.
    PROTECT(fun->container());

    initClosureContext(ast, &cntxt, symbol::delayedEnv, env, arglist, callee);
    R_Srcref = getAttrib(callee, symbol::srcref);

    // TODO debug

    SEXP result = rirCallTrampoline_(cntxt, fun->body(), args, env, callee);

    endClosureContext(&cntxt, result);

    PROTECT(result);
    R_Srcref = cntxt.srcref;
    R_ReturnedValue = R_NilValue;

    UNPROTECT(2);
    ostack_popn(globalContext(), missing);

    assert(t == R_BCNodeStackTop);
    return result;
}

NativeBuiltin NativeBuiltins::nativeCallTrampoline = {
    "nativeCallTrampoline",
    (void*)&nativeCallTrampolineImpl,
};

SEXP subassign11Impl(SEXP vector, SEXP index, SEXP value, SEXP env,
                     Immediate srcIdx) {
    if (MAYBE_SHARED(vector))
        vector = Rf_shallow_duplicate(vector);
    PROTECT(vector);
    SEXP args = CONS_NR(vector, CONS_NR(index, CONS_NR(value, R_NilValue)));
    SET_TAG(CDDR(args), symbol::value);
    PROTECT(args);
    SEXP res = nullptr;
    SEXP call = src_pool_at(globalContext(), srcIdx);
    RCNTXT assignContext;
    Rf_begincontext(&assignContext, CTXT_RETURN, call, env, ENCLOS(env), args,
                    symbol::AssignBracket);
    if (isObject(vector))
        res = dispatchApply(call, vector, args, symbol::AssignBracket, env,
                            globalContext());
    if (!res) {
        res = do_subassign_dflt(call, symbol::AssignBracket, args, env);
        SET_NAMED(res, 0);
    }
    Rf_endcontext(&assignContext);
    UNPROTECT(2);
    return res;
}

NativeBuiltin NativeBuiltins::subassign11 = {
    "subassign1_1D",
    (void*)subassign11Impl,
};

SEXP subassign21Impl(SEXP vec, SEXP idx, SEXP val, SEXP env, Immediate srcIdx) {
    int prot = 0;
    if (MAYBE_SHARED(vec)) {
        vec = Rf_shallow_duplicate(vec);
        PROTECT(vec);
        prot++;
    }

    if (!isObject(vec) && !ALTREP(vec)) {
        R_xlen_t pos = -1;
        if (IS_SIMPLE_SCALAR(idx, INTSXP)) {
            if (*INTEGER(idx) >= 1 && *INTEGER(idx) != NA_INTEGER)
                pos = *INTEGER(idx) - 1;
        } else if (IS_SIMPLE_SCALAR(idx, REALSXP)) {
            if (*REAL(idx) >= 1 && *REAL(idx) == *REAL(idx))
                pos = *REAL(idx) - 1;
        }
        if (pos != (R_xlen_t)-1) {
            if (IS_SIMPLE_SCALAR(val, INTSXP) && TYPEOF(vec) == INTSXP) {
                if (XLENGTH(vec) > pos ||
                    (XLENGTH(vec) >= pos && XTRUELENGTH(vec) > pos)) {
                    if (XLENGTH(vec) == pos)
                        SETLENGTH(vec, pos + 1);
                    INTEGER(vec)[pos] = *INTEGER(val);
                    UNPROTECT(prot);
                    return vec;
                }
            }
            if (IS_SIMPLE_SCALAR(val, REALSXP) && TYPEOF(vec) == REALSXP) {
                if (XLENGTH(vec) > pos ||
                    (XLENGTH(vec) >= pos && XTRUELENGTH(vec) > pos)) {
                    if (XLENGTH(vec) == pos)
                        SETLENGTH(vec, pos + 1);
                    REAL(vec)[pos] = *REAL(val);
                    UNPROTECT(prot);
                    return vec;
                }
            }
            if (TYPEOF(vec) == VECSXP) {
                if (XLENGTH(vec) > pos ||
                    (XLENGTH(vec) >= pos && XTRUELENGTH(vec) > pos)) {
                    if (XLENGTH(vec) == pos)
                        SETLENGTH(vec, pos + 1);
                    SET_VECTOR_ELT(vec, pos, val);
                    UNPROTECT(prot);
                    return vec;
                }
            }
        }
    }

    SEXP args = CONS_NR(vec, CONS_NR(idx, CONS_NR(val, R_NilValue)));
    SET_TAG(CDDR(args), symbol::value);
    PROTECT(args);
    SEXP res = nullptr;
    SEXP call = src_pool_at(globalContext(), srcIdx);
    RCNTXT assignContext;
    Rf_begincontext(&assignContext, CTXT_RETURN, call, env, ENCLOS(env), args,
                    symbol::AssignDoubleBracket);
    if (isObject(vec))
        res = dispatchApply(call, vec, args, symbol::AssignDoubleBracket, env,
                            globalContext());
    if (!res) {
        res = do_subassign2_dflt(call, symbol::AssignDoubleBracket, args, env);
        SET_NAMED(res, 0);
    }
    Rf_endcontext(&assignContext);
    UNPROTECT(prot + 1);
    return res;
}

NativeBuiltin NativeBuiltins::subassign21 = {
    "subassign2_1D",
    (void*)subassign21Impl,
};

SEXP subassign21rrImpl(SEXP vec, double idx, double val, SEXP env,
                       Immediate srcIdx) {
    int prot = 0;
    if (MAYBE_SHARED(vec)) {
        vec = Rf_shallow_duplicate(vec);
        PROTECT(vec);
        prot++;
    }

    if (!isObject(vec) && idx == idx && !ALTREP(vec)) {
        auto pos = (R_xlen_t)(idx - 1);

        if (TYPEOF(vec) == REALSXP) {
            if (XLENGTH(vec) > pos ||
                (XLENGTH(vec) >= pos && XTRUELENGTH(vec) > pos)) {
                if (XLENGTH(vec) == pos)
                    SETLENGTH(vec, pos + 1);
                REAL(vec)[pos] = val;
                UNPROTECT(prot);
                return vec;
            }
        }
        if (TYPEOF(vec) == VECSXP) {
            if (XLENGTH(vec) > pos ||
                (XLENGTH(vec) >= pos && XTRUELENGTH(vec) > pos)) {
                if (XLENGTH(vec) == pos)
                    SETLENGTH(vec, pos + 1);
                SET_VECTOR_ELT(vec, pos, ScalarReal(val));
                UNPROTECT(prot);
                return vec;
            }
        }
    }

    auto v = PROTECT(ScalarReal(val));
    auto i = PROTECT(ScalarReal(idx));
    auto res = subassign21Impl(vec, i, v, env, srcIdx);
    UNPROTECT(prot + 2);
    return res;
}
SEXP subassign21irImpl(SEXP vec, int idx, double val, SEXP env,
                       Immediate srcIdx) {
    int prot = 0;
    if (MAYBE_SHARED(vec)) {
        vec = Rf_shallow_duplicate(vec);
        PROTECT(vec);
        prot++;
    }

    if (!isObject(vec) && idx != NA_INTEGER && idx >= 1 && !ALTREP(vec)) {
        auto pos = (idx - 1);

        if (TYPEOF(vec) == REALSXP) {
            if (XLENGTH(vec) > pos ||
                (XLENGTH(vec) >= pos && XTRUELENGTH(vec) > pos)) {
                if (XLENGTH(vec) == pos)
                    SETLENGTH(vec, pos + 1);
                REAL(vec)[pos] = val;
                UNPROTECT(prot);
                return vec;
            }
        }
        if (TYPEOF(vec) == VECSXP) {
            if (XLENGTH(vec) > pos ||
                (XLENGTH(vec) >= pos && XTRUELENGTH(vec) > pos)) {
                if (XLENGTH(vec) == pos)
                    SETLENGTH(vec, pos + 1);
                SET_VECTOR_ELT(vec, pos, ScalarReal(val));
                UNPROTECT(prot);
                return vec;
            }
        }
    }

    auto v = PROTECT(ScalarReal(val));
    auto i = PROTECT(ScalarInteger(idx));
    auto res = subassign21Impl(vec, i, v, env, srcIdx);
    UNPROTECT(prot + 2);
    return res;
}
SEXP subassign21riImpl(SEXP vec, double idx, int val, SEXP env,
                       Immediate srcIdx) {
    int prot = 0;
    if (MAYBE_SHARED(vec)) {
        vec = Rf_shallow_duplicate(vec);
        PROTECT(vec);
        prot++;
    }

    if (!isObject(vec) && idx == idx && !ALTREP(vec)) {
        auto pos = (R_xlen_t)(idx - 1);

        if (TYPEOF(vec) == INTSXP || TYPEOF(vec) == LGLSXP) {
            if (XLENGTH(vec) > pos ||
                (XLENGTH(vec) >= pos && XTRUELENGTH(vec) > pos)) {
                if (XLENGTH(vec) == pos)
                    SETLENGTH(vec, pos + 1);
                INTEGER(vec)[pos] = val;
                UNPROTECT(prot);
                return vec;
            }
        }
        if (TYPEOF(vec) == REALSXP) {
            if (XLENGTH(vec) > pos ||
                (XLENGTH(vec) >= pos && XTRUELENGTH(vec) > pos)) {
                if (XLENGTH(vec) == pos)
                    SETLENGTH(vec, pos + 1);
                REAL(vec)[pos] = val == NA_INTEGER ? NAN : val;
                UNPROTECT(prot);
                return vec;
            }
        }
        if (TYPEOF(vec) == VECSXP) {
            if (XLENGTH(vec) > pos ||
                (XLENGTH(vec) >= pos && XTRUELENGTH(vec) > pos)) {
                if (XLENGTH(vec) == pos)
                    SETLENGTH(vec, pos + 1);
                SET_VECTOR_ELT(vec, pos, ScalarInteger(val));
                UNPROTECT(prot);
                return vec;
            }
        }
    }

    auto v = PROTECT(ScalarInteger(val));
    auto i = PROTECT(ScalarReal(idx));
    auto res = subassign21Impl(vec, i, v, env, srcIdx);
    UNPROTECT(prot + 2);
    return res;
}
SEXP subassign21iiImpl(SEXP vec, int idx, int val, SEXP env, Immediate srcIdx) {
    int prot = 0;
    if (MAYBE_SHARED(vec)) {
        vec = Rf_shallow_duplicate(vec);
        PROTECT(vec);
        prot++;
    }

    if (!isObject(vec) && idx != NA_INTEGER && !ALTREP(vec)) {
        auto pos = idx - 1;

        if (TYPEOF(vec) == INTSXP || TYPEOF(vec) == LGLSXP) {
            if (XLENGTH(vec) > pos ||
                (XLENGTH(vec) >= pos && XTRUELENGTH(vec) > pos)) {
                if (XLENGTH(vec) == pos)
                    SETLENGTH(vec, pos + 1);
                INTEGER(vec)[pos] = val;
                UNPROTECT(prot);
                return vec;
            }
        }
        if (TYPEOF(vec) == REALSXP) {
            if (XLENGTH(vec) > pos ||
                (XLENGTH(vec) >= pos && XTRUELENGTH(vec) > pos)) {
                if (XLENGTH(vec) == pos)
                    SETLENGTH(vec, pos + 1);
                REAL(vec)[pos] = val == NA_INTEGER ? NAN : val;
                UNPROTECT(prot);
                return vec;
            }
        }
        if (TYPEOF(vec) == VECSXP) {
            if (XLENGTH(vec) > pos ||
                (XLENGTH(vec) >= pos && XTRUELENGTH(vec) > pos)) {
                if (XLENGTH(vec) == pos)
                    SETLENGTH(vec, pos + 1);
                SET_VECTOR_ELT(vec, pos, ScalarInteger(val));
                UNPROTECT(prot);
                return vec;
            }
        }
    }

    auto v = PROTECT(ScalarInteger(val));
    auto i = PROTECT(ScalarInteger(idx));
    auto res = subassign21Impl(vec, i, v, env, srcIdx);
    UNPROTECT(prot + 2);
    return res;
}

NativeBuiltin NativeBuiltins::subassign21ii = {
    "subassign2_1D_ii",
    (void*)subassign21iiImpl,
};
NativeBuiltin NativeBuiltins::subassign21rr = {
    "subassign2_1D_rr",
    (void*)subassign21rrImpl,
};
NativeBuiltin NativeBuiltins::subassign21ri = {
    "subassign2_1D_ri",
    (void*)subassign21riImpl,
};
NativeBuiltin NativeBuiltins::subassign21ir = {
    "subassign2_1D_ir",
    (void*)subassign21irImpl,
};

SEXP subassign12Impl(SEXP vector, SEXP index1, SEXP index2, SEXP value,
                     SEXP env, Immediate srcIdx) {
    if (MAYBE_SHARED(vector))
        vector = Rf_shallow_duplicate(vector);
    PROTECT(vector);
    SEXP args = CONS_NR(
        vector, CONS_NR(index1, CONS_NR(index2, CONS_NR(value, R_NilValue))));
    SET_TAG(CDDDR(args), symbol::value);
    PROTECT(args);
    SEXP res = nullptr;
    SEXP call = src_pool_at(globalContext(), srcIdx);
    RCNTXT assignContext;
    Rf_begincontext(&assignContext, CTXT_RETURN, call, env, ENCLOS(env), args,
                    symbol::AssignBracket);
    if (isObject(vector))
        res = dispatchApply(call, vector, args, symbol::AssignBracket, env,
                            globalContext());
    if (!res) {
        res = do_subassign_dflt(call, symbol::AssignBracket, args, env);
        SET_NAMED(res, 0);
    }
    Rf_endcontext(&assignContext);
    UNPROTECT(2);
    return res;
}

NativeBuiltin NativeBuiltins::subassign12 = {
    "subassign1_22",
    (void*)subassign12Impl,
};

SEXP subassign13Impl(SEXP vector, SEXP index1, SEXP index2, SEXP index3,
                     SEXP value, SEXP env, Immediate srcIdx) {
    if (MAYBE_SHARED(vector))
        vector = Rf_shallow_duplicate(vector);
    PROTECT(vector);
    SEXP args = CONS_NR(
        vector,
        CONS_NR(index1,
                CONS_NR(index2, CONS_NR(index3, CONS_NR(value, R_NilValue)))));
    SET_TAG(CDDDR(args), symbol::value);
    PROTECT(args);
    SEXP res = nullptr;
    SEXP call = src_pool_at(globalContext(), srcIdx);
    RCNTXT assignContext;
    Rf_begincontext(&assignContext, CTXT_RETURN, call, env, ENCLOS(env), args,
                    symbol::AssignBracket);
    if (isObject(vector))
        res = dispatchApply(call, vector, args, symbol::AssignBracket, env,
                            globalContext());
    if (!res) {
        res = do_subassign_dflt(call, symbol::AssignBracket, args, env);
        SET_NAMED(res, 0);
    }
    Rf_endcontext(&assignContext);
    UNPROTECT(2);
    return res;
}

NativeBuiltin NativeBuiltins::subassign13 = {
    "subassign1_3D",
    (void*)subassign13Impl,
};

SEXP subassign22Impl(SEXP vec, SEXP idx1, SEXP idx2, SEXP val, SEXP env,
                     Immediate srcIdx) {
    int prot = 0;
    if (MAYBE_SHARED(vec)) {
        vec = Rf_shallow_duplicate(vec);
        PROTECT(vec);
        prot++;
    }

    if (!isObject(vec) && isMatrix(vec)) {
        R_xlen_t pos1 = -1;
        R_xlen_t pos2 = -1;
        if (IS_SIMPLE_SCALAR(idx1, INTSXP)) {
            if (*INTEGER(idx1) >= 1 && *INTEGER(idx1) != NA_INTEGER)
                pos1 = *INTEGER(idx1) - 1;
        } else if (IS_SIMPLE_SCALAR(idx1, REALSXP)) {
            if (*REAL(idx1) >= 1 && *REAL(idx1) == *REAL(idx1))
                pos1 = *REAL(idx1) - 1;
        }
        if (IS_SIMPLE_SCALAR(idx2, INTSXP)) {
            if (*INTEGER(idx2) >= 1 && *INTEGER(idx2) != NA_INTEGER)
                pos2 = *INTEGER(idx2) - 1;
        } else if (IS_SIMPLE_SCALAR(idx2, REALSXP)) {
            if (*REAL(idx2) >= 1 && *REAL(idx2) == *REAL(idx2))
                pos2 = *REAL(idx2) - 1;
        }
        if (pos1 != (R_xlen_t)-1 && pos2 != (R_xlen_t)-1) {
            auto n = getMatrixDim(vec);
            if (IS_SIMPLE_SCALAR(val, INTSXP) && TYPEOF(vec) == INTSXP) {
                if (pos1 < n.row && pos2 < n.col) {
                    INTEGER(vec)[n.row * pos2 + pos1] = *INTEGER(val);
                    UNPROTECT(prot);
                    return vec;
                }
            }
            if (IS_SIMPLE_SCALAR(val, REALSXP) && TYPEOF(vec) == REALSXP) {
                if (pos1 < n.row && pos2 < n.col) {
                    REAL(vec)[n.row * pos2 + pos1] = *REAL(val);
                    UNPROTECT(prot);
                    return vec;
                }
            }
            if (TYPEOF(vec) == VECSXP) {
                if (pos1 < n.row && pos2 < n.col) {
                    SET_VECTOR_ELT(vec, n.row * pos2 + pos1, val);
                    UNPROTECT(prot);
                    return vec;
                }
            }
        }
    }

    SEXP args =
        CONS_NR(vec, CONS_NR(idx1, CONS_NR(idx2, CONS_NR(val, R_NilValue))));
    SET_TAG(CDDDR(args), symbol::value);
    PROTECT(args);
    SEXP res = nullptr;
    SEXP call = src_pool_at(globalContext(), srcIdx);
    RCNTXT assignContext;
    Rf_begincontext(&assignContext, CTXT_RETURN, call, env, ENCLOS(env), args,
                    symbol::AssignDoubleBracket);
    if (isObject(vec))
        res = dispatchApply(call, vec, args, symbol::AssignDoubleBracket, env,
                            globalContext());
    if (!res) {
        res = do_subassign2_dflt(call, symbol::AssignDoubleBracket, args, env);
        SET_NAMED(res, 0);
    }
    Rf_endcontext(&assignContext);
    UNPROTECT(prot + 1);
    return res;
}

SEXP subassign22rrrImpl(SEXP vec, double idx1, double idx2, double val,
                        SEXP env, Immediate srcIdx) {
    int prot = 0;
    if (MAYBE_SHARED(vec)) {
        vec = Rf_shallow_duplicate(vec);
        PROTECT(vec);
        prot++;
    }

    if (!isObject(vec) && isMatrix(vec) && idx1 == idx1 && idx2 == idx2 &&
        idx1 >= 1 && idx2 >= 1) {
        R_xlen_t pos1 = idx1 - 1;
        R_xlen_t pos2 = idx2 - 1;
        auto n = getMatrixDim(vec);
        if (TYPEOF(vec) == REALSXP) {
            if (pos1 < n.row && pos2 < n.col) {
                REAL(vec)[n.row * pos2 + pos1] = val;
                UNPROTECT(prot);
                return vec;
            }
        }
        if (TYPEOF(vec) == VECSXP) {
            if (pos1 < n.row && pos2 < n.col) {
                SET_VECTOR_ELT(vec, n.row * pos2 + pos1, ScalarReal(val));
                UNPROTECT(prot);
                return vec;
            }
        }
    }

    auto i1 = PROTECT(ScalarReal(idx1));
    auto i2 = PROTECT(ScalarReal(idx2));
    auto v = PROTECT(ScalarReal(val));
    prot += 3;
    SEXP args = CONS_NR(vec, CONS_NR(i1, CONS_NR(i2, CONS_NR(v, R_NilValue))));
    SET_TAG(CDDDR(args), symbol::value);
    PROTECT(args);
    SEXP res = nullptr;
    SEXP call = src_pool_at(globalContext(), srcIdx);
    RCNTXT assignContext;
    Rf_begincontext(&assignContext, CTXT_RETURN, call, env, ENCLOS(env), args,
                    symbol::AssignDoubleBracket);
    if (isObject(vec))
        res = dispatchApply(call, vec, args, symbol::AssignDoubleBracket, env,
                            globalContext());
    if (!res) {
        res = do_subassign2_dflt(call, symbol::AssignDoubleBracket, args, env);
        SET_NAMED(res, 0);
    }
    Rf_endcontext(&assignContext);
    UNPROTECT(prot + 1);
    return res;
}

SEXP subassign22iirImpl(SEXP vec, int idx1, int idx2, double val, SEXP env,
                        Immediate srcIdx) {
    int prot = 0;
    if (MAYBE_SHARED(vec)) {
        vec = Rf_shallow_duplicate(vec);
        PROTECT(vec);
        prot++;
    }

    if (!isObject(vec) && isMatrix(vec) && idx1 != NA_INTEGER &&
        idx2 != NA_INTEGER && idx1 >= 1 && idx2 >= 1) {
        R_xlen_t pos1 = idx1 - 1;
        R_xlen_t pos2 = idx2 - 1;
        auto n = getMatrixDim(vec);
        if (TYPEOF(vec) == REALSXP) {
            if (pos1 < n.row && pos2 < n.col) {
                REAL(vec)[n.row * pos2 + pos1] = val;
                UNPROTECT(prot);
                return vec;
            }
        }
        if (TYPEOF(vec) == VECSXP) {
            if (pos1 < n.row && pos2 < n.col) {
                SET_VECTOR_ELT(vec, n.row * pos2 + pos1, ScalarReal(val));
                UNPROTECT(prot);
                return vec;
            }
        }
    }

    auto i1 = PROTECT(ScalarInteger(idx1));
    auto i2 = PROTECT(ScalarInteger(idx2));
    auto v = PROTECT(ScalarReal(val));
    prot += 3;
    SEXP args = CONS_NR(vec, CONS_NR(i1, CONS_NR(i2, CONS_NR(v, R_NilValue))));
    SET_TAG(CDDDR(args), symbol::value);
    PROTECT(args);
    SEXP res = nullptr;
    SEXP call = src_pool_at(globalContext(), srcIdx);
    RCNTXT assignContext;
    Rf_begincontext(&assignContext, CTXT_RETURN, call, env, ENCLOS(env), args,
                    symbol::AssignDoubleBracket);
    if (isObject(vec))
        res = dispatchApply(call, vec, args, symbol::AssignDoubleBracket, env,
                            globalContext());
    if (!res) {
        res = do_subassign2_dflt(call, symbol::AssignDoubleBracket, args, env);
        SET_NAMED(res, 0);
    }
    Rf_endcontext(&assignContext);
    UNPROTECT(prot + 1);
    return res;
}

SEXP subassign22iiiImpl(SEXP vec, int idx1, int idx2, int val, SEXP env,
                        Immediate srcIdx) {
    int prot = 0;
    if (MAYBE_SHARED(vec)) {
        vec = Rf_shallow_duplicate(vec);
        PROTECT(vec);
        prot++;
    }

    if (!isObject(vec) && isMatrix(vec) && idx1 != NA_INTEGER &&
        idx2 != NA_INTEGER && idx1 >= 1 && idx2 >= 1) {
        R_xlen_t pos1 = idx1 - 1;
        R_xlen_t pos2 = idx2 - 1;
        auto n = getMatrixDim(vec);
        if (TYPEOF(vec) == INTSXP) {
            if (pos1 < n.row && pos2 < n.col) {
                INTEGER(vec)[n.row * pos2 + pos1] = val;
                UNPROTECT(prot);
                return vec;
            }
        }
        if (TYPEOF(vec) == REALSXP) {
            if (pos1 < n.row && pos2 < n.col) {
                REAL(vec)[n.row * pos2 + pos1] = val == NA_INTEGER ? NAN : val;
                UNPROTECT(prot);
                return vec;
            }
        }
        if (TYPEOF(vec) == VECSXP) {
            if (pos1 < n.row && pos2 < n.col) {
                SET_VECTOR_ELT(vec, n.row * pos2 + pos1, ScalarInteger(val));
                UNPROTECT(prot);
                return vec;
            }
        }
    }

    auto i1 = PROTECT(ScalarInteger(idx1));
    auto i2 = PROTECT(ScalarInteger(idx2));
    auto v = PROTECT(ScalarInteger(val));
    prot += 3;
    SEXP args = CONS_NR(vec, CONS_NR(i1, CONS_NR(i2, CONS_NR(v, R_NilValue))));
    SET_TAG(CDDDR(args), symbol::value);
    PROTECT(args);
    SEXP res = nullptr;
    SEXP call = src_pool_at(globalContext(), srcIdx);
    RCNTXT assignContext;
    Rf_begincontext(&assignContext, CTXT_RETURN, call, env, ENCLOS(env), args,
                    symbol::AssignDoubleBracket);
    if (isObject(vec))
        res = dispatchApply(call, vec, args, symbol::AssignDoubleBracket, env,
                            globalContext());
    if (!res) {
        res = do_subassign2_dflt(call, symbol::AssignDoubleBracket, args, env);
        SET_NAMED(res, 0);
    }
    Rf_endcontext(&assignContext);
    UNPROTECT(prot + 1);
    return res;
}

SEXP subassign22rriImpl(SEXP vec, double idx1, double idx2, int val, SEXP env,
                        Immediate srcIdx) {
    int prot = 0;
    if (MAYBE_SHARED(vec)) {
        vec = Rf_shallow_duplicate(vec);
        PROTECT(vec);
        prot++;
    }

    if (!isObject(vec) && isMatrix(vec) && idx1 == idx1 && idx2 == idx2 &&
        idx1 >= 1 && idx2 >= 1) {
        R_xlen_t pos1 = idx1 - 1;
        R_xlen_t pos2 = idx2 - 1;
        auto n = getMatrixDim(vec);

        if (TYPEOF(vec) == INTSXP) {
            if (pos1 < n.row && pos2 < n.col) {
                INTEGER(vec)[n.row * pos2 + pos1] = val;
                UNPROTECT(prot);
                return vec;
            }
        }
        if (TYPEOF(vec) == REALSXP) {
            if (pos1 < n.row && pos2 < n.col) {
                REAL(vec)[n.row * pos2 + pos1] = val == NA_INTEGER ? NAN : val;
                UNPROTECT(prot);
                return vec;
            }
        }
        if (TYPEOF(vec) == VECSXP) {
            if (pos1 < n.row && pos2 < n.col) {
                SET_VECTOR_ELT(vec, n.row * pos2 + pos1, ScalarInteger(val));
                UNPROTECT(prot);
                return vec;
            }
        }
    }

    auto i1 = PROTECT(ScalarReal(idx1));
    auto i2 = PROTECT(ScalarReal(idx2));
    auto v = PROTECT(ScalarInteger(val));
    prot += 3;
    SEXP args = CONS_NR(vec, CONS_NR(i1, CONS_NR(i2, CONS_NR(v, R_NilValue))));
    SET_TAG(CDDDR(args), symbol::value);
    PROTECT(args);
    SEXP res = nullptr;
    SEXP call = src_pool_at(globalContext(), srcIdx);
    RCNTXT assignContext;
    Rf_begincontext(&assignContext, CTXT_RETURN, call, env, ENCLOS(env), args,
                    symbol::AssignDoubleBracket);
    if (isObject(vec))
        res = dispatchApply(call, vec, args, symbol::AssignDoubleBracket, env,
                            globalContext());
    if (!res) {
        res = do_subassign2_dflt(call, symbol::AssignDoubleBracket, args, env);
        SET_NAMED(res, 0);
    }
    Rf_endcontext(&assignContext);
    UNPROTECT(prot + 1);
    return res;
}

NativeBuiltin NativeBuiltins::subassign22 = {
    "subassign2_2D",
    (void*)subassign22Impl,
};
NativeBuiltin NativeBuiltins::subassign22iii = {
    "subassign2_2Diii",
    (void*)subassign22iiiImpl,
};
NativeBuiltin NativeBuiltins::subassign22rrr = {
    "subassign2_2Drrr",
    (void*)subassign22rrrImpl,
};
NativeBuiltin NativeBuiltins::subassign22rri = {
    "subassign2_2Drr1",
    (void*)subassign22rriImpl,
};
NativeBuiltin NativeBuiltins::subassign22iir = {
    "subassign2_2Diir",
    (void*)subassign22iirImpl,
};

int forSeqSizeImpl(SEXP seq) {
    // TODO: we should extract the length just once at the begining of
    // the loop and generally have somthing more clever here...
    int res;
    if (Rf_isVector(seq)) {
        res = LENGTH(seq);
    } else if (Rf_isList(seq) || isNull(seq)) {
        res = Rf_length(seq);
    } else {
        Rf_errorcall(R_NilValue, "invalid for() loop sequence");
    }
    // TODO: Even when the for loop sequence is an object, R won't
    // dispatch on it. Since in RIR we use the normals extract2_1
    // BC on it, we would. To prevent this we strip the object
    // flag here. What we should do instead, is use a non-dispatching
    // extract BC.
    if (isObject(seq)) {
        seq = Rf_shallow_duplicate(seq);
        SET_OBJECT(seq, 0);
        ostack_set(ctx, 0, seq);
    }
    return res;
}

NativeBuiltin NativeBuiltins::forSeqSize = {"forSeqSize",
                                            (void*)&forSeqSizeImpl};

void initClosureContextImpl(SEXP ast, RCNTXT* cntxt, SEXP sysparent, SEXP op) {
    auto global = (RCNTXT*)R_GlobalContext;
    if (global->callflag == CTXT_GENERIC)
        Rf_begincontext(cntxt, CTXT_RETURN, ast, symbol::delayedEnv,
                        global->sysparent, symbol::delayedArglist, op);
    else
        Rf_begincontext(cntxt, CTXT_RETURN, ast, symbol::delayedEnv, sysparent,
                        symbol::delayedArglist, op);
}

NativeBuiltin NativeBuiltins::initClosureContext = {
    "initClosureContext",
    (void*)&initClosureContextImpl,
};

static void endClosureContextImpl(RCNTXT* cntxt, SEXP result) {
    cntxt->returnValue = result;
    Rf_endcontext(cntxt);
}

NativeBuiltin NativeBuiltins::endClosureContext = {
    "endClosureContext",
    (void*)&endClosureContextImpl,
};

int ncolsImpl(SEXP v) { return getMatrixDim(v).col; }
NativeBuiltin NativeBuiltins::matrixNcols = {
    "ncols",
    (void*)ncolsImpl,
    nullptr,
    {llvm::Attribute::ReadOnly, llvm::Attribute::Speculatable}};

int nrowsImpl(SEXP v) { return getMatrixDim(v).row; }
NativeBuiltin NativeBuiltins::matrixNrows = {
    "nrows",
    (void*)nrowsImpl,
    nullptr,
    {llvm::Attribute::ReadOnly, llvm::Attribute::Speculatable}};

SEXP makeVectorImpl(int mode, size_t len) {
    auto s = Rf_allocVector(mode, len);
    if (mode == INTSXP || mode == LGLSXP)
        Memzero(INTEGER(s), len);
    else if (mode == REALSXP)
        Memzero(REAL(s), len);
    else if (mode == CPLXSXP)
        Memzero(COMPLEX(s), len);
    else if (mode == RAWSXP)
        Memzero(RAW(s), len);
    return s;
}
NativeBuiltin NativeBuiltins::makeVector = {
    "makeVector",
    (void*)makeVectorImpl,
};

double prodrImpl(SEXP v) {
    double res = 1;
    auto len = XLENGTH(v);
    if (TYPEOF(v) == REALSXP) {
        for (R_xlen_t i = 0; i < len; ++i) {
            res *= REAL(v)[i];
        }
    } else if (TYPEOF(v) == INTSXP) {
        for (R_xlen_t i = 0; i < len; ++i) {
            res *= INTEGER(v)[i];
        }
    } else {
        assert(false);
    }
    return res;
}
NativeBuiltin NativeBuiltins::prodr = {
    "prodr",
    (void*)prodrImpl,
    nullptr,
    {llvm::Attribute::ReadOnly, llvm::Attribute::Speculatable}};

double sumrImpl(SEXP v) {
    double res = 0;
    auto len = XLENGTH(v);
    if (TYPEOF(v) == REALSXP) {
        for (R_xlen_t i = 0; i < len; ++i) {
            res += REAL(v)[i];
        }
    } else if (TYPEOF(v) == INTSXP) {
        for (R_xlen_t i = 0; i < len; ++i) {
            res += INTEGER(v)[i];
        }
    } else {
        assert(false);
    }
    return res;
}
NativeBuiltin NativeBuiltins::sumr = {
    "sumr",
    (void*)sumrImpl,
    nullptr,
    {llvm::Attribute::ReadOnly, llvm::Attribute::Speculatable}};

NativeBuiltin NativeBuiltins::colonInputEffects = {
    "colonInputEffects",
    (void*)rir::colonInputEffects,
};

NativeBuiltin NativeBuiltins::colonCastLhs = {
    "colonCastLhs",
    (void*)rir::colonCastLhs,
};

NativeBuiltin NativeBuiltins::colonCastRhs = {"colonCastRhs",
                                              (void*)rir::colonCastRhs,
                                              nullptr,
                                              {llvm::Attribute::ReadOnly}};

SEXP namesImpl(SEXP val) { return Rf_getAttrib(val, R_NamesSymbol); }
NativeBuiltin NativeBuiltins::names = {
    "names",
    (void*)&namesImpl,
};

SEXP setNamesImpl(SEXP val, SEXP names) {
    // If names is R_NilValue, setAttrib doesn't return the val but rather
    // R_NilValue, hence we cannot return val directly...
    Rf_setAttrib(val, R_NamesSymbol, names);
    return val;
}
NativeBuiltin NativeBuiltins::setNames = {
    "setNames",
    (void*)&setNamesImpl,
};

SEXP xlength_Impl(SEXP val) {
    SEXP len = Rf_allocVector(INTSXP, 1);
    INTEGER(len)[0] = Rf_xlength(val);
    return len;
}
NativeBuiltin NativeBuiltins::xlength_ = {
    "xlength_",
    (void*)&xlength_Impl,
};

SEXP getAttribImpl(SEXP val, SEXP sym) { return Rf_getAttrib(val, sym); }
NativeBuiltin NativeBuiltins::getAttrb = {
    "getAttrib",
    (void*)&getAttribImpl,
};

void nonLocalReturnImpl(SEXP res, SEXP env) {
    Rf_findcontext(CTXT_BROWSER | CTXT_FUNCTION, env, res);
}

NativeBuiltin NativeBuiltins::nonLocalReturn = {"nonLocalReturn",
                                                (void*)&nonLocalReturnImpl,
                                                nullptr,
                                                {llvm::Attribute::NoReturn}};

bool clsEqImpl(SEXP lhs, SEXP rhs) {
    SLOWASSERT(TYPEOF(lhs) == CLOSXP && TYPEOF(rhs) == CLOSXP);
    return CLOENV(lhs) == CLOENV(rhs) && FORMALS(lhs) == FORMALS(rhs) &&
           BODY_EXPR(lhs) == BODY_EXPR(rhs);
}

NativeBuiltin NativeBuiltins::clsEq = {
    "cksEq",
    (void*)&clsEqImpl,
    nullptr,
    {llvm::Attribute::ReadOnly, llvm::Attribute::Speculatable}};
}
}
