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

namespace rir {
namespace pir {

static SEXP forcePromiseImpl(SEXP prom) {
    SLOWASSERT(TYPEOF(prom) == PROMSXP);
    auto res = forcePromise(prom);
    ENSURE_NAMEDMAX(res);
    return res;
}
NativeBuiltin NativeBuiltins::forcePromise = {"forcePromise",
                                              (void*)&forcePromiseImpl};

NativeBuiltin NativeBuiltins::consNr = {"consNr", (void*)&CONS_NR};

static SEXP createBindingCellImpl(SEXP val, SEXP name, SEXP rest) {
    SEXP res = CONS_NR(val, rest);
    if (val == R_MissingArg)
        SET_MISSING(res, 2);
    SET_TAG(res, name);
    INCREMENT_NAMED(val);
    return res;
}

NativeBuiltin NativeBuiltins::createBindingCell = {
    "createBindingCellImpl", (void*)&createBindingCellImpl};

static SEXP createMissingBindingCellImpl(SEXP val, SEXP name, SEXP rest) {
    SEXP res = CONS_NR(val, rest);
    SET_TAG(res, name);
    SET_MISSING(res, 2);
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

SEXP ldvarImpl(SEXP a, SEXP b) {
    auto res = Rf_findVar(a, b);
    // std::cout << CHAR(PRINTNAME(a)) << "=";
    // Rf_PrintValue(res);
    ENSURE_NAMED(res);
    return res;
};

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
};

NativeBuiltin NativeBuiltins::ldvarCacheMiss = {
    "ldvarCacheMiss",
    (void*)&ldvarCachedImpl,
};

void stvarImpl(SEXP a, SEXP val, SEXP c) { rirDefineVarWrapper(a, val, c); };
NativeBuiltin NativeBuiltins::stvar = {
    "stvar",
    (void*)&stvarImpl,
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
            }
            return;
        }
    }

    rirDefineVarWrapper(sym, val, env);
};
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
    "setCar",
    (void*)&setCarImpl,
};

void externalsxpSetEntryImpl(SEXP x, int i, SEXP y) {
    assert(x->sxpinfo.mark && "Use fastpath setEntry");
    assert((!y->sxpinfo.mark || y->sxpinfo.gcgen < x->sxpinfo.gcgen) &&
           "use fast path setEntry");
    EXTERNALSXP_SET_ENTRY(x, i, y);
}

NativeBuiltin NativeBuiltins::externalsxpSetEntry = {
    "externalsxpSetEntry",
    (void*)&externalsxpSetEntryImpl,
};

void defvarImpl(SEXP var, SEXP value, SEXP env) {
    rirSetVarWrapper(var, value, ENCLOS(env));
};

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
    "findFun",
    (void*)&Rf_findFun,
};

NativeBuiltin NativeBuiltins::chkfun = {
    "chkfun",
    (void*)&chkfunImpl,
};

static void errorImpl() { Rf_error("Some error in compiled code"); };

NativeBuiltin NativeBuiltins::error = {
    "error",
    (void*)&errorImpl,
};

static bool debugPrintCallBuiltinImpl = false;
static SEXP callBuiltinImpl(rir::Code* c, Immediate ast, SEXP callee, SEXP env,
                            size_t nargs) {
    auto ctx = globalContext();
    CallContext call(c, callee, nargs, ast, ostack_cell_at(ctx, nargs - 1), env,
                     Assumptions(), ctx);
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
    SLOWASSERT(env == symbol::delayedEnv || TYPEOF(env) == ENVSXP ||
               LazyEnvironment::check(env) || env == R_NilValue);
    SLOWASSERT(ctx);
    auto res = builtinCall(call, ctx);
    SLOWASSERT(res);
    return res;
};
NativeBuiltin NativeBuiltins::callBuiltin = {
    "callBuiltin",
    (void*)&callBuiltinImpl,
};

static SEXP callImpl(rir::Code* c, Immediate ast, SEXP callee, SEXP env,
                     size_t nargs, unsigned long available) {
    auto ctx = globalContext();
    CallContext call(c, callee, nargs, ast, ostack_cell_at(ctx, nargs - 1), env,
                     Assumptions(available), ctx);
    SLOWASSERT(env == symbol::delayedEnv || TYPEOF(env) == ENVSXP ||
               LazyEnvironment::check(env) || env == R_NilValue);
    SLOWASSERT(ctx);
    auto res = doCall(call, ctx);
    return res;
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
                     names, env, Assumptions(available), ctx);
    SLOWASSERT(env == symbol::delayedEnv || TYPEOF(env) == ENVSXP ||
               LazyEnvironment::check(env) || env == R_NilValue);
    SLOWASSERT(ctx);
    auto res = doCall(call, ctx);
    return res;
};

NativeBuiltin NativeBuiltins::namedCall = {
    "namedCall",
    (void*)&namedCallImpl,
};

static SEXP dotsCallImpl(rir::Code* c, Immediate ast, SEXP callee, SEXP env,
                         size_t nargs, Immediate* names,
                         unsigned long available) {
    auto ctx = globalContext();
    auto given = Assumptions(available);
    auto toPop = nargs;

    if (TYPEOF(callee) != SPECIALSXP) {
        nargs = expandDotDotDotCallArgs(
            ctx, nargs, names, env,
            given.includes(Assumption::StaticallyArgmatched));
        auto namesStore = ostack_at(ctx, nargs);
        if (namesStore == R_NilValue)
            names = nullptr;
        else
            names = (Immediate*)DATAPTR(namesStore);
        toPop = nargs + 1;
    }

    CallContext call(c, callee, nargs, ast, ostack_cell_at(ctx, nargs - 1),
                     names, env, given, ctx);
    SLOWASSERT(env == symbol::delayedEnv || TYPEOF(env) == ENVSXP ||
               LazyEnvironment::check(env) || env == R_NilValue);
    SLOWASSERT(ctx);
    auto res = doCall(call, ctx);
    ostack_popn(ctx, toPop);
    return res;
};

NativeBuiltin NativeBuiltins::dotsCall = {
    "dotsCall",
    (void*)&dotsCallImpl,
};

SEXP createPromiseImpl(rir::Code* c, unsigned id, SEXP env, SEXP value) {
    assert(TYPEOF(value) != PROMSXP);
    PROTECT(value);
    SEXP res = Rf_mkPROMISE(c->getPromise(id)->container(), env);
    UNPROTECT(1);
    ENSURE_NAMEDMAX(value);
    SET_PRVALUE(res, value);
    return res;
}

NativeBuiltin NativeBuiltins::createPromise = {
    "createPromise",
    (void*)&createPromiseImpl,
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

static SEXPREC createFakeSEXP(SEXPTYPE t) {
    SEXPREC res;
    res.attrib = R_NilValue;
    res.gengc_next_node = R_NilValue;
    res.gengc_prev_node = R_NilValue;
    res.sxpinfo.gcgen = 1;
    res.sxpinfo.mark = 1;
    res.sxpinfo.named = 2;
    res.sxpinfo.type = t;
    return res;
}

static SEXPREC createFakeCONS(SEXP cdr) {
    auto res = createFakeSEXP(LISTSXP);
    res.u.listsxp.carval = R_NilValue;
    res.u.listsxp.tagval = R_NilValue;
    res.u.listsxp.cdrval = cdr;
    return res;
}

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
    SEXPREC arglistStruct = createFakeCONS(R_NilValue);
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
    SEXPREC arglistStruct = createFakeCONS(R_NilValue);
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
    SEXPREC arglist2 = createFakeCONS(R_NilValue);
    SEXPREC arglist1 = createFakeCONS(&arglist2);

    arglist1.u.listsxp.carval = lhs;
    arglist2.u.listsxp.carval = rhs;
    SEXP arglist = &arglist1;
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

int asLogicalImpl(SEXP a) { return Rf_asLogical(a); }

NativeBuiltin NativeBuiltins::asLogicalBlt = {"aslogical",
                                              (void*)&asLogicalImpl};

int lengthImpl(SEXP e) { return Rf_length(e); }

NativeBuiltin NativeBuiltins::length = {
    "length",
    (void*)&lengthImpl,
};

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
                     (Immediate*)nullptr, env, Assumptions(), globalContext());

    deoptFramesWithContext(globalContext(), &call, m, R_NilValue,
                           m->numFrames - 1, stackHeight, true);
    assert(false);
}

NativeBuiltin NativeBuiltins::deopt = {
    "deopt",
    (void*)&deoptImpl,
};
NativeBuiltin NativeBuiltins::recordDeopt = {
    "recordDeopt",
    (void*)&recordDeoptReason,
};

void assertFailImpl(const char* msg) {
    std::cout << "Assertion in jitted code failed: '" << msg << "'\n";
    asm("int3");
}
NativeBuiltin NativeBuiltins::assertFail = {"assertFail",
                                            (void*)&assertFailImpl};

void printValueImpl(SEXP v) { Rf_PrintValue(v); }
NativeBuiltin NativeBuiltins::printValue = {
    "printValue",
    (void*)printValueImpl,
};

SEXP extract11Impl(SEXP vector, SEXP index, SEXP env, Immediate srcIdx) {
    SEXP args = CONS_NR(vector, CONS_NR(index, R_NilValue));
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

NativeBuiltin NativeBuiltins::extract11 = {
    "extract1_1D",
    (void*)&extract11Impl,
};

SEXP extract21Impl(SEXP vector, SEXP index, SEXP env, Immediate srcIdx) {
    SEXP args = CONS_NR(vector, CONS_NR(index, R_NilValue));
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

NativeBuiltin NativeBuiltins::extract21 = {
    "extract2_1D",
    (void*)&extract21Impl,
};

static SEXP rirCallTrampoline_(RCNTXT& cntxt, Code* code, R_bcstack_t* args,
                               SEXP env, SEXP callee) {
    code->registerInvocation();
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

    if (R_GlobalContext->callflag == CTXT_GENERIC)
        Rf_begincontext(cntxt, CTXT_RETURN, ast, rho,
                        R_GlobalContext->sysparent, arglist, op);
    else
        Rf_begincontext(cntxt, CTXT_RETURN, ast, rho, sysparent, arglist, op);
}

static void endClosureContext(RCNTXT* cntxt, SEXP result) {
    cntxt->returnValue = result;
    Rf_endcontext(cntxt);
}

static SEXP nativeCallTrampolineImpl(SEXP callee, rir::Function* fun,
                                     Immediate astP, SEXP env, size_t nargs,
                                     unsigned long available) {
    SLOWASSERT(env == symbol::delayedEnv || TYPEOF(env) == ENVSXP ||
               LazyEnvironment::check(env) || env == R_NilValue);

    if (fun->dead || !fun->body()->nativeCode)
        return callImpl(fun->body(), astP, callee, env, nargs, available);

    auto missing = fun->signature().numArguments - nargs;
    for (size_t i = 0; i < missing; ++i)
        ostack_push(globalContext(), R_MissingArg);

    auto t = R_BCNodeStackTop;
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
    assert(t == R_BCNodeStackTop);

    ostack_popn(globalContext(), missing);
    return result;
}

NativeBuiltin NativeBuiltins::nativeCallTrampoline = {
    "nativeCallTrampoline",
    (void*)&nativeCallTrampolineImpl,
};

SEXP subassign11Impl(SEXP vector, SEXP index, SEXP value, SEXP env,
                     Immediate srcIdx) {
    if (MAYBE_SHARED(vector))
        vector = Rf_duplicate(vector);
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
    if (MAYBE_SHARED(vec))
        vec = Rf_duplicate(vec);
    PROTECT(vec);

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
    UNPROTECT(2);
    return res;
}

NativeBuiltin NativeBuiltins::subassign21 = {
    "subassign2_1D",
    (void*)subassign21Impl,
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
        seq = Rf_duplicate(seq);
        SET_OBJECT(seq, 0);
        ostack_set(ctx, 0, seq);
    }
    return res;
}

NativeBuiltin NativeBuiltins::forSeqSize = {"forSeqSize",
                                            (void*)&forSeqSizeImpl};

void initClosureContextImpl(SEXP ast, RCNTXT* cntxt, SEXP sysparent, SEXP op) {
    if (R_GlobalContext->callflag == CTXT_GENERIC)
        Rf_begincontext(cntxt, CTXT_RETURN, ast, symbol::delayedEnv,
                        R_GlobalContext->sysparent, symbol::delayedArglist, op);
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
}
}
