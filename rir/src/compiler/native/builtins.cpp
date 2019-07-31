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
    return forcePromise(prom);
}
static jit_type_t sxp1[1] = {sxp};
static jit_type_t sxp2[2] = {sxp, sxp};
static jit_type_t sxp3[3] = {sxp, sxp, sxp};
static jit_type_t sxp4[4] = {sxp, sxp, sxp, sxp};
static jit_type_t int1[1] = {jit_type_int};
static jit_type_t double1[1] = {jit_type_float64};

static jit_type_t sxp2_int[3] = {sxp, sxp, jit_type_int};
static jit_type_t sxp2_void[3] = {sxp, sxp, jit_type_void_ptr};

static jit_type_t sxp3_int[4] = {sxp, sxp, sxp, jit_type_int};

static jit_type_t sxp3_int2[5] = {sxp, sxp, sxp, jit_type_int, jit_type_int};

static jit_type_t ptr1[1] = {jit_type_void_ptr};

NativeBuiltin NativeBuiltins::forcePromise = {
    "forcePromise", (void*)&forcePromiseImpl, 1,
    jit_type_create_signature(jit_abi_cdecl, sxp, sxp1, 1, 0),
};

NativeBuiltin NativeBuiltins::consNr = {
    "consNr", (void*)&CONS_NR, 2,
    jit_type_create_signature(jit_abi_cdecl, sxp, sxp2, 2, 0),
};

static SEXP createBindingCellImpl(SEXP val, SEXP name, SEXP rest) {
    SEXP res = CONS_NR(val, rest);
    if (val == R_MissingArg)
        SET_MISSING(res, 2);
    SET_TAG(res, name);
    return res;
}

NativeBuiltin NativeBuiltins::createBindingCell = {
    "createBindingCellImpl",
    (void*)&createBindingCellImpl,
    3,
    jit_type_create_signature(jit_abi_cdecl, sxp, sxp3, 3, 0),
};

static SEXP createMissingBindingCellImpl(SEXP name, SEXP rest) {
    SEXP res = CONS_NR(R_MissingArg, rest);
    SET_TAG(res, name);
    SET_MISSING(res, 2);
    return res;
}

NativeBuiltin NativeBuiltins::createMissingBindingCell = {
    "createMissingBindingCell",
    (void*)&createMissingBindingCellImpl,
    2,
    jit_type_create_signature(jit_abi_cdecl, sxp, sxp2, 2, 0),
};

SEXP createEnvironmentImpl(SEXP parent, SEXP arglist, int contextPos) {
    SLOWASSERT(TYPEOF(parent) == ENVSXP);
    SLOWASSERT(TYPEOF(arglist) == LISTSXP || arglist == R_NilValue);
    SEXP res = Rf_NewEnvironment(R_NilValue, arglist, parent);

    if (contextPos > 0) {
        if (auto cptr = getFunctionContext(contextPos - 1)) {
            cptr->cloenv = res;
            if (cptr->promargs == symbol::delayedArglist)
                cptr->promargs = arglist;
        }
    }

    return res;
}

NativeBuiltin NativeBuiltins::createEnvironment = {
    "createEnvironment",
    (void*)&createEnvironmentImpl,
    3,
    jit_type_create_signature(jit_abi_cdecl, sxp, sxp2_int, 3, 0),
};

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

static jit_type_t createStubEnvironmentSignature[4] = {
    sxp, jit_type_sys_int, jit_type_void_ptr, jit_type_sys_int};
NativeBuiltin NativeBuiltins::createStubEnvironment = {
    "createStubEnvironment",
    (void*)&createStubEnvironmentImpl,
    4,
    jit_type_create_signature(jit_abi_cdecl, sxp,
                              createStubEnvironmentSignature, 4, 0),
};

SEXP ldvarImpl(SEXP a, SEXP b) {
    auto res = Rf_findVar(a, b);
    // std::cout << CHAR(PRINTNAME(a)) << "=";
    // Rf_PrintValue(res);
    return res;
};

NativeBuiltin NativeBuiltins::ldvar = {
    "ldvar", (void*)&ldvarImpl, 2,
    jit_type_create_signature(jit_abi_cdecl, sxp, sxp2, 2, 0),
};

SEXP ldvarCachedImpl(SEXP sym, SEXP env, SEXP* cache) {
    if (*cache != (SEXP)1) {
        R_varloc_t loc = R_findVarLocInFrame(env, sym);
        if (R_VARLOC_IS_NULL(loc)) {
            *cache = (SEXP)1;
        } else {
            *cache = loc.cell;
            if (CAR(*cache) != R_UnboundValue)
                return CAR(*cache);
        }
    }
    return Rf_findVar(sym, env);
};

NativeBuiltin NativeBuiltins::ldvarCacheMiss = {
    "ldvarCacheMiss", (void*)&ldvarCachedImpl, 3,
    jit_type_create_signature(jit_abi_cdecl, sxp, sxp2_void, 3, 0),
};

void stvarImpl(SEXP a, SEXP val, SEXP c) { rirDefineVarWrapper(a, val, c); };
NativeBuiltin NativeBuiltins::stvar = {
    "stvar", (void*)&stvarImpl, 3,
    jit_type_create_signature(jit_abi_cdecl, sxp, sxp3, 3, 0),
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

    Rf_defineVar(sym, val, env);
};
NativeBuiltin NativeBuiltins::starg = {
    "starg",
    (void*)&stargImpl,
    3,
    jit_type_create_signature(jit_abi_cdecl, sxp, sxp3, 3, 0),
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
    2,
    jit_type_create_signature(jit_abi_cdecl, sxp, sxp2, 2, 0),
};

void externalsxpSetEntryImpl(SEXP x, int i, SEXP y) {
    assert(x->sxpinfo.mark && "Use fastpath setEntry");
    assert((!y->sxpinfo.mark || y->sxpinfo.gcgen < x->sxpinfo.gcgen) &&
           "use fast path setEntry");
    EXTERNALSXP_SET_ENTRY(x, i, y);
}

static jit_type_t externalsxpSetEntrySignature[4] = {sxp, jit_type_sys_int,
                                                     sxp};
NativeBuiltin NativeBuiltins::externalsxpSetEntry = {
    "externalsxpSetEntry",
    (void*)&externalsxpSetEntryImpl,
    3,
    jit_type_create_signature(jit_abi_cdecl, sxp, externalsxpSetEntrySignature,
                              3, 0),
};

void defvarImpl(SEXP var, SEXP value, SEXP env) {
    rirSetVarWrapper(var, value, ENCLOS(env));
};

NativeBuiltin NativeBuiltins::defvar = {
    "defvar",
    (void*)&defvarImpl,
    3,
    jit_type_create_signature(jit_abi_cdecl, sxp, sxp3, 3, 0),
};

NativeBuiltin NativeBuiltins::ldfun = {
    "Rf_findFun", (void*)&Rf_findFun, 2,
    jit_type_create_signature(jit_abi_cdecl, sxp, sxp2, 2, 0),
};

static void errorImpl() { Rf_error("Some error in compiled code"); };

NativeBuiltin NativeBuiltins::error = {
    "error", (void*)&errorImpl, 0,
    jit_type_create_signature(jit_abi_cdecl, jit_type_void, {}, 0, 0),
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
static jit_type_t callArgs[5] = {jit_type_void_ptr, jit_type_uint, sxp, sxp,
                                 jit_type_ulong};
NativeBuiltin NativeBuiltins::callBuiltin = {
    "callBuiltin",
    (void*)&callBuiltinImpl,
    5,
    jit_type_create_signature(jit_abi_cdecl, sxp, callArgs, 5, 0),
};

static SEXP callImpl(rir::Code* c, Immediate ast, SEXP callee, SEXP env,
                     size_t nargs) {
    auto ctx = globalContext();
    CallContext call(c, callee, nargs, ast, ostack_cell_at(ctx, nargs - 1), env,
                     Assumptions(), ctx);
    SLOWASSERT(env == symbol::delayedEnv || TYPEOF(env) == ENVSXP ||
               LazyEnvironment::check(env) || env == R_NilValue);
    SLOWASSERT(ctx);
    auto res = doCall(call, ctx);
    return res;
};

NativeBuiltin NativeBuiltins::call = {
    "call",
    (void*)&callImpl,
    5,
    jit_type_create_signature(jit_abi_cdecl, sxp, callArgs, 5, 0),
};

SEXP createPromiseImpl(rir::Code* c, unsigned id, SEXP env, SEXP value) {
    SEXP res = Rf_mkPROMISE(c->getPromise(id)->container(), env);
    SET_PRVALUE(res, value);
    return res;
}

static jit_type_t createPromiseArgs[4] = {jit_type_void_ptr, jit_type_uint, sxp,
                                          sxp};
NativeBuiltin NativeBuiltins::createPromise = {
    "createPromise", (void*)&createPromiseImpl, 4,
    jit_type_create_signature(jit_abi_cdecl, sxp, createPromiseArgs, 4, 0),
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
    4,
    jit_type_create_signature(jit_abi_cdecl, sxp, sxp4, 4, 0),
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
    "newIntFromReal", (void*)&newIntFromRealImpl, 1,
    jit_type_create_signature(jit_abi_cdecl, sxp, double1, 1, 0),
};
NativeBuiltin NativeBuiltins::newRealFromInt = {
    "newRealFromInt", (void*)&newRealFromIntImpl, 1,
    jit_type_create_signature(jit_abi_cdecl, sxp, int1, 1, 0),
};
NativeBuiltin NativeBuiltins::newLglFromReal = {
    "newLglFromReal", (void*)&newLglFromRealImpl, 1,
    jit_type_create_signature(jit_abi_cdecl, sxp, double1, 1, 0),
};
NativeBuiltin NativeBuiltins::newInt = {
    "newInt", (void*)&newIntImpl, 1,
    jit_type_create_signature(jit_abi_cdecl, sxp, int1, 1, 0),
};
NativeBuiltin NativeBuiltins::newReal = {
    "newReal", (void*)&newRealImpl, 1,
    jit_type_create_signature(jit_abi_cdecl, sxp, double1, 1, 0),
};
NativeBuiltin NativeBuiltins::newLgl = {
    "newLgl", (void*)&newLglImpl, 1,
    jit_type_create_signature(jit_abi_cdecl, sxp, int1, 1, 0),
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
    3,
    jit_type_create_signature(jit_abi_cdecl, sxp, sxp2_int, 3, 0),
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

NativeBuiltin NativeBuiltins::notOp = {
    "not",
    (void*)&notImpl,
    3,
    jit_type_create_signature(jit_abi_cdecl, sxp, sxp2_int, 3, 0),
};

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
    }
    UNPROTECT(1);
    SLOWASSERT(res);
    return res;
}

NativeBuiltin NativeBuiltins::binopEnv = {
    "binopEnv", (void*)&binopEnvImpl, 5,
    jit_type_create_signature(jit_abi_cdecl, sxp, sxp3_int2, 5, 0),
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
    3,
    jit_type_create_signature(jit_abi_cdecl, sxp, sxp2_int, 3, 0),
};

int isMissingImpl(SEXP symbol, SEXP environment) {
    // TODO: Send the proper src
    return rir::isMissing(symbol, environment, nullptr, nullptr);
}

NativeBuiltin NativeBuiltins::isMissing = {
    "isMissing",
    (void*)&isMissingImpl,
    2,
    jit_type_create_signature(jit_abi_cdecl, jit_type_int, sxp2, 2, 0),
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
    "astest", (void*)&astestImpl, 1,
    jit_type_create_signature(jit_abi_cdecl, jit_type_int, sxp1, 1, 0),
};

int asLogicalImpl(SEXP a) { return Rf_asLogical(a); }

NativeBuiltin NativeBuiltins::asLogicalBlt = {
    "aslogical",
    (void*)&asLogicalImpl,
    1,
    jit_type_create_signature(jit_abi_cdecl, jit_type_int, sxp1, 1, 0),
};

int lengthImpl(SEXP e) { return Rf_length(e); }

NativeBuiltin NativeBuiltins::length = {
    "length", (void*)&lengthImpl, 1,
    jit_type_create_signature(jit_abi_cdecl, jit_type_int, sxp1, 1, 0),
};

void deoptImpl(Code* c, SEXP cls, DeoptMetadata* m, R_bcstack_t* args) {
    if (!pir::Parameter::DEOPT_CHAOS && cls) {
        // TODO: this version is still reachable from static call inline
        // caches. Thus we need to preserve it forever. We need some
        // dependency management here.
        Pool::insert(c->container());
        // remove the deoptimized function. Unless on deopt chaos,
        // always recompiling would just blow testing time...
        auto dt = DispatchTable::unpack(BODY(cls));
        dt->remove(c);
    }
    assert(m->numFrames >= 1);
    size_t stackHeight = 0;
    for (size_t i = 0; i < m->numFrames; ++i) {
        stackHeight += m->frames[i].stackSize + 1;
    }

    SEXP env =
        ostack_at(ctx, stackHeight - m->frames[m->numFrames - 1].stackSize - 1);
    CallContext call(c, cls, /* nargs */ -1,
                     src_pool_at(globalContext(), c->src), args,
                     (Immediate*)nullptr, env, Assumptions(), globalContext());

    deoptFramesWithContext(globalContext(), &call, m, R_NilValue,
                           m->numFrames - 1, stackHeight, true);
    assert(false);
}

static jit_type_t deoptType[4] = {jit_type_void_ptr, sxp, jit_type_void_ptr,
                                  jit_type_void_ptr};
NativeBuiltin NativeBuiltins::deopt = {
    "deopt",
    (void*)&deoptImpl,
    4,
    jit_type_create_signature(jit_abi_cdecl, jit_type_void, deoptType, 4, 0),
};

void assertFailImpl(const char* msg) {
    std::cout << "Assertion in jitted code failed: '" << msg << "'\n";
    asm("int3");
}
NativeBuiltin NativeBuiltins::assertFail = {
    "assertFail",
    (void*)&assertFailImpl,
    1,
    jit_type_create_signature(jit_abi_cdecl, jit_type_void, ptr1, 1, 0),
};

void printValueImpl(SEXP v) { Rf_PrintValue(v); }
NativeBuiltin NativeBuiltins::printValue = {
    "printValue",
    (void*)printValueImpl,
    1,
    jit_type_create_signature(jit_abi_cdecl, jit_type_void, sxp1, 1, 0),
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
            res = do_subset_dflt(R_NilValue, symbol::Bracket, args, env);
    } else {
        res = do_subset_dflt(R_NilValue, symbol::Bracket, args, env);
    }
    UNPROTECT(1);
    return res;
}

NativeBuiltin NativeBuiltins::extract11 = {
    "extract1_1D",
    (void*)&extract11Impl,
    4,
    jit_type_create_signature(jit_abi_cdecl, sxp, sxp3_int, 4, 0),
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
    4,
    jit_type_create_signature(jit_abi_cdecl, sxp, sxp3_int, 4, 0),
};

static SEXP rirCallTrampoline_(RCNTXT& cntxt, Code* code, R_bcstack_t* args,
                               SEXP env, SEXP callee) {
    if ((SETJMP(cntxt.cjmpbuf))) {
        if (R_ReturnedValue == R_RestartToken) {
            cntxt.callflag = CTXT_RETURN; /* turn restart off */
            R_ReturnedValue = R_NilValue; /* remove restart token */
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
                                     Immediate astP, SEXP env, size_t nargs) {
    SLOWASSERT(env == symbol::delayedEnv || TYPEOF(env) == ENVSXP ||
               LazyEnvironment::check(env) || env == R_NilValue);

    auto t = R_BCNodeStackTop;
    R_bcstack_t* args = ostack_cell_at(ctx, nargs - 1);
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
    return result;
}

NativeBuiltin NativeBuiltins::nativeCallTrampoline = {
    "nativeCallTrampoline",
    (void*)&nativeCallTrampolineImpl,
    5,
    nullptr,
};
}
}
