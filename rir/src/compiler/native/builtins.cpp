#include "builtins.h"
#include "R/Funtab.h"
#include "R/Symbols.h"
#include "compiler/parameter.h"
#include "interpreter/cache.h"
#include "interpreter/call_context.h"
#include "interpreter/interp.h"
#include "ir/Deoptimization.h"
#include "utils/Pool.h"

namespace rir {
namespace pir {

static SEXP forcePromiseImpl(SEXP prom) {
    SLOWASSERT(TYPEOF(prom) == PROMSXP);
    return forcePromise(prom);
}
static jit_type_t sxp1[1] = {sxp};
static jit_type_t sxp2[2] = {sxp, sxp};
static jit_type_t sxp3[3] = {sxp, sxp, sxp};
static jit_type_t int1[1] = {jit_type_int};
static jit_type_t double1[1] = {jit_type_float64};

static jit_type_t sxp2_int[3] = {sxp, sxp, jit_type_int};
static jit_type_t sxp2_void[3] = {sxp, sxp, jit_type_void_ptr};
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

static SEXP consNrTaggedImpl(SEXP val, SEXP name, SEXP rest) {
    SEXP res = CONS_NR(val, rest);
    SET_TAG(res, name);
    return res;
}

NativeBuiltin NativeBuiltins::consNrTagged = {
    "consNrTagged", (void*)&consNrTaggedImpl, 3,
    jit_type_create_signature(jit_abi_cdecl, sxp, sxp3, 3, 0),
};

static SEXP consNrTaggedMissingImpl(SEXP name, SEXP rest) {
    SEXP res = CONS_NR(R_MissingArg, rest);
    SET_TAG(res, name);
    SET_MISSING(res, 2);
    return res;
}

NativeBuiltin NativeBuiltins::consNrTaggedMissing = {
    "consNrTaggedMissing", (void*)&consNrTaggedMissingImpl, 2,
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
    "createEnvironment", (void*)createEnvironmentImpl, 3,
    jit_type_create_signature(jit_abi_cdecl, sxp, sxp2_int, 3, 0),
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
                            size_t nargs, InterpreterInstance* ctx) {
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
    SLOWASSERT(env == symbol::delayedEnv || TYPEOF(env) == ENVSXP);
    SLOWASSERT(ctx);
    auto res = builtinCall(call, ctx);
    SLOWASSERT(res);
    return res;
};
static jit_type_t callArgs[6] = {jit_type_void_ptr, jit_type_uint,    sxp, sxp,
                                 jit_type_ulong,    jit_type_void_ptr};
NativeBuiltin NativeBuiltins::callBuiltin = {
    "callBuiltin", (void*)&callBuiltinImpl, 6,
    jit_type_create_signature(jit_abi_cdecl, sxp, callArgs, 6, 0),
};

static SEXP callImpl(rir::Code* c, Immediate ast, SEXP callee, SEXP env,
                     size_t nargs, InterpreterInstance* ctx) {
    CallContext call(c, callee, nargs, ast, ostack_cell_at(ctx, nargs - 1), env,
                     Assumptions(), ctx);
    SLOWASSERT(env == symbol::delayedEnv || TYPEOF(env) == ENVSXP);
    SLOWASSERT(ctx);
    auto res = doCall(call, ctx);
    return res;
};

NativeBuiltin NativeBuiltins::call = {
    "call", (void*)&callImpl, 6,
    jit_type_create_signature(jit_abi_cdecl, sxp, callArgs, 6, 0),
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

#define BINOP_FALLBACK(op)                                                     \
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

static SEXP binopEnvImpl(SEXP lhs, SEXP rhs, SEXP env, Immediate srcIdx,
                         BinopKind kind) {
    SEXP res = nullptr;
    SEXP arglist2 = CONS_NR(rhs, R_NilValue);
    SEXP arglist = CONS_NR(lhs, arglist2);
    SEXP call = src_pool_at(globalContext(), srcIdx);

    PROTECT(arglist);
    switch (kind) {
    case BinopKind::ADD:
        BINOP_FALLBACK("+");
        break;
    case BinopKind::SUB:
        BINOP_FALLBACK("-");
        break;
    case BinopKind::MUL:
        BINOP_FALLBACK("*");
        break;
    case BinopKind::DIV:
        BINOP_FALLBACK("/");
        break;
    case BinopKind::EQ:
        BINOP_FALLBACK("==");
        break;
    case BinopKind::NE:
        BINOP_FALLBACK("!=");
        break;
    case BinopKind::GT:
        BINOP_FALLBACK(">");
        break;
    case BinopKind::GTE:
        BINOP_FALLBACK(">=");
        break;
    case BinopKind::LT:
        BINOP_FALLBACK("<");
        break;
    case BinopKind::LTE:
        BINOP_FALLBACK("<=");
        break;
    case BinopKind::LAND:
        BINOP_FALLBACK("&&");
        break;
    case BinopKind::LOR:
        BINOP_FALLBACK("||");
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

    switch (kind) {
    case BinopKind::ADD:
        BINOP_FALLBACK("+");
        break;
    case BinopKind::SUB:
        BINOP_FALLBACK("-");
        break;
    case BinopKind::MUL:
        BINOP_FALLBACK("*");
        break;
    case BinopKind::DIV:
        BINOP_FALLBACK("/");
        break;
    case BinopKind::EQ:
        BINOP_FALLBACK("==");
        break;
    case BinopKind::NE:
        BINOP_FALLBACK("!=");
        break;
    case BinopKind::GT:
        BINOP_FALLBACK(">");
        break;
    case BinopKind::GTE:
        BINOP_FALLBACK(">=");
        break;
    case BinopKind::LT:
        BINOP_FALLBACK("<");
        break;
    case BinopKind::LTE:
        BINOP_FALLBACK("<=");
        break;
    case BinopKind::LAND:
        BINOP_FALLBACK("&&");
        break;
    case BinopKind::LOR:
        BINOP_FALLBACK("||");
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

NativeBuiltin NativeBuiltins::asLogical = {
    "aslogical", (void*)&asLogicalImpl, 1,
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
                     src_pool_at(globalContext(), c->src), args, nullptr,
                     (Immediate*)nullptr, env, Assumptions(), globalContext());

    deoptFramesWithContext(globalContext(), &call, m, R_NilValue,
                           m->numFrames - 1, stackHeight, true);
    assert(false);
}

static jit_type_t deoptType[4] = {jit_type_void_ptr, sxp, jit_type_void_ptr,
                                  jit_type_void_ptr};
NativeBuiltin NativeBuiltins::deopt = {
    "deopt", (void*)deoptImpl, 4,
    jit_type_create_signature(jit_abi_cdecl, jit_type_void, deoptType, 4, 0),
};

void assertFailImpl(const char* msg) {
    std::cout << "Assertion in jitted code failed: '" << msg << "'\n";
    asm("int3");
}
NativeBuiltin NativeBuiltins::assertFail = {
    "assertFail", (void*)assertFailImpl, 1,
    jit_type_create_signature(jit_abi_cdecl, jit_type_void, ptr1, 1, 0),
};

NativeBuiltin NativeBuiltins::printValue = {
    "printValue", (void*)Rf_PrintValue, 1,
    jit_type_create_signature(jit_abi_cdecl, jit_type_void, sxp1, 1, 0),
};
}
}
