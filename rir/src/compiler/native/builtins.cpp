#include "builtins.h"
#include "R/Symbols.h"
#include "interpreter/call_context.h"
#include "interpreter/interp.h"

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
static jit_type_t double1[1] = {jit_type_sys_double};

static jit_type_t sxp2_int[3] = {sxp, sxp, jit_type_int};

NativeBuiltin NativeBuiltins::forcePromise = {
    "forcePromise", (void*)&forcePromiseImpl, 1,
    jit_type_create_signature(jit_abi_cdecl, sxp, sxp2, 1, 0),
};

NativeBuiltin NativeBuiltins::consNr = {
    "consNr", (void*)&CONS_NR, 2,
    jit_type_create_signature(jit_abi_cdecl, sxp, sxp1, 2, 0),
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

NativeBuiltin NativeBuiltins::ldvar = {
    "Rf_findVar", (void*)&Rf_findVar, 2,
    jit_type_create_signature(jit_abi_cdecl, sxp, sxp2, 2, 0),
};

NativeBuiltin NativeBuiltins::stvar = {
    "Rf_defineVar", (void*)&Rf_defineVar, 3,
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
    SLOWASSERT(!env || TYPEOF(env) == ENVSXP);
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
    SLOWASSERT(!env || TYPEOF(env) == ENVSXP);
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

SEXP newIntImpl(int i) {
    auto res = Rf_allocVector(INTSXP, 1);
    INTEGER(res)[0] = i;
    return res;
}

SEXP newRealImpl(double i) {
    auto res = Rf_allocVector(REALSXP, 1);
    REAL(res)[0] = i;
    return res;
}

NativeBuiltin NativeBuiltins::newInt = {
    "newInt", (void*)&newIntImpl, 1,
    jit_type_create_signature(jit_abi_cdecl, sxp, int1, 1, 0),
};
NativeBuiltin NativeBuiltins::newReal = {
    "newReal", (void*)&newRealImpl, 1,
    jit_type_create_signature(jit_abi_cdecl, sxp, double1, 1, 0),
};
}
}
