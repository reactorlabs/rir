#include "interp.h"
#include "ArgsLazyData.h"
#include "LazyEnvironment.h"
#include "R/Funtab.h"
#include "R/RList.h"
#include "R/Symbols.h"
#include "api.h"
#include "compiler/parameter.h"
#include "compiler/translations/rir_2_pir/rir_2_pir_compiler.h"
#include "ir/Deoptimization.h"
#include "runtime/TypeFeedback_inl.h"
#include "safe_force.h"
#include "utils/Pool.h"

#include <assert.h>
#include <deque>
#include <set>

#define NOT_IMPLEMENTED assert(false)

#undef eval

extern "C" {
extern SEXP Rf_NewEnvironment(SEXP, SEXP, SEXP);
extern Rboolean R_Visible;
}

namespace rir {

static RIR_INLINE SEXP getSrcAt(Code* c, Opcode* pc, InterpreterInstance* ctx) {
    unsigned sidx = c->getSrcIdxAt(pc, true);
    if (sidx == 0)
        return src_pool_at(ctx, c->src);
    return src_pool_at(ctx, sidx);
}

static RIR_INLINE SEXP getSrcForCall(Code* c, Opcode* pc,
                                     InterpreterInstance* ctx) {
    unsigned sidx = c->getSrcIdxAt(pc, false);
    return src_pool_at(ctx, sidx);
}

#define PC_BOUNDSCHECK(pc, c)                                                  \
    SLOWASSERT((pc) >= (c)->code() && (pc) < (c)->endCode());

#ifdef THREADED_CODE
#define BEGIN_MACHINE NEXT();
#ifdef ENABLE_MEASURE
#define INSTRUCTION(name)                                                      \
    op_##name : /* debug(c, pc, #name, ostack_length(ctx) - bp, ctx); */       \
                GlobalMeasureTable.record(Opcode::name, c);
#else
#define INSTRUCTION(name)                                                      \
    op_##name: /* debug(c, pc, #name, ostack_length(ctx) - bp, ctx); */
#endif
#define NEXT()                                                                 \
    (__extension__({ goto* opAddr[static_cast<uint8_t>(advanceOpcode())]; }))
#define LASTOP                                                                 \
    {}
#else
#define BEGIN_MACHINE                                                          \
    loop:                                                                      \
    switch (advanceOpcode())
#define INSTRUCTION(name)                                                      \
    case Opcode::name:                                                         \
        /* debug(c, pc, #name, ostack_length(ctx) - bp, ctx); */
#define NEXT() goto loop
#define LASTOP                                                                 \
    default:                                                                   \
        assert(false && "wrong or unimplemented opcode")
#endif

// bytecode accesses
#define advanceOpcode() (*(pc++))
#define readImmediate() (*(Immediate*)pc)
#define readSignedImmediate() (*(SignedImmediate*)pc)
#define readJumpOffset() (*(JumpOffset*)(pc))
#define advanceImmediate() pc += sizeof(Immediate)
#define advanceImmediateN(n) pc += n * sizeof(Immediate)
#define advanceJump() pc += sizeof(JumpOffset)

#define readConst(ctx, idx) (cp_pool_at(ctx, idx))

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

static RIR_INLINE SEXP createPromise(Code* code, SEXP env) {
    SEXP p = Rf_mkPROMISE(code->container(), env);
    return p;
}

static RIR_INLINE SEXP promiseValue(SEXP promise, InterpreterInstance* ctx) {
    // if already evaluated, return the value
    if (PRVALUE(promise) && PRVALUE(promise) != R_UnboundValue) {
        promise = PRVALUE(promise);
        assert(TYPEOF(promise) != PROMSXP);
        return promise;
    } else {
        SEXP res = forcePromise(promise);
        assert(TYPEOF(res) != PROMSXP && "promise returned promise");
        return res;
    }
}

static void jit(SEXP cls, SEXP name, InterpreterInstance* ctx) {
    assert(TYPEOF(cls) == CLOSXP);
    if (TYPEOF(BODY(cls)) == EXTERNALSXP)
        return;
    SEXP cmp = ctx->closureCompiler(cls, name);
    SET_BODY(cls, BODY(cmp));
}

static void closureDebug(SEXP call, SEXP op, SEXP rho, SEXP newrho,
                         RCNTXT* cntxt) {
    // TODO!!!
}

static void endClosureDebug(SEXP call, SEXP op, SEXP rho) {
    // TODO!!!
}

/** Given argument code offsets, creates the argslist from their promises.
 */
// TODO unnamed only at this point
static RIR_INLINE void __listAppend(SEXP* front, SEXP* last, SEXP value,
                                    SEXP name) {
    SLOWASSERT(TYPEOF(*front) == LISTSXP || TYPEOF(*front) == NILSXP);
    SLOWASSERT(TYPEOF(*last) == LISTSXP || TYPEOF(*last) == NILSXP);

    SEXP app = CONS_NR(value, R_NilValue);

    SET_TAG(app, name);

    if (*front == R_NilValue) {
        *front = app;
        PROTECT(*front);
    }

    if (*last != R_NilValue)
        SETCDR(*last, app);
    *last = app;
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-align"

SEXP createEnvironment(std::vector<SEXP>* args, const SEXP parent,
                       const Opcode* pc, InterpreterInstance* ctx,
                       R_bcstack_t* localsBase, SEXP stub) {
    SEXP arglist = R_NilValue;
    auto names = (Immediate*)pc;
    int j = 0;
    for (long i = args->size() - 1; i >= 0; --i) {
        SEXP val = args->at(j);
        SEXP name = cp_pool_at(ctx, names[i]);
        arglist = CONS_NR(val, arglist);
        SET_TAG(arglist, name);
        SET_MISSING(arglist, val == R_MissingArg ? 2 : 0);
        j++;
    }

    SEXP environment = Rf_NewEnvironment(R_NilValue, arglist, parent);
    for (auto i = 0; i < R_BCNodeStackTop - localsBase; i++) {
        if (ostack_at(ctx, i) == stub)
            ostack_set(ctx, i, environment);
    }
    return environment;
}

SEXP createLegacyArgsListFromStackValues(const CallContext& call,
                                         bool eagerCallee,
                                         InterpreterInstance* ctx) {
    SEXP result = R_NilValue;
    SEXP pos = result;

    for (size_t i = 0; i < call.suppliedArgs; ++i) {

        SEXP name = call.hasNames() ? call.name(i, ctx) : R_NilValue;

        SEXP arg = call.stackArg(i);

        if (eagerCallee && TYPEOF(arg) == PROMSXP) {
            arg = Rf_eval(arg, call.callerEnv);
        }
        __listAppend(&result, &pos, arg, name);
    }

    if (result != R_NilValue)
        UNPROTECT(1);

    return result;
}

static SEXP createLegacyArgsList(const CallContext& call, bool eagerCallee,
                                 InterpreterInstance* ctx) {
    SEXP result = R_NilValue;
    SEXP pos = result;

    // loop through the arguments and create a promise, unless it is a missing
    // argument
    for (size_t i = 0; i < call.suppliedArgs; ++i) {
        unsigned argi = call.implicitArgIdx(i);
        SEXP name = call.hasNames() ? call.name(i, ctx) : R_NilValue;

        // if the argument is an ellipsis, then retrieve it from the environment
        // and
        // flatten the ellipsis
        if (argi == DOTS_ARG_IDX) {
            SEXP ellipsis = Rf_findVar(R_DotsSymbol, call.callerEnv);
            if (TYPEOF(ellipsis) == DOTSXP) {
                while (ellipsis != R_NilValue) {
                    name = TAG(ellipsis);
                    if (eagerCallee) {
                        SEXP arg = CAR(ellipsis);
                        if (arg != R_MissingArg)
                            arg = Rf_eval(CAR(ellipsis), call.callerEnv);
                        assert(TYPEOF(arg) != PROMSXP);
                        __listAppend(&result, &pos, arg, name);
                    } else {
                        SEXP promise =
                            Rf_mkPROMISE(CAR(ellipsis), call.callerEnv);
                        __listAppend(&result, &pos, promise, name);
                    }
                    ellipsis = CDR(ellipsis);
                }
            }
        } else if (argi == MISSING_ARG_IDX) {
            if (eagerCallee)
                Rf_errorcall(call.ast, "argument %d is empty", i + 1);
            __listAppend(&result, &pos, R_MissingArg, R_NilValue);
        } else {
            if (eagerCallee) {
                SEXP arg = evalRirCodeExtCaller(call.implicitArg(i), ctx,
                                                call.callerEnv);
                assert(TYPEOF(arg) != PROMSXP);
                __listAppend(&result, &pos, arg, name);
            } else {
                Code* arg = call.implicitArg(i);
                SEXP promise = createPromise(arg, call.callerEnv);
                __listAppend(&result, &pos, promise, name);
            }
        }
    }

    if (result != R_NilValue)
        UNPROTECT(1);
    return result;
}

SEXP materialize(void* rirDataWrapper) {
    if (auto promargs = ArgsLazyData::cast(rirDataWrapper)) {
        return promargs->createArgsLists();
    } else if (auto stub = LazyEnvironment::cast(rirDataWrapper)) {
        return stub->create();
    }
    assert(false);
    return nullptr;
}

SEXP* keepAliveSEXPs(void* rirDataWrapper) {
    if (auto env = LazyEnvironment::cast(rirDataWrapper)) {
        return env->gcData();
    }
    assert(false);
    return nullptr;
}

SEXP lazyPromargsCreation(void* rirDataWrapper) {
    return ArgsLazyData::cast(rirDataWrapper)->createArgsLists();
}

static RIR_INLINE SEXP createLegacyLazyArgsList(const CallContext& call,
                                                InterpreterInstance* ctx) {
    if (call.hasStackArgs()) {
        return createLegacyArgsListFromStackValues(call, false, ctx);
    } else {
        return createLegacyArgsList(call, false, ctx);
    }
}

static RIR_INLINE SEXP createLegacyArgsList(const CallContext& call,
                                            InterpreterInstance* ctx) {
    if (call.hasStackArgs()) {
        return createLegacyArgsListFromStackValues(call, call.hasEagerCallee(),
                                                   ctx);
    } else {
        return createLegacyArgsList(call, call.hasEagerCallee(), ctx);
    }
}

SEXP evalRirCode(Code*, InterpreterInstance*, SEXP, const CallContext*, Opcode*,
                 R_bcstack_t* = nullptr);
static SEXP rirCallTrampoline_(RCNTXT& cntxt, const CallContext& call,
                               Code* code, SEXP env, InterpreterInstance* ctx) {
    if ((SETJMP(cntxt.cjmpbuf))) {
        if (R_ReturnedValue == R_RestartToken) {
            cntxt.callflag = CTXT_RETURN; /* turn restart off */
            R_ReturnedValue = R_NilValue; /* remove restart token */
            return evalRirCode(code, ctx, cntxt.cloenv, &call);
        } else {
            return R_ReturnedValue;
        }
    }
    return evalRirCode(code, ctx, env, &call);
}

static RIR_INLINE SEXP rirCallTrampoline(const CallContext& call, Function* fun,
                                         SEXP env, SEXP arglist,
                                         InterpreterInstance* ctx) {
    assert(TYPEOF(env) == ENVSXP ||
           fun->signature().envCreation ==
               FunctionSignature::Environment::CalleeCreated);

    RCNTXT cntxt;

    // This code needs to be protected, because its slot in the dispatch table
    // could get overwritten while we are executing it.
    PROTECT(fun->container());

    initClosureContext(call.ast, &cntxt, env, call.callerEnv, arglist,
                       call.callee);
    R_Srcref = getAttrib(call.callee, symbol::srcref);

    closureDebug(call.ast, call.callee, env, R_NilValue, &cntxt);

    // Warning: call.popArgs() between initClosureContext and trampoline will
    // result in broken stack on non-local returns.

    Code* code = fun->body();
    // Pass &cntxt.cloenv, to let evalRirCode update the env of the current
    // context
    SEXP result = rirCallTrampoline_(cntxt, call, code, env, ctx);
    PROTECT(result);

    endClosureDebug(call.ast, call.callee, env);
    endClosureContext(&cntxt, result);

    R_Srcref = cntxt.srcref;
    R_ReturnedValue = R_NilValue;

    UNPROTECT(2);
    return result;
}

static RIR_INLINE SEXP rirCallTrampoline(const CallContext& call, Function* fun,
                                         SEXP arglist,
                                         InterpreterInstance* ctx) {
    return rirCallTrampoline(call, fun, symbol::delayedEnv, arglist, ctx);
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-variable"
const static SEXP loopTrampolineMarker = (SEXP)0x7007;
static void loopTrampoline(Code* c, InterpreterInstance* ctx, SEXP env,
                           const CallContext* callCtxt, Opcode* pc,
                           R_bcstack_t* localsBase) {
    assert(env);

    RCNTXT cntxt;
    Rf_begincontext(&cntxt, CTXT_LOOP, R_NilValue, env, R_BaseEnv, R_NilValue,
                    R_NilValue);

    if (int s = SETJMP(cntxt.cjmpbuf)) {
        // incoming non-local break/continue:
        if (s == CTXT_BREAK) {
            Rf_endcontext(&cntxt);
            return;
        }
        // continue case: fall through to do another iteration
    }

    // execute the loop body
    SEXP res = evalRirCode(c, ctx, env, callCtxt, pc, localsBase);
    assert(res == loopTrampolineMarker);
    Rf_endcontext(&cntxt);
}
#pragma GCC diagnostic pop

static SEXP inlineContextTrampoline(Code* c, const CallContext* callCtx,
                                    SEXP ast, SEXP sysparent, SEXP op,
                                    InterpreterInstance* ctx, Opcode* pc,
                                    R_bcstack_t* localsBase) {
    RCNTXT cntxt;
    // The first env should be the callee env, but that will be done by the
    // callee. We store sysparent there, because our optimizer may actually
    // delay instructions into the inlinee and might assume that we still have
    // to outer env.
    initClosureContext(ast, &cntxt, symbol::delayedEnv, sysparent,
                       symbol::delayedArglist, op);
    auto trampoline = [&]() {
        if ((SETJMP(cntxt.cjmpbuf))) {
            if (R_ReturnedValue == R_RestartToken) {
                cntxt.callflag = CTXT_RETURN; /* turn restart off */
                R_ReturnedValue = R_NilValue; /* remove restart token */
                return evalRirCode(c, ctx, cntxt.cloenv, callCtx, pc);
            } else {
                return R_ReturnedValue;
            }
        }
        return evalRirCode(c, ctx, sysparent, callCtx, pc, localsBase);
    };

    // execute the inlined function
    auto res = trampoline();
    endClosureContext(&cntxt, res);
    return res;
}

static RIR_INLINE SEXP legacySpecialCall(const CallContext& call,
                                         InterpreterInstance* ctx) {
    assert(call.ast != R_NilValue);
    assert(TYPEOF(call.callerEnv) == ENVSXP);

    // get the ccode
    CCODE f = getBuiltin(call.callee);
    int flag = getFlag(call.callee);
    R_Visible = static_cast<Rboolean>(flag != 1);
    // call it with the AST only
    SEXP result = f(call.ast, call.callee, CDR(call.ast), call.callerEnv);
    if (flag < 2)
        R_Visible = static_cast<Rboolean>(flag != 1);
    return result;
}

static RIR_INLINE SEXP legacyCallWithArgslist(const CallContext& call,
                                              SEXP argslist,
                                              InterpreterInstance* ctx) {
    if (TYPEOF(call.callee) == BUILTINSXP) {
        // get the ccode
        CCODE f = getBuiltin(call.callee);
        int flag = getFlag(call.callee);
        if (flag < 2)
            R_Visible = static_cast<Rboolean>(flag != 1);
        // call it
        SEXP result = f(call.ast, call.callee, argslist, call.callerEnv);
        if (flag < 2)
            R_Visible = static_cast<Rboolean>(flag != 1);
        return result;
    }

    assert(TYPEOF(call.callee) == CLOSXP &&
           TYPEOF(BODY(call.callee)) != EXTERNALSXP);
    return Rf_applyClosure(call.ast, call.callee, argslist, call.callerEnv,
                           R_NilValue);
}

static RIR_INLINE SEXP legacyCall(const CallContext& call,
                                  InterpreterInstance* ctx) {
    // create the argslist
    SEXP argslist = createLegacyArgsList(call, ctx);
    PROTECT(argslist);
    SEXP res = legacyCallWithArgslist(call, argslist, ctx);
    UNPROTECT(1);
    return res;
}

static SEXP closureArgumentAdaptor(const CallContext& call, SEXP arglist,
                                   SEXP suppliedvars) {
    SEXP op = call.callee;
    if (FORMALS(op) == R_NilValue && arglist == R_NilValue)
        return Rf_NewEnvironment(R_NilValue, R_NilValue, CLOENV(op));

    /*  Set up a context with the call in it so error has access to it */
    RCNTXT cntxt;
    initClosureContext(call.ast, &cntxt, CLOENV(op), call.callerEnv, arglist,
                       op);

    /*  Build a list which matches the actual (unevaluated) arguments
        to the formal paramters.  Build a new environment which
        contains the matched pairs.  Ideally this environment sould be
        hashed.  */
    SEXP newrho, a, f;

    SEXP actuals = Rf_matchArgs(FORMALS(op), arglist, call.ast);
    PROTECT(newrho = Rf_NewEnvironment(FORMALS(op), actuals, CLOENV(op)));

    /* Turn on reference counting for the binding cells so local
       assignments arguments increment REFCNT values */
    for (a = actuals; a != R_NilValue; a = CDR(a))
        ENABLE_REFCNT(a);

    /*  Use the default code for unbound formals.  FIXME: It looks like
        this code should preceed the building of the environment so that
        this will also go into the hash table.  */

    /* This piece of code is destructively modifying the actuals list,
       which is now also the list of bindings in the frame of newrho.
       This is one place where internal structure of environment
       bindings leaks out of envir.c.  It should be rewritten
       eventually so as not to break encapsulation of the internal
       environment layout.  We can live with it for now since it only
       happens immediately after the environment creation.  LT */

    f = FORMALS(op);
    a = actuals;
    // get the first Code that is a compiled default value of a formal arg
    // (or nullptr if no such exist)
    Function* fun = DispatchTable::unpack(BODY(op))->baseline();
    size_t pos = 0;
    while (f != R_NilValue) {
        Code* c = fun->defaultArg(pos++);
        if (CAR(f) != R_MissingArg) {
            if (CAR(a) == R_MissingArg) {
                assert(c != nullptr && "No more compiled formals available.");
                SETCAR(a, createPromise(c, newrho));
                SET_MISSING(a, 2);
            }
            // Either just used the compiled formal or it was not needed.
            // Skip over current compiled formal and find the next default arg.
        }
        assert(CAR(f) != R_DotsSymbol || TYPEOF(CAR(a)) == DOTSXP);
        f = CDR(f);
        a = CDR(a);
    }

    /*  Fix up any extras that were supplied by usemethod. */

    if (suppliedvars != R_NilValue)
        Rf_addMissingVarsToNewEnv(newrho, suppliedvars);

    if (R_envHasNoSpecialSymbols(newrho))
        SET_NO_SPECIAL_SYMBOLS(newrho);

    endClosureContext(&cntxt, R_NilValue);

    UNPROTECT(1);

    return newrho;
};

static SEXP findRootPromise(SEXP p) {
    if (TYPEOF(p) == PROMSXP) {
        while (TYPEOF(PREXPR(p)) == PROMSXP) {
            p = PREXPR(p);
        }
    }
    return p;
}

static void addDynamicAssumptionsFromContext(CallContext& call) {
    Assumptions& given = call.givenAssumptions;

    if (!call.hasNames())
        given.add(Assumption::CorrectOrderOfArguments);

    given.add(Assumption::NoExplicitlyMissingArgs);
    if (call.hasStackArgs()) {
        // Always true in this case, since we will pad missing args on the stack
        // later with R_MissingArg's
        given.add(Assumption::NotTooFewArguments);

        auto testArg = [&](size_t i) {
            SEXP arg = call.stackArg(i);
            bool notObj = true;
            bool isEager = true;
            if (TYPEOF(arg) == PROMSXP) {
                arg = PRVALUE(arg);
                if (arg == R_UnboundValue) {
                    notObj = false;
                    isEager = false;
                }
            } else if (arg == R_MissingArg) {
                given.remove(Assumption::NoExplicitlyMissingArgs);
                isEager = false;
            }
            if (isObject(arg)) {
                notObj = false;
            }
            if (isEager)
                given.setEager(i);
            if (notObj)
                given.setNotObj(i);
            if (isEager && notObj && IS_SIMPLE_SCALAR(arg, REALSXP))
                given.setSimpleReal(i);
            if (isEager && notObj && IS_SIMPLE_SCALAR(arg, INTSXP))
                given.setSimpleInt(i);
        };

        for (size_t i = 0; i < call.suppliedArgs; ++i) {
            testArg(i);
        }
    } else {
        for (size_t i = 0; i < call.suppliedArgs; ++i) {
            if (call.missingArg(i))
                given.remove(Assumption::NoExplicitlyMissingArgs);
        }
    }
}

static RIR_INLINE Assumptions addDynamicAssumptionsForOneTarget(
    const CallContext& call, const FunctionSignature& signature) {
    Assumptions given = call.givenAssumptions;

    if (call.suppliedArgs <= signature.formalNargs()) {
        given.numMissing(signature.formalNargs() - call.suppliedArgs);
    }

    if (!call.hasStackArgs()) {
        if (call.suppliedArgs >= signature.expectedNargs())
            given.add(Assumption::NotTooFewArguments);
    }

    if (call.suppliedArgs <= signature.formalNargs())
        given.add(Assumption::NotTooManyArguments);

    return given;
}

static RIR_INLINE bool matches(const CallContext& call,
                               const FunctionSignature& signature) {
    // TODO: look at the arguments of the function signature and not just at the
    // global assumptions list. This only becomes relevant as soon as we want to
    // optimize based on argument types.

    // Baseline always matches!
    if (signature.optimization ==
        FunctionSignature::OptimizationLevel::Baseline) {
#ifdef DEBUG_DISPATCH
        std::cout << "BL\n";
#endif
        return true;
    }

    assert(signature.envCreation ==
           FunctionSignature::Environment::CalleeCreated);

    if (!call.hasStackArgs()) {
        // We can't materialize ... in optimized code yet
        for (size_t i = 0; i < call.suppliedArgs; ++i)
            if (call.implicitArgIdx(i) == DOTS_ARG_IDX)
                return false;
    }

    Assumptions given = addDynamicAssumptionsForOneTarget(call, signature);

#ifdef DEBUG_DISPATCH
    std::cout << "have   " << given << "\n";
    std::cout << "trying " << signature.assumptions << "\n";
    std::cout << " -> " << signature.assumptions.subtype(given) << "\n";
#endif
    // Check if given assumptions match required assumptions
    return signature.assumptions.subtype(given);
}

// Watch out: this changes call.nargs! To clean up after the call, you need to
// pop call.nargs number of arguments (which now might be more than the number
// of actually supplied arguments).
static RIR_INLINE void supplyMissingArgs(CallContext& call,
                                         const Function* fun) {
    auto signature = fun->signature();
    assert(call.hasStackArgs());
    if (signature.expectedNargs() > call.suppliedArgs) {
        for (size_t i = 0; i < signature.expectedNargs() - call.suppliedArgs;
             ++i)
            ostack_push(ctx, R_MissingArg);
        call.passedArgs = signature.expectedNargs();
    }
}

static Function* dispatch(const CallContext& call, DispatchTable* vt) {
    // Find the most specific version of the function that can be called given
    // the current call context.
    Function* fun = nullptr;
    for (int i = vt->size() - 1; i >= 0; i--) {
        auto candidate = vt->get(i);
        if (matches(call, candidate->signature())) {
            fun = candidate;
            break;
        }
    }
    assert(fun);

    return fun;
};

unsigned pir::Parameter::RIR_WARMUP =
    getenv("PIR_WARMUP") ? atoi(getenv("PIR_WARMUP")) : 3;

// Call a RIR function. Arguments are still untouched.
RIR_INLINE SEXP rirCall(CallContext& call, InterpreterInstance* ctx) {
    SEXP body = BODY(call.callee);
    assert(DispatchTable::check(body));

    auto table = DispatchTable::unpack(body);

    addDynamicAssumptionsFromContext(call);
    Function* fun = dispatch(call, table);
    fun->registerInvocation();

    if (!fun->unoptimizable &&
        fun->invocationCount() % pir::Parameter::RIR_WARMUP == 0) {
        Assumptions given =
            addDynamicAssumptionsForOneTarget(call, fun->signature());
        // addDynamicAssumptionForOneTarget compares arguments with the
        // signature of the current dispatch target. There the number of
        // arguments might be off. But we want to force compiling a new version
        // exactly for this number of arguments, thus we need to add this as an
        // explicit assumption.
        given.add(Assumption::NotTooFewArguments);
        if (fun == table->baseline() || given != fun->signature().assumptions) {
            if (Assumptions(given).includes(
                    pir::Rir2PirCompiler::minimalAssumptions)) {
                // More assumptions are available than this version uses. Let's
                // try compile a better matching version.
#ifdef DEBUG_DISPATCH
                std::cout << "Optimizing for new context:";
                std::cout << given << " vs " << fun->signature().assumptions
                          << "\n";
#endif
                SEXP lhs = CAR(call.ast);
                SEXP name = R_NilValue;
                if (TYPEOF(lhs) == SYMSXP)
                    name = lhs;
                ctx->closureOptimizer(call.callee, given, name);
                fun = dispatch(call, table);
            }
        }
    }

    bool needsEnv = fun->signature().envCreation ==
                    FunctionSignature::Environment::CallerProvided;
    SEXP result = nullptr;
    auto arglist = call.arglist;
    if (needsEnv) {
        if (!arglist)
            arglist = createLegacyLazyArgsList(call, ctx);
        PROTECT(arglist);
        SEXP env = closureArgumentAdaptor(call, arglist, R_NilValue);
        PROTECT(env);
        result = rirCallTrampoline(call, fun, env, arglist, ctx);
        UNPROTECT(2);
    } else {
        if (call.hasStackArgs()) {
            // Instead of a SEXP with the argslist we create an
            // structure with the information needed to recreate
            // the list lazily if the gnu-r interpreter needs it
            ArgsLazyData lazyArgs(&call, ctx);
            if (!arglist)
                arglist = (SEXP)&lazyArgs;
            supplyMissingArgs(call, fun);
            result = rirCallTrampoline(call, fun, arglist, ctx);
        } else {
            if (!arglist)
                arglist = createLegacyArgsList(call, ctx);
            PROTECT(arglist);
            result = rirCallTrampoline(call, fun, arglist, ctx);
            UNPROTECT(1);
        }
    }

    assert(result);

    assert(!fun->deopt);
    return result;
}

#ifdef DEBUG_SLOWCASES

class SlowcaseCounter {
  public:
    std::unordered_map<std::string, size_t> counter;

    void count(const std::string& kind, CallContext& call,
               InterpreterInstance* ctx) {
        std::stringstream message;
        message << "Fast case " << kind << " failed for "
                << getBuiltinName(getBuiltinNr(call.callee)) << " ("
                << getBuiltinNr(call.callee) << ") "
                << "nargs : " << call.suppliedArgs;
        if (call.suppliedArgs > 0) {
            auto arg = call.stackArg(0);
            if (TYPEOF(arg) == PROMSXP)
                arg = safeForcePromise(arg);
            if (arg == R_UnboundValue)
                message << "arg0 lazy";
            else if (arg == R_MissingArg)
                message << "arg0 missing";
            else
                message << " arg0 : " << type2char(TYPEOF(arg)) << " a "
                        << (ATTRIB(arg) != R_NilValue);
        }
        if (!counter.count(message.str()))
            counter[message.str()] = 0;
        counter[message.str()]++;
    }

    static constexpr size_t TRESHOLD = 100;
    ~SlowcaseCounter() {
        std::map<size_t, std::set<std::string>> order;
        for (auto& e : counter)
            if (e.second > TRESHOLD)
                order[e.second].insert(e.first);
        for (auto& o : order) {
            for (auto& e : o.second) {
                std::cout << o.first << " times: " << e << "\n";
            }
        }
    }
};
SlowcaseCounter SLOWCASE_COUNTER;
#endif

static RIR_INLINE SEXP builtinCall(CallContext& call,
                                   InterpreterInstance* ctx) {
    if (call.hasStackArgs() && !call.hasNames()) {
        SEXP res = tryFastBuiltinCall(call, ctx);
        if (res)
            return res;
#ifdef DEBUG_SLOWCASES
        SLOWCASE_COUNTER.count("builtin", call, ctx);
#endif
    }
    return legacyCall(call, ctx);
}

static RIR_INLINE SEXP specialCall(CallContext& call,
                                   InterpreterInstance* ctx) {
    if (call.hasStackArgs() && !call.hasNames()) {
        SEXP res = tryFastSpecialCall(call, ctx);
        if (res)
            return res;
#ifdef DEBUG_SLOWCASES
        SLOWCASE_COUNTER.count("special", call, ctx);
#endif
    }
    return legacySpecialCall(call, ctx);
}

static SEXP doCall(CallContext& call, InterpreterInstance* ctx) {
    assert(call.callee);

    switch (TYPEOF(call.callee)) {
    case SPECIALSXP:
        return specialCall(call, ctx);
    case BUILTINSXP:
        return builtinCall(call, ctx);
    case CLOSXP: {
        if (TYPEOF(BODY(call.callee)) != EXTERNALSXP)
            return legacyCall(call, ctx);
        return rirCall(call, ctx);
    }
    default:
        Rf_error("Invalid Callee");
    };
    return R_NilValue;
}

static SEXP dispatchApply(SEXP ast, SEXP obj, SEXP actuals, SEXP selector,
                          SEXP callerEnv, InterpreterInstance* ctx) {
    SEXP op = SYMVALUE(selector);

    // ===============================================
    // First try S4
    if (IS_S4_OBJECT(obj) && R_has_methods(op)) {
        SEXP result = R_possible_dispatch(ast, op, actuals, callerEnv, TRUE);
        if (result)
            return result;
    }

    // ===============================================
    // Then try S3
    const char* generic = CHAR(PRINTNAME(selector));
    SEXP rho1 = Rf_NewEnvironment(R_NilValue, R_NilValue, callerEnv);
    PROTECT(rho1);
    RCNTXT cntxt;
    initClosureContext(ast, &cntxt, rho1, callerEnv, actuals, op);
    SEXP result;
    bool success = Rf_usemethod(generic, obj, ast, actuals, rho1, callerEnv,
                                R_BaseEnv, &result);
    UNPROTECT(1);
    endClosureContext(&cntxt, success ? result : R_NilValue);
    if (success)
        return result;

    return nullptr;
}

#define R_INT_MAX INT_MAX
#define R_INT_MIN -INT_MAX
// .. relying on fact that NA_INTEGER is outside of these

static R_INLINE int R_integer_plus(int x, int y, Rboolean* pnaflag) {
    if (x == NA_INTEGER || y == NA_INTEGER)
        return NA_INTEGER;

    if (((y > 0) && (x > (R_INT_MAX - y))) ||
        ((y < 0) && (x < (R_INT_MIN - y)))) {
        if (pnaflag != NULL)
            *pnaflag = TRUE;
        return NA_INTEGER;
    }
    return x + y;
}

static R_INLINE int R_integer_minus(int x, int y, Rboolean* pnaflag) {
    if (x == NA_INTEGER || y == NA_INTEGER)
        return NA_INTEGER;

    if (((y < 0) && (x > (R_INT_MAX + y))) ||
        ((y > 0) && (x < (R_INT_MIN + y)))) {
        if (pnaflag != NULL)
            *pnaflag = TRUE;
        return NA_INTEGER;
    }
    return x - y;
}

#define GOODIPROD(x, y, z) ((double)(x) * (double)(y) == (z))
static R_INLINE int R_integer_times(int x, int y, Rboolean* pnaflag) {
    if (x == NA_INTEGER || y == NA_INTEGER)
        return NA_INTEGER;
    else {
        int z = x * y;
        if (GOODIPROD(x, y, z) && z != NA_INTEGER)
            return z;
        else {
            if (pnaflag != NULL)
                *pnaflag = TRUE;
            return NA_INTEGER;
        }
    }
}

enum op { PLUSOP, MINUSOP, TIMESOP, DIVOP, POWOP, MODOP, IDIVOP };
#define INTEGER_OVERFLOW_WARNING "NAs produced by integer overflow"

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

#define CHECK_INTEGER_OVERFLOW(ans, naflag)                                    \
    do {                                                                       \
        if (naflag) {                                                          \
            PROTECT(ans);                                                      \
            SEXP call = getSrcForCall(c, pc - 1, ctx);                         \
            Rf_warningcall(call, INTEGER_OVERFLOW_WARNING);                    \
            UNPROTECT(1);                                                      \
        }                                                                      \
    } while (0)

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
                                                                               \
        if (flag < 2)                                                          \
            R_Visible = static_cast<Rboolean>(flag != 1);                      \
        SEXP call = getSrcForCall(c, pc - 1, ctx);                             \
                                                                               \
        if (!env || !(isObject(lhs) || isObject(rhs))) {                       \
            SEXPREC arglist2 = createFakeCONS(R_NilValue);                     \
            SEXPREC arglist = createFakeCONS(&arglist2);                       \
            arglist.u.listsxp.carval = lhs;                                    \
            arglist2.u.listsxp.carval = rhs;                                   \
            res = blt(call, prim, &arglist, env);                              \
        } else {                                                               \
            SEXP arglist = CONS_NR(lhs, CONS_NR(rhs, R_NilValue));             \
            ostack_push(ctx, arglist);                                         \
            res = blt(call, prim, arglist, env);                               \
            ostack_pop(ctx);                                                   \
        }                                                                      \
                                                                               \
        if (flag < 2)                                                          \
            R_Visible = static_cast<Rboolean>(flag != 1);                      \
    } while (false)

#define DO_FAST_BINOP(op, op2)                                                 \
    do {                                                                       \
        if (IS_SIMPLE_SCALAR(lhs, REALSXP)) {                                  \
            if (IS_SIMPLE_SCALAR(rhs, REALSXP)) {                              \
                res_type = REALSXP;                                            \
                real_res = (*REAL(lhs) == NA_REAL || *REAL(rhs) == NA_REAL)    \
                               ? NA_REAL                                       \
                               : *REAL(lhs) op * REAL(rhs);                    \
            } else if (IS_SIMPLE_SCALAR(rhs, INTSXP)) {                        \
                res_type = REALSXP;                                            \
                real_res =                                                     \
                    (*REAL(lhs) == NA_REAL || *INTEGER(rhs) == NA_INTEGER)     \
                        ? NA_REAL                                              \
                        : *REAL(lhs) op * INTEGER(rhs);                        \
            }                                                                  \
        } else if (IS_SIMPLE_SCALAR(lhs, INTSXP)) {                            \
            if (IS_SIMPLE_SCALAR(rhs, INTSXP)) {                               \
                Rboolean naflag = FALSE;                                       \
                switch (op2) {                                                 \
                case PLUSOP:                                                   \
                    int_res =                                                  \
                        R_integer_plus(*INTEGER(lhs), *INTEGER(rhs), &naflag); \
                    break;                                                     \
                case MINUSOP:                                                  \
                    int_res = R_integer_minus(*INTEGER(lhs), *INTEGER(rhs),    \
                                              &naflag);                        \
                    break;                                                     \
                case TIMESOP:                                                  \
                    int_res = R_integer_times(*INTEGER(lhs), *INTEGER(rhs),    \
                                              &naflag);                        \
                    break;                                                     \
                }                                                              \
                res_type = INTSXP;                                             \
                CHECK_INTEGER_OVERFLOW(R_NilValue, naflag);                    \
            } else if (IS_SIMPLE_SCALAR(rhs, REALSXP)) {                       \
                res_type = REALSXP;                                            \
                real_res =                                                     \
                    (*INTEGER(lhs) == NA_INTEGER || *REAL(rhs) == NA_REAL)     \
                        ? NA_REAL                                              \
                        : *INTEGER(lhs) op * REAL(rhs);                        \
            }                                                                  \
        }                                                                      \
    } while (false)

#define STORE_BINOP(res_type, int_res, real_res)                               \
    do {                                                                       \
        SEXP a = ostack_at(ctx, 0);                                            \
        SEXP b = ostack_at(ctx, 1);                                            \
        if (NO_REFERENCES(a)) {                                                \
            TYPEOF(a) = res_type;                                              \
            res = a;                                                           \
            ostack_pop(ctx);                                                   \
            ostack_at(ctx, 0) = a;                                             \
        } else if (NO_REFERENCES(b)) {                                         \
            TYPEOF(b) = res_type;                                              \
            res = b;                                                           \
            ostack_pop(ctx);                                                   \
        } else {                                                               \
            ostack_pop(ctx);                                                   \
            ostack_at(ctx, 0) = res = Rf_allocVector(res_type, 1);             \
        }                                                                      \
        switch (res_type) {                                                    \
        case INTSXP:                                                           \
            INTEGER(res)[0] = int_res;                                         \
            break;                                                             \
        case REALSXP:                                                          \
            REAL(res)[0] = real_res;                                           \
            break;                                                             \
        }                                                                      \
    } while (false)

#define DO_BINOP(op, op2)                                                      \
    do {                                                                       \
        int int_res = -1;                                                      \
        double real_res = -2.0;                                                \
        int res_type = 0;                                                      \
        DO_FAST_BINOP(op, op2);                                                \
        if (res_type) {                                                        \
            STORE_BINOP(res_type, int_res, real_res);                          \
            R_Visible = (Rboolean) true;                                       \
        } else {                                                               \
            BINOP_FALLBACK(#op);                                               \
            ostack_pop(ctx);                                                   \
            ostack_set(ctx, 0, res);                                           \
        }                                                                      \
    } while (false)

static double myfloor(double x1, double x2) {
    double q = x1 / x2, tmp;

    if (x2 == 0.0)
        return q;
    tmp = x1 - floor(q) * x2;
    return floor(q) + floor(tmp / x2);
}

static double myfmod(double x1, double x2) {
    if (x2 == 0.0)
        return R_NaN;
    double q = x1 / x2, tmp = x1 - floor(q) * x2;
    if (R_FINITE(q) && (fabs(q) > 1 / R_AccuracyInfo.eps))
        Rf_warning("probable complete loss of accuracy in modulus");
    q = floor(tmp / x2);
    return tmp - q * x2;
}

static R_INLINE int R_integer_uplus(int x, Rboolean* pnaflag) {
    if (x == NA_INTEGER)
        return NA_INTEGER;

    return x;
}

static R_INLINE int R_integer_uminus(int x, Rboolean* pnaflag) {
    if (x == NA_INTEGER)
        return NA_INTEGER;

    return -x;
}

#define UNOP_FALLBACK(op)                                                      \
    do {                                                                       \
        static SEXP prim = NULL;                                               \
        static CCODE blt;                                                      \
        static int flag;                                                       \
        if (!prim) {                                                           \
            prim = Rf_findFun(Rf_install(op), R_GlobalEnv);                    \
            blt = getBuiltin(prim);                                            \
            flag = getFlag(prim);                                              \
        }                                                                      \
        SEXP call = getSrcForCall(c, pc - 1, ctx);                             \
        SEXP argslist = CONS_NR(val, R_NilValue);                              \
        ostack_push(ctx, argslist);                                            \
        if (flag < 2)                                                          \
            R_Visible = static_cast<Rboolean>(flag != 1);                      \
        res = blt(call, prim, argslist, env);                                  \
        if (flag < 2)                                                          \
            R_Visible = static_cast<Rboolean>(flag != 1);                      \
        ostack_pop(ctx);                                                       \
    } while (false)

#define DO_UNOP(op, op2)                                                       \
    do {                                                                       \
        if (IS_SIMPLE_SCALAR(val, REALSXP)) {                                  \
            res = Rf_allocVector(REALSXP, 1);                                  \
            *REAL(res) = (*REAL(val) == NA_REAL) ? NA_REAL : op * REAL(val);   \
            R_Visible = (Rboolean) true;                                       \
        } else if (IS_SIMPLE_SCALAR(val, INTSXP)) {                            \
            Rboolean naflag = FALSE;                                           \
            res = Rf_allocVector(INTSXP, 1);                                   \
            switch (op2) {                                                     \
            case PLUSOP:                                                       \
                *INTEGER(res) = R_integer_uplus(*INTEGER(val), &naflag);       \
                break;                                                         \
            case MINUSOP:                                                      \
                *INTEGER(res) = R_integer_uminus(*INTEGER(val), &naflag);      \
                break;                                                         \
            }                                                                  \
            CHECK_INTEGER_OVERFLOW(res, naflag);                               \
            R_Visible = (Rboolean) true;                                       \
        } else {                                                               \
            UNOP_FALLBACK(#op);                                                \
        }                                                                      \
        ostack_set(ctx, 0, res);                                               \
    } while (false)

#define DO_RELOP(op)                                                           \
    do {                                                                       \
        if (IS_SIMPLE_SCALAR(lhs, LGLSXP)) {                                   \
            if (IS_SIMPLE_SCALAR(rhs, LGLSXP)) {                               \
                if (*LOGICAL(lhs) == NA_LOGICAL ||                             \
                    *LOGICAL(rhs) == NA_LOGICAL) {                             \
                    res = R_LogicalNAValue;                                    \
                } else {                                                       \
                    res = *LOGICAL(lhs) op * LOGICAL(rhs) ? R_TrueValue        \
                                                          : R_FalseValue;      \
                }                                                              \
                break;                                                         \
            }                                                                  \
        } else if (IS_SIMPLE_SCALAR(lhs, REALSXP)) {                           \
            if (IS_SIMPLE_SCALAR(rhs, REALSXP)) {                              \
                if (*REAL(lhs) == NA_REAL || *REAL(rhs) == NA_REAL) {          \
                    res = R_LogicalNAValue;                                    \
                } else {                                                       \
                    res = *REAL(lhs) op * REAL(rhs) ? R_TrueValue              \
                                                    : R_FalseValue;            \
                }                                                              \
                break;                                                         \
            } else if (IS_SIMPLE_SCALAR(rhs, INTSXP)) {                        \
                if (*REAL(lhs) == NA_REAL || *INTEGER(rhs) == NA_INTEGER) {    \
                    res = R_LogicalNAValue;                                    \
                } else {                                                       \
                    res = *REAL(lhs) op * INTEGER(rhs) ? R_TrueValue           \
                                                       : R_FalseValue;         \
                }                                                              \
                break;                                                         \
            }                                                                  \
        } else if (IS_SIMPLE_SCALAR(lhs, INTSXP)) {                            \
            if (IS_SIMPLE_SCALAR(rhs, INTSXP)) {                               \
                if (*INTEGER(lhs) == NA_INTEGER ||                             \
                    *INTEGER(rhs) == NA_INTEGER) {                             \
                    res = R_LogicalNAValue;                                    \
                } else {                                                       \
                    res = *INTEGER(lhs) op * INTEGER(rhs) ? R_TrueValue        \
                                                          : R_FalseValue;      \
                }                                                              \
                break;                                                         \
            } else if (IS_SIMPLE_SCALAR(rhs, REALSXP)) {                       \
                if (*INTEGER(lhs) == NA_INTEGER || *REAL(rhs) == NA_REAL) {    \
                    res = R_LogicalNAValue;                                    \
                } else {                                                       \
                    res = *INTEGER(lhs) op * REAL(rhs) ? R_TrueValue           \
                                                       : R_FalseValue;         \
                }                                                              \
                break;                                                         \
            }                                                                  \
        }                                                                      \
        BINOP_FALLBACK(#op);                                                   \
    } while (false)

static SEXP seq_int(int n1, int n2) {
    int n = n1 <= n2 ? n2 - n1 + 1 : n1 - n2 + 1;
    SEXP ans = Rf_allocVector(INTSXP, n);
    int* data = INTEGER(ans);
    if (n1 <= n2) {
        while (n1 <= n2)
            *data++ = n1++;
    } else {
        while (n1 >= n2)
            *data++ = n1--;
    }
    return ans;
}

#define BINDING_CACHE_SIZE 5
typedef struct {
    SEXP loc;
    Immediate idx;
} BindingCache;

static RIR_INLINE SEXP cachedGetBindingCell(SEXP env, Immediate idx,
                                            InterpreterInstance* ctx,
                                            BindingCache* bindingCache) {
    if (env == R_BaseEnv || env == R_BaseNamespace)
        return NULL;

    Immediate cidx = idx % BINDING_CACHE_SIZE;
    if (bindingCache[cidx].idx == idx) {
        return bindingCache[cidx].loc;
    }

    SEXP sym = cp_pool_at(ctx, idx);
    SLOWASSERT(TYPEOF(sym) == SYMSXP);
    R_varloc_t loc = R_findVarLocInFrame(env, sym);
    if (!R_VARLOC_IS_NULL(loc)) {
        bindingCache[cidx].loc = loc.cell;
        bindingCache[cidx].idx = idx;
        return loc.cell;
    }
    return NULL;
}

static SEXP cachedGetVar(SEXP env, Immediate idx, InterpreterInstance* ctx,
                         BindingCache* bindingCache) {
    SEXP loc = cachedGetBindingCell(env, idx, ctx, bindingCache);
    if (loc) {
        SEXP res = CAR(loc);
        if (res != R_UnboundValue)
            return res;
    }
    SEXP sym = cp_pool_at(ctx, idx);
    SLOWASSERT(TYPEOF(sym) == SYMSXP);
    return Rf_findVar(sym, env);
}

#define ACTIVE_BINDING_MASK (1 << 15)
#define BINDING_LOCK_MASK (1 << 14)
#define IS_ACTIVE_BINDING(b) ((b)->sxpinfo.gp & ACTIVE_BINDING_MASK)
#define BINDING_IS_LOCKED(b) ((b)->sxpinfo.gp & BINDING_LOCK_MASK)
static void cachedSetVar(SEXP val, SEXP env, Immediate idx,
                         InterpreterInstance* ctx, BindingCache* bindingCache,
                         bool keepMissing = false) {
    SEXP loc = cachedGetBindingCell(env, idx, ctx, bindingCache);
    if (loc && !BINDING_IS_LOCKED(loc) && !IS_ACTIVE_BINDING(loc)) {
        SEXP cur = CAR(loc);
        if (cur == val)
            return;
        INCREMENT_NAMED(val);
        SETCAR(loc, val);
        if (!keepMissing && MISSING(loc))
            SET_MISSING(loc, 0);
        return;
    }

    SEXP sym = cp_pool_at(ctx, idx);
    SLOWASSERT(TYPEOF(sym) == SYMSXP);
    INCREMENT_NAMED(val);
    PROTECT(val);
    Rf_defineVar(sym, val, env);
    UNPROTECT(1);
}

RIR_INLINE static void castInt(bool ceil_, Code* c, Opcode* pc,
                               InterpreterInstance* ctx) {
    SEXP val = ostack_top(ctx);
    // Scalar integers (already done)
    if (IS_SIMPLE_SCALAR(val, INTSXP) && *INTEGER(val) != NA_INTEGER) {
        return;
    } else if (IS_SIMPLE_SCALAR(val, REALSXP) && NO_REFERENCES(val) &&
               !ISNAN(*REAL(val))) {
        double r = *REAL(val);
        TYPEOF(val) = INTSXP;
        *INTEGER(val) = (int)(ceil_ ? ceil(r) : floor(r));
        return;
    } else if (IS_SIMPLE_SCALAR(val, LGLSXP) && NO_REFERENCES(val) &&
               *LOGICAL(val) != NA_LOGICAL) {
        TYPEOF(val) = INTSXP;
        return;
    }
    int x = -20;
    bool isNaOrNan = false;
    if (TYPEOF(val) == INTSXP || TYPEOF(val) == REALSXP ||
        TYPEOF(val) == LGLSXP) {
        if (XLENGTH(val) == 0) {
            Rf_errorcall(getSrcAt(c, pc - 1, ctx), "argument of length 0");
            x = NA_INTEGER;
            isNaOrNan = false;
        } else {
            switch (TYPEOF(val)) {
            case INTSXP: {
                int i = *INTEGER(val);
                if (i == NA_INTEGER) {
                    x = NA_INTEGER;
                    isNaOrNan = true;
                } else {
                    x = i;
                    isNaOrNan = false;
                }
                break;
            }
            case REALSXP: {
                double r = *REAL(val);
                if (ISNAN(r)) {
                    x = NA_INTEGER;
                    isNaOrNan = true;
                } else {
                    x = (int)(ceil_ ? ceil(r) : floor(r));
                    isNaOrNan = false;
                }
                break;
            }
            case LGLSXP: {
                int l = *LOGICAL(val);
                if (l == NA_LOGICAL) {
                    x = NA_INTEGER;
                    isNaOrNan = true;
                } else {
                    x = (int)l;
                    isNaOrNan = false;
                }
                break;
            }
            default:
                assert(false);
            }
            if (XLENGTH(val) > 1) {
                Rf_warningcall(getSrcAt(c, pc - 1, ctx),
                               "numerical expression has multiple "
                               "elements: only the first used");
            }
        }
    } else { // Everything else
        x = NA_INTEGER;
        isNaOrNan = true;
    }
    if (isNaOrNan) {
        Rf_errorcall(getSrcAt(c, pc - 1, ctx), "NA/NaN argument");
    }
    SEXP res = Rf_allocVector(INTSXP, 1);
    *INTEGER(res) = x;
    ostack_pop(ctx);
    ostack_push(ctx, res);
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-align"

// This happens since enabling -fno-exceptions, but the error message is
// terrible, can't find out where in the evalRirCode function
#pragma GCC diagnostic ignored "-Wstrict-overflow"

/*
 * This function takes some deopt metadata and stack frame contents on the
 * interpreter stack. It first recursively reconstructs a context for each
 * inlined frame, bottom up. This means either reuse the already existing
 * context, or synthesize a new one. Then the frames are executed (in the
 * deoptimized version), top down. At the end we long-jump out of the outermost
 * context and thus return from the R function that triggered this deopt
 * routine.
 */
void deoptFramesWithContext(InterpreterInstance* ctx,
                            const CallContext* callCtxt,
                            DeoptMetadata* deoptData, SEXP sysparent,
                            size_t pos, size_t stackHeight,
                            bool outerHasContext) {
    size_t excessStack = stackHeight;

    FrameInfo& f = deoptData->frames[pos];
    stackHeight -= f.stackSize + 1;
    SEXP deoptEnv = ostack_at(ctx, stackHeight);
    auto code = f.code;

    bool outermostFrame = pos == deoptData->numFrames - 1;
    bool innermostFrame = pos == 0;

    RCNTXT fake;
    RCNTXT* cntxt;
    auto originalCntxt = findFunctionContextFor(deoptEnv);
    if (originalCntxt) {
        assert(outerHasContext &&
               "Frame with context after frame without context");
        cntxt = originalCntxt;
    } else {
        // NOTE: this assert triggers if we can't find the context of the
        // current function. Usually the reason is that a wrong environment is
        // stored in the context.
        assert(!outermostFrame && "Cannot find outermost function context");
        // If the inlinee had no context, we need to synthesize one
        // TODO: need to add ast and closure to the deopt metadata to create a
        // complete context
        cntxt = &fake;
        initClosureContext(R_NilValue, cntxt, deoptEnv, sysparent,
                           FRAME(sysparent), R_NilValue);
    }

    if (auto stub = LazyEnvironment::cast(deoptEnv)) {
        deoptEnv = stub->create();
        cntxt->cloenv = deoptEnv;
    }
    assert(TYPEOF(deoptEnv) == ENVSXP);

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-variable"
    auto frameBaseSize = ostack_length(ctx) - excessStack;
#pragma GCC diagnostic pop

    auto trampoline = [&]() {
        // 1. Set up our (outer) context
        //
        // The inlinees need to be bound to a new trampoline, or they could
        // long-jump out of this deopt routine into the inlineContextTrampoline.
        // The outermost frame is the caller, not an inlinee, thus we need not
        // change its context.
        if (!outermostFrame) {
            // The longjump is initialized, when we are still reconstructing
            // the frames. But if we restart from here, we need to remove
            // all the extra stuff from the stack used for reconstruction.
            cntxt->nodestack = ostack_cell_at(ctx, excessStack - 1);
            if ((SETJMP(cntxt->cjmpbuf))) {
                assert((size_t)ostack_length(ctx) == frameBaseSize);
                if (R_ReturnedValue == R_RestartToken) {
                    cntxt->callflag = CTXT_RETURN; /* turn restart off */
                    R_ReturnedValue = R_NilValue;  /* remove restart token */
                    return evalRirCode(code, ctx, cntxt->cloenv, callCtxt);
                } else {
                    return R_ReturnedValue;
                }
            }
        }

        // 2. Execute the inner frames
        if (!innermostFrame) {
            deoptFramesWithContext(ctx, callCtxt, deoptData, deoptEnv, pos - 1,
                                   stackHeight, originalCntxt);
        }

        // 3. Execute our frame
        //
        // This wrapper consumes the environment from the deopt metadata and the
        // result of the previous frame.
        assert((size_t)ostack_length(ctx) ==
               frameBaseSize + f.stackSize + (innermostFrame ? 1 : 2));
        SEXP res = nullptr;
        if (!innermostFrame)
            res = ostack_pop(ctx);
        assert(ostack_top() == deoptEnv);
        ostack_pop(ctx);
        if (!innermostFrame)
            ostack_push(ctx, res);
        code->registerInvocation();
        return evalRirCode(code, ctx, cntxt->cloenv, callCtxt, f.pc);
    };

    SEXP res = trampoline();
    assert((size_t)ostack_length(ctx) == frameBaseSize);

    if (!outermostFrame) {
        endClosureContext(cntxt, res);
    } else {
        assert(findFunctionContextFor(deoptEnv) == cntxt);
        // long-jump out of all the inlined contexts
        Rf_findcontext(CTXT_BROWSER | CTXT_FUNCTION, cntxt->cloenv, res);
        assert(false);
    }

    ostack_push(ctx, res);
}

SEXP evalRirCode(Code* c, InterpreterInstance* ctx, SEXP env,
                 const CallContext* callCtxt, Opcode* initialPC,
                 R_bcstack_t* localsBase) {
    assert(env != symbol::delayedEnv || (callCtxt != nullptr));

#ifdef THREADED_CODE
    static void* opAddr[static_cast<uint8_t>(Opcode::num_of)] = {
#define DEF_INSTR(name, ...) (__extension__ && op_##name),
#include "ir/insns.h"
#undef DEF_INSTR
    };
#endif

    assert(c->info.magic == CODE_MAGIC);

    BindingCache bindingCache[BINDING_CACHE_SIZE];
    memset(&bindingCache, 0, sizeof(bindingCache));

    bool existingLocals = localsBase;
    if (!existingLocals) {
#ifdef TYPED_STACK
        // Zero the region of the locals to avoid keeping stuff alive and to
        // zero all the type tags. Note: this trick does not work with the stack
        // in general, since there intermediate callees might set the type tags
        // to something else.
        memset(R_BCNodeStackTop, 0, sizeof(*R_BCNodeStackTop) * c->localsCount);
#endif
        localsBase = R_BCNodeStackTop;
    }
    Locals locals(localsBase, c->localsCount, existingLocals);

    // make sure there is enough room on the stack
    // there is some slack of 5 to make sure the call instruction can store
    // some intermediate values on the stack
    ostack_ensureSize(ctx, c->stackLength + 5);

    Opcode* pc = initialPC ? initialPC : c->code();
    SEXP res;

    std::vector<LazyEnvironment*> envStubs;

    auto changeEnv = [&](SEXP e) {
        assert((TYPEOF(e) == ENVSXP || LazyEnvironment::cast(e)) &&
               "Expected an environment");
        if (e != env) {
            env = e;
            // We need to clear the bindings cache, when we change the
            // environment
            memset(&bindingCache, 0, sizeof(bindingCache));
        }
    };
    R_Visible = TRUE;

    // main loop
    BEGIN_MACHINE {

        INSTRUCTION(invalid_) assert(false && "wrong or unimplemented opcode");

        INSTRUCTION(nop_) NEXT();

        INSTRUCTION(push_context_) {
            SEXP ast = ostack_at(ctx, 1);
            SEXP op = ostack_at(ctx, 0);
            assert(TYPEOF(env) == ENVSXP);
            assert(TYPEOF(op) == CLOSXP);
            ostack_popn(ctx, 2);
            int offset = readJumpOffset();
            advanceJump();
            // Recursively call myself through a inlineContextTrampoline. The
            // trampoline creates an RCNTXT, and then continues executing the
            // same code.
            inlineContextTrampoline(c, callCtxt, ast, env, op, ctx, pc,
                                    localsBase);
            // After returning from the inlined context we need to skip all the
            // instructions inside the context. Otherwise we would execute them
            // twice. Effectively this updates our pc to match the one the
            // pop_context_ had in the inlined context.
            pc += offset;
            assert(*pc == Opcode::pop_context_);
            advanceOpcode();
            NEXT();
        }

        INSTRUCTION(pop_context_) { return ostack_pop(ctx); }

        INSTRUCTION(mk_env_) {
            size_t n = readImmediate();
            advanceImmediate();
            int contextPos = readSignedImmediate();
            advanceImmediate();
            SEXP parent = ostack_pop(ctx);
            PROTECT(parent);
            assert(TYPEOF(parent) == ENVSXP &&
                   "Non-environment used as environment parent.");
            SEXP arglist = R_NilValue;
            auto names = (Immediate*)pc;
            advanceImmediateN(n);
            bool hasMissing = false;
            for (long i = n - 1; i >= 0; --i) {
                SEXP val = ostack_pop(ctx);
                ENSURE_NAMED(val);
                SEXP name = cp_pool_at(ctx, names[i]);
                arglist = CONS_NR(val, arglist);
                SET_TAG(arglist, name);
                hasMissing = hasMissing || val == R_MissingArg;
                SET_MISSING(arglist, val == R_MissingArg ? 2 : 0);
            }
            res = Rf_NewEnvironment(R_NilValue, arglist, parent);

            if (contextPos > 0) {
                if (auto cptr = getFunctionContext(contextPos - 1)) {
                    cptr->cloenv = res;
                    if (cptr->promargs == symbol::delayedArglist) {
                        auto promargs = arglist;
                        if (hasMissing) {
                            // For the promargs we need to strip missing
                            // arguments from the list, otherwise nargs()
                            // reports the wrong value.
                            promargs = Rf_shallow_duplicate(arglist);
                            // Need to test for R_MissingArg because
                            // shallowDuplicate does not copy the missing flag.
                            while (CAR(promargs) == R_MissingArg &&
                                   promargs != R_NilValue) {
                                promargs = CDR(promargs);
                            }
                            auto p = promargs;
                            auto prev = p;
                            while (p != R_NilValue) {
                                if (CAR(p) == R_MissingArg)
                                    SETCDR(prev, CDR(p));
                                prev = p;
                                p = CDR(p);
                            }
                        }
                        cptr->promargs = promargs;
                    }
                }
            }

            ostack_push(ctx, res);
            UNPROTECT(1);
            NEXT();
        }

        INSTRUCTION(mk_stub_env_) {
            // TODO: There is a potential safety problem because we are not
            // preserving the args and parent SEXP. Doing it here is not an
            // option becase R_Preserve is slow. We must find a simple story so
            // that the gc trace rir wrappers.
            size_t n = readImmediate();
            advanceImmediate();
            int contextPos = readSignedImmediate();
            advanceImmediate();
            // Do we need to preserve parent and the arg vals?
            SEXP parent = ostack_pop(ctx);
            assert(TYPEOF(parent) == ENVSXP &&
                   "Non-environment used as environment parent.");
            auto names = pc;
            advanceImmediateN(n);
            std::vector<SEXP>* args = new std::vector<SEXP>();
            for (size_t i = 0; i < n; ++i) {
                SEXP val = ostack_pop(ctx);
                ENSURE_NAMED(val);
                args->push_back(val);
            }
            auto envStub =
                new LazyEnvironment(args, parent, names, ctx, localsBase);
            envStubs.push_back(envStub);
            res = (SEXP)envStub;

            if (contextPos > 0) {
                if (auto cptr = getFunctionContext(contextPos - 1))
                    cptr->cloenv = res;
            }

            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(parent_env_) {
            // Can only be used for pir. In pir we always have a closure that
            // stores the lexical envrionment
            assert(callCtxt);
            ostack_push(ctx, CLOENV(callCtxt->callee));
            NEXT();
        }

        INSTRUCTION(get_env_) {
            assert(env);
            ostack_push(ctx, env);
            NEXT();
        }

        INSTRUCTION(set_env_) {
            SEXP e = ostack_pop(ctx);
            changeEnv(e);
            NEXT();
        }

        INSTRUCTION(ldfun_) {
            SEXP sym = readConst(ctx, readImmediate());
            advanceImmediate();
            res = Rf_findFun(sym, env);

            // TODO something should happen here
            if (res == R_UnboundValue)
                assert(false && "Unbound var");
            if (res == R_MissingArg)
                assert(false && "Missing argument");

            switch (TYPEOF(res)) {
            case CLOSXP:
                jit(res, sym, ctx);
                break;
            case SPECIALSXP:
            case BUILTINSXP:
                // special and builtin functions are ok
                break;
            default:
                Rf_error("attempt to apply non-function");
            }
            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(ldvar_for_update_) {
            Immediate id = readImmediate();
            advanceImmediate();
            SEXP loc = cachedGetBindingCell(env, id, ctx, bindingCache);
            bool isLocal = loc;
            SEXP res = nullptr;

            if (isLocal) {
                res = CAR(loc);
                if (res == R_UnboundValue)
                    isLocal = false;
            }

            if (!isLocal) {
                SEXP sym = cp_pool_at(ctx, id);
                res = Rf_findVar(sym, env);
            }

            if (res == R_UnboundValue) {
                SEXP sym = cp_pool_at(ctx, id);
                Rf_error("object \"%s\" not found", CHAR(PRINTNAME(sym)));
            } else if (res == R_MissingArg) {
                SEXP sym = cp_pool_at(ctx, id);
                Rf_error("argument \"%s\" is missing, with no default",
                         CHAR(PRINTNAME(sym)));
            }

            // if promise, evaluate & return
            if (TYPEOF(res) == PROMSXP)
                res = promiseValue(res, ctx);

            if (res != R_NilValue) {
                if (isLocal)
                    ENSURE_NAMED(res);
                else if (NAMED(res) < 2)
                    SET_NAMED(res, 2);
            }

            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(ldvar_) {
            Immediate id = readImmediate();
            advanceImmediate();
            res = cachedGetVar(env, id, ctx, bindingCache);

            if (res == R_UnboundValue) {
                SEXP sym = cp_pool_at(ctx, id);
                Rf_error("object \"%s\" not found", CHAR(PRINTNAME(sym)));
            } else if (res == R_MissingArg) {
                SEXP sym = cp_pool_at(ctx, id);
                Rf_error("argument \"%s\" is missing, with no default",
                         CHAR(PRINTNAME(sym)));
            }

            // if promise, evaluate & return
            if (TYPEOF(res) == PROMSXP)
                res = promiseValue(res, ctx);

            if (res != R_NilValue)
                ENSURE_NAMED(res);

            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(ldvar_noforce_) {
            Immediate id = readImmediate();
            advanceImmediate();
            res = cachedGetVar(env, id, ctx, bindingCache);

            if (res == R_UnboundValue) {
                SEXP sym = cp_pool_at(ctx, id);
                Rf_error("object \"%s\" not found", CHAR(PRINTNAME(sym)));
            } else if (res == R_MissingArg) {
                SEXP sym = cp_pool_at(ctx, id);
                Rf_error("argument \"%s\" is missing, with no default",
                         CHAR(PRINTNAME(sym)));
            }

            if (res != R_NilValue)
                ENSURE_NAMED(res);

            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(ldvar_super_) {
            SEXP sym = readConst(ctx, readImmediate());
            advanceImmediate();
            res = Rf_findVar(sym, ENCLOS(env));

            if (res == R_UnboundValue) {
                Rf_error("object \"%s\" not found", CHAR(PRINTNAME(sym)));
            } else if (res == R_MissingArg) {
                Rf_error("argument \"%s\" is missing, with no default",
                         CHAR(PRINTNAME(sym)));
            }

            // if promise, evaluate & return
            if (TYPEOF(res) == PROMSXP)
                res = promiseValue(res, ctx);

            if (res != R_NilValue)
                ENSURE_NAMED(res);

            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(ldvar_noforce_super_) {
            SEXP sym = readConst(ctx, readImmediate());
            advanceImmediate();
            res = Rf_findVar(sym, ENCLOS(env));

            if (res == R_UnboundValue) {
                Rf_error("object \"%s\" not found", CHAR(PRINTNAME(sym)));
            } else if (res == R_MissingArg) {
                Rf_error("argument \"%s\" is missing, with no default",
                         CHAR(PRINTNAME(sym)));
            }

            if (res != R_NilValue)
                ENSURE_NAMED(res);

            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(ldddvar_) {
            SEXP sym = readConst(ctx, readImmediate());
            advanceImmediate();
            res = Rf_ddfindVar(sym, env);

            if (res == R_UnboundValue) {
                Rf_error("object \"%s\" not found", CHAR(PRINTNAME(sym)));
            } else if (res == R_MissingArg) {
                Rf_error("argument \"%s\" is missing, with no default",
                         CHAR(PRINTNAME(sym)));
            }

            // if promise, evaluate & return
            if (TYPEOF(res) == PROMSXP)
                res = promiseValue(res, ctx);

            if (res != R_NilValue)
                ENSURE_NAMED(res);

            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(ldarg_) {
            Immediate idx = readImmediate();
            advanceImmediate();
            assert(callCtxt);

            if (callCtxt->hasStackArgs()) {
                ostack_push(ctx, callCtxt->stackArg(idx));
            } else {
                if (callCtxt->missingArg(idx)) {
                    res = R_MissingArg;
                } else {
                    Code* arg = callCtxt->implicitArg(idx);
                    res = createPromise(arg, callCtxt->callerEnv);
                }
                ostack_push(ctx, res);
            }
            NEXT();
        }

        INSTRUCTION(ldloc_) {
            Immediate offset = readImmediate();
            advanceImmediate();
            res = locals.load(offset);
            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(stvar_) {
            Immediate id = readImmediate();
            advanceImmediate();
            SEXP val = ostack_pop(ctx);

            if (auto stub = LazyEnvironment::cast(env)) {
                env = stub->create();
            }
            cachedSetVar(val, env, id, ctx, bindingCache);

            NEXT();
        }

        INSTRUCTION(starg_) {
            Immediate id = readImmediate();
            advanceImmediate();
            SEXP val = ostack_pop(ctx);

            if (auto stub = LazyEnvironment::cast(env)) {
                env = stub->create();
            }
            cachedSetVar(val, env, id, ctx, bindingCache, true);

            NEXT();
        }

        INSTRUCTION(stvar_super_) {
            SEXP sym = readConst(ctx, readImmediate());
            advanceImmediate();
            SLOWASSERT(TYPEOF(sym) == SYMSXP);
            SEXP val = ostack_pop(ctx);
            INCREMENT_NAMED(val);
            Rf_setVar(sym, val, ENCLOS(env));
            NEXT();
        }

        INSTRUCTION(stloc_) {
            Immediate offset = readImmediate();
            advanceImmediate();
            locals.store(offset, ostack_top(ctx));
            ostack_pop(ctx);
            NEXT();
        }

        INSTRUCTION(movloc_) {
            Immediate target = readImmediate();
            advanceImmediate();
            Immediate source = readImmediate();
            advanceImmediate();
            locals.store(target, locals.load(source));
            NEXT();
        }

        INSTRUCTION(named_call_implicit_) {
#ifdef ENABLE_SLOWASSERT
            auto lll = ostack_length(ctx);
            int ttt = R_PPStackTop;
#endif

            // Callee is TOS
            // Arguments and names are immediate given as promise code indices.
            size_t n = readImmediate();
            advanceImmediate();
            size_t ast = readImmediate();
            advanceImmediate();
            Assumptions given(pc);
            pc += sizeof(Assumptions);
            auto arguments = (Immediate*)pc;
            advanceImmediateN(n);
            auto names = (Immediate*)pc;
            advanceImmediateN(n);
            CallContext call(c, ostack_top(ctx), n, ast, arguments, names, env,
                             given, ctx);
            res = doCall(call, ctx);
            ostack_pop(ctx); // callee
            ostack_push(ctx, res);

            SLOWASSERT(ttt == R_PPStackTop);
            SLOWASSERT(lll == ostack_length(ctx));
            NEXT();
        }

        INSTRUCTION(record_call_) {
            ObservedCallees* feedback = (ObservedCallees*)pc;
            SEXP callee = ostack_top(ctx);
            feedback->record(c, callee);
            pc += sizeof(ObservedCallees);
            NEXT();
        }

        INSTRUCTION(record_type_) {
            ObservedValues* feedback = (ObservedValues*)pc;
            SEXP t = ostack_top(ctx);
            feedback->record(t);
            pc += sizeof(ObservedValues);
            NEXT();
        }

        INSTRUCTION(call_implicit_) {
#ifdef ENABLE_SLOWASSERT
            auto lll = ostack_length(ctx);
            int ttt = R_PPStackTop;
#endif

            // Callee is TOS
            // Arguments are immediate given as promise code indices.
            size_t n = readImmediate();
            advanceImmediate();
            size_t ast = readImmediate();
            advanceImmediate();
            Assumptions given(pc);
            pc += sizeof(Assumptions);
            auto arguments = (Immediate*)pc;
            advanceImmediateN(n);
            CallContext call(c, ostack_top(ctx), n, ast, arguments, env, given,
                             ctx);
            res = doCall(call, ctx);
            ostack_pop(ctx); // callee
            ostack_push(ctx, res);

            SLOWASSERT(ttt == R_PPStackTop);
            SLOWASSERT(lll == ostack_length(ctx));
            NEXT();
        }

        INSTRUCTION(call_) {
#ifdef ENABLE_SLOWASSERT
            auto lll = ostack_length(ctx);
            int ttt = R_PPStackTop;
#endif

            // Stack contains [callee, arg1, ..., argn]
            Immediate n = readImmediate();
            advanceImmediate();
            size_t ast = readImmediate();
            advanceImmediate();
            Assumptions given(pc);
            pc += sizeof(Assumptions);
            CallContext call(c, ostack_at(ctx, n), n, ast,
                             ostack_cell_at(ctx, n - 1), env, given, ctx);
            res = doCall(call, ctx);
            ostack_popn(ctx, call.passedArgs + 1);
            ostack_push(ctx, res);

            SLOWASSERT(ttt == R_PPStackTop);
            SLOWASSERT(lll - call.suppliedArgs == (unsigned)ostack_length(ctx));
            NEXT();
        }

        INSTRUCTION(named_call_) {
#ifdef ENABLE_SLOWASSERT
            auto lll = ostack_length(ctx);
            int ttt = R_PPStackTop;
#endif

            // Stack contains [callee, arg1, ..., argn]
            Immediate n = readImmediate();
            advanceImmediate();
            size_t ast = readImmediate();
            advanceImmediate();
            Assumptions given(pc);
            pc += sizeof(Assumptions);
            auto names = (Immediate*)pc;
            advanceImmediateN(n);
            CallContext call(c, ostack_at(ctx, n), n, ast,
                             ostack_cell_at(ctx, n - 1), names, env, given,
                             ctx);
            res = doCall(call, ctx);
            ostack_popn(ctx, call.passedArgs + 1);
            ostack_push(ctx, res);

            SLOWASSERT(ttt == R_PPStackTop);
            SLOWASSERT(lll - call.suppliedArgs == (unsigned)ostack_length(ctx));
            NEXT();
        }

        INSTRUCTION(call_builtin_) {
#ifdef ENABLE_SLOWASSERT
            auto lll = ostack_length(ctx);
            int ttt = R_PPStackTop;
#endif

            // Stack contains [arg1, ..., argn], callee is immediate
            Immediate n = readImmediate();
            advanceImmediate();
            Immediate ast = readImmediate();
            advanceImmediate();
            SEXP callee = cp_pool_at(ctx, readImmediate());
            advanceImmediate();
            CallContext call(c, callee, n, ast, ostack_cell_at(ctx, n - 1), env,
                             Assumptions(), ctx);
            res = builtinCall(call, ctx);
            ostack_popn(ctx, call.passedArgs);
            ostack_push(ctx, res);

            SLOWASSERT(ttt == R_PPStackTop);
            SLOWASSERT(lll - call.suppliedArgs + 1 ==
                       (unsigned)ostack_length(ctx));
            NEXT();
        }

        INSTRUCTION(static_call_) {
#ifdef ENABLE_SLOWASSERT
            auto lll = ostack_length(ctx);
            int ttt = R_PPStackTop;
#endif

            // Stack contains [arg1, ..., argn], callee is immediate
            Immediate n = readImmediate();
            advanceImmediate();
            Immediate ast = readImmediate();
            advanceImmediate();
            Assumptions given(pc);
            pc += sizeof(Assumptions);
            SEXP callee = cp_pool_at(ctx, readImmediate());
            advanceImmediate();
            SEXP version = cp_pool_at(ctx, readImmediate());
            CallContext call(c, callee, n, ast, ostack_cell_at(ctx, n - 1), env,
                             given, ctx);
            auto fun = Function::unpack(version);
            addDynamicAssumptionsFromContext(call);
            bool dispatchFail = !fun->dead && !matches(call, fun->signature());
            if (fun->invocationCount() % pir::Parameter::RIR_WARMUP == 0) {
                Assumptions assumptions =
                    addDynamicAssumptionsForOneTarget(call, fun->signature());
                if (assumptions != fun->signature().assumptions)
                    // We have more assumptions available, let's recompile
                    dispatchFail = true;
            }

            if (dispatchFail) {
                auto dt = DispatchTable::unpack(BODY(callee));
                fun = dispatch(call, dt);
                // Patch inline cache
                (*(Immediate*)pc) = Pool::insert(fun->container());
            }
            advanceImmediate();

            if (fun->signature().envCreation ==
                FunctionSignature::Environment::CallerProvided) {
                res = doCall(call, ctx);
            } else {
                ArgsLazyData lazyArgs(&call, ctx);
                fun->registerInvocation();
                supplyMissingArgs(call, fun);
                res = rirCallTrampoline(call, fun, symbol::delayedEnv,
                                        (SEXP)&lazyArgs, ctx);
            }
            ostack_popn(ctx, call.passedArgs);
            ostack_push(ctx, res);

            SLOWASSERT(ttt == R_PPStackTop);
            SLOWASSERT(lll - call.suppliedArgs + 1 ==
                       (unsigned)ostack_length(ctx));
            NEXT();
        }

        INSTRUCTION(close_) {
            SEXP srcref = ostack_at(ctx, 0);
            SEXP body = ostack_at(ctx, 1);
            SEXP formals = ostack_at(ctx, 2);
            res = Rf_allocSExp(CLOSXP);
            assert(DispatchTable::check(body));
            SET_FORMALS(res, formals);
            SET_BODY(res, body);
            SET_CLOENV(res, env);
            Rf_setAttrib(res, Rf_install("srcref"), srcref);
            ostack_popn(ctx, 3);
            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(isfun_) {
            SEXP val = ostack_top(ctx);

            switch (TYPEOF(val)) {
            case CLOSXP:
                jit(val, R_NilValue, ctx);
                break;
            case SPECIALSXP:
            case BUILTINSXP:
                // builtins and specials are fine
                // TODO for now - we might be fancier here later
                break;
            default:
                Rf_error("attempt to apply non-function");
            }
            NEXT();
        }

        INSTRUCTION(promise_) {
            Immediate id = readImmediate();
            advanceImmediate();
            SEXP prom = Rf_mkPROMISE(c->getPromise(id)->container(), env);
            SET_PRVALUE(prom, ostack_pop(ctx));
            ostack_push(ctx, prom);
            NEXT();
        }

        INSTRUCTION(force_) {
            if (TYPEOF(ostack_top(ctx)) == PROMSXP) {
                SEXP val = ostack_pop(ctx);
                // If the promise is already evaluated then push the value
                // inside the promise onto the stack, otherwise push the value
                // from forcing the promise
                ostack_push(ctx, promiseValue(val, ctx));
            }
            NEXT();
        }

        INSTRUCTION(push_) {
            res = readConst(ctx, readImmediate());
            advanceImmediate();
            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(push_code_) {
            Immediate n = readImmediate();
            advanceImmediate();
            ostack_push(ctx, c->getPromise(n)->container());
            NEXT();
        }

        INSTRUCTION(dup_) {
            ostack_push(ctx, ostack_top(ctx));
            NEXT();
        }

        INSTRUCTION(dup2_) {
            ostack_push(ctx, ostack_at(ctx, 1));
            ostack_push(ctx, ostack_at(ctx, 1));
            NEXT();
        }

        INSTRUCTION(pop_) {
            ostack_pop(ctx);
            NEXT();
        }

        INSTRUCTION(popn_) {
            Immediate i = readImmediate();
            advanceImmediate();
            ostack_popn(ctx, i);
            NEXT();
        }

        INSTRUCTION(swap_) {
            SEXP lhs = ostack_pop(ctx);
            SEXP rhs = ostack_pop(ctx);
            ostack_push(ctx, lhs);
            ostack_push(ctx, rhs);
            NEXT();
        }

        INSTRUCTION(put_) {
            Immediate i = readImmediate();
            advanceImmediate();
            R_bcstack_t* pos = ostack_cell_at(ctx, 0);
#ifdef TYPED_STACK
            SEXP val = pos->u.sxpval;
            while (i--) {
                pos->u.sxpval = (pos - 1)->u.sxpval;
                pos--;
            }
            pos->u.sxpval = val;
#else
            SEXP val = *pos;
            while (i--) {
                *pos = *(pos - 1);
                pos--;
            }
            *pos = val;
#endif
            NEXT();
        }

        INSTRUCTION(pick_) {
            Immediate i = readImmediate();
            advanceImmediate();
            R_bcstack_t* pos = ostack_cell_at(ctx, i);
#ifdef TYPED_STACK
            SEXP val = pos->u.sxpval;
            while (i--) {
                pos->u.sxpval = (pos + 1)->u.sxpval;
                pos++;
            }
            pos->u.sxpval = val;
#else
            SEXP val = *pos;
            while (i--) {
                *pos = *(pos + 1);
                pos++;
            }
            *pos = val;
#endif
            NEXT();
        }

        INSTRUCTION(pull_) {
            Immediate i = readImmediate();
            advanceImmediate();
            SEXP val = ostack_at(ctx, i);
            ostack_push(ctx, val);
            NEXT();
        }

        INSTRUCTION(add_) {
            SEXP lhs = ostack_at(ctx, 1);
            SEXP rhs = ostack_at(ctx, 0);
            DO_BINOP(+, PLUSOP);
            NEXT();
        }

        INSTRUCTION(uplus_) {
            SEXP val = ostack_at(ctx, 0);
            DO_UNOP(+, PLUSOP);
            NEXT();
        }

        INSTRUCTION(inc_) {
            SEXP val = ostack_top(ctx);
            SLOWASSERT(TYPEOF(val) == INTSXP);
            if (MAYBE_REFERENCED(val)) {
                int i = INTEGER(val)[0];
                ostack_pop(ctx);
                SEXP n = Rf_allocVector(INTSXP, 1);
                INTEGER(n)[0] = i + 1;
                ostack_push(ctx, n);
            } else {
                INTEGER(val)[0]++;
            }
            NEXT();
        }

        INSTRUCTION(dec_) {
            SEXP val = ostack_top(ctx);
            SLOWASSERT(TYPEOF(val) == INTSXP);
            if (MAYBE_REFERENCED(val)) {
                int i = INTEGER(val)[0];
                ostack_pop(ctx);
                SEXP n = Rf_allocVector(INTSXP, 1);
                INTEGER(n)[0] = i - 1;
                ostack_push(ctx, n);
            } else {
                INTEGER(val)[0]--;
            }
            NEXT();
        }

        INSTRUCTION(sub_) {
            SEXP lhs = ostack_at(ctx, 1);
            SEXP rhs = ostack_at(ctx, 0);
            DO_BINOP(-, MINUSOP);
            NEXT();
        }

        INSTRUCTION(uminus_) {
            SEXP val = ostack_at(ctx, 0);
            DO_UNOP(-, MINUSOP);
            NEXT();
        }

        INSTRUCTION(mul_) {
            SEXP lhs = ostack_at(ctx, 1);
            SEXP rhs = ostack_at(ctx, 0);
            DO_BINOP(*, TIMESOP);
            NEXT();
        }

        INSTRUCTION(div_) {
            SEXP lhs = ostack_at(ctx, 1);
            SEXP rhs = ostack_at(ctx, 0);

            if (IS_SIMPLE_SCALAR(lhs, REALSXP) &&
                IS_SIMPLE_SCALAR(rhs, REALSXP)) {
                double real_res =
                    (*REAL(lhs) == NA_REAL || *REAL(rhs) == NA_REAL)
                        ? NA_REAL
                        : *REAL(lhs) / *REAL(rhs);
                STORE_BINOP(REALSXP, 0, real_res);
            } else if (IS_SIMPLE_SCALAR(lhs, REALSXP) &&
                       IS_SIMPLE_SCALAR(rhs, INTSXP)) {
                double real_res;
                int r = *INTEGER(rhs);
                if (*REAL(lhs) == NA_REAL || r == NA_INTEGER)
                    real_res = NA_REAL;
                else
                    real_res = *REAL(lhs) / (double)r;
                STORE_BINOP(REALSXP, 0, real_res);
            } else if (IS_SIMPLE_SCALAR(lhs, INTSXP) &&
                       IS_SIMPLE_SCALAR(rhs, REALSXP)) {
                double real_res;
                int l = *INTEGER(lhs);
                if (l == NA_INTEGER || *REAL(rhs) == NA_REAL)
                    real_res = NA_REAL;
                else
                    real_res = (double)l / *REAL(rhs);
                STORE_BINOP(REALSXP, 0, real_res);
            } else if (IS_SIMPLE_SCALAR(lhs, INTSXP) &&
                       IS_SIMPLE_SCALAR(rhs, INTSXP)) {
                double real_res;
                int l = *INTEGER(lhs);
                int r = *INTEGER(rhs);
                if (l == NA_INTEGER || r == NA_INTEGER)
                    real_res = NA_REAL;
                else
                    real_res = (double)l / (double)r;
                STORE_BINOP(REALSXP, 0, real_res);
            } else {
                BINOP_FALLBACK("/");
                ostack_popn(ctx, 2);
                ostack_push(ctx, res);
            }
            NEXT();
        }

        INSTRUCTION(idiv_) {
            SEXP lhs = ostack_at(ctx, 1);
            SEXP rhs = ostack_at(ctx, 0);

            if (IS_SIMPLE_SCALAR(lhs, REALSXP) &&
                IS_SIMPLE_SCALAR(rhs, REALSXP)) {
                double real_res = myfloor(*REAL(lhs), *REAL(rhs));
                STORE_BINOP(REALSXP, 0, real_res);
            } else if (IS_SIMPLE_SCALAR(lhs, REALSXP) &&
                       IS_SIMPLE_SCALAR(rhs, INTSXP)) {
                double real_res = myfloor(*REAL(lhs), (double)*INTEGER(rhs));
                STORE_BINOP(REALSXP, 0, real_res);
            } else if (IS_SIMPLE_SCALAR(lhs, INTSXP) &&
                       IS_SIMPLE_SCALAR(rhs, REALSXP)) {
                double real_res = myfloor((double)*INTEGER(lhs), *REAL(rhs));
                STORE_BINOP(REALSXP, 0, real_res);
            } else if (IS_SIMPLE_SCALAR(lhs, INTSXP) &&
                       IS_SIMPLE_SCALAR(rhs, INTSXP)) {
                int int_res;
                int l = *INTEGER(lhs);
                int r = *INTEGER(rhs);
                /* This had x %/% 0 == 0 prior to 2.14.1, but
                   it seems conventionally to be undefined */
                if (l == NA_INTEGER || r == NA_INTEGER || r == 0)
                    int_res = NA_INTEGER;
                else
                    int_res = (int)floor((double)l / (double)r);
                STORE_BINOP(INTSXP, int_res, 0);
            } else {
                BINOP_FALLBACK("%/%");
                ostack_popn(ctx, 2);
                ostack_push(ctx, res);
            }
            NEXT();
        }

        INSTRUCTION(mod_) {
            SEXP lhs = ostack_at(ctx, 1);
            SEXP rhs = ostack_at(ctx, 0);

            if (IS_SIMPLE_SCALAR(lhs, REALSXP) &&
                IS_SIMPLE_SCALAR(rhs, REALSXP)) {
                double real_res = myfmod(*REAL(lhs), *REAL(rhs));
                STORE_BINOP(REALSXP, 0, real_res);
            } else if (IS_SIMPLE_SCALAR(lhs, REALSXP) &&
                       IS_SIMPLE_SCALAR(rhs, INTSXP)) {
                double real_res = myfmod(*REAL(lhs), (double)*INTEGER(rhs));
                STORE_BINOP(REALSXP, 0, real_res);
            } else if (IS_SIMPLE_SCALAR(lhs, INTSXP) &&
                       IS_SIMPLE_SCALAR(rhs, REALSXP)) {
                double real_res = myfmod((double)*INTEGER(lhs), *REAL(rhs));
                STORE_BINOP(REALSXP, 0, real_res);
            } else if (IS_SIMPLE_SCALAR(lhs, INTSXP) &&
                       IS_SIMPLE_SCALAR(rhs, INTSXP)) {
                int int_res;
                int l = *INTEGER(lhs);
                int r = *INTEGER(rhs);
                if (l == NA_INTEGER || r == NA_INTEGER || r == 0) {
                    int_res = NA_INTEGER;
                } else {
                    int_res = (l >= 0 && r > 0)
                                  ? l % r
                                  : (int)myfmod((double)l, (double)r);
                }
                STORE_BINOP(INTSXP, int_res, 0);
            } else {
                BINOP_FALLBACK("%%");
                ostack_popn(ctx, 2);
                ostack_push(ctx, res);
            }
            NEXT();
        }

        INSTRUCTION(pow_) {
            SEXP lhs = ostack_at(ctx, 1);
            SEXP rhs = ostack_at(ctx, 0);
            BINOP_FALLBACK("^");
            ostack_popn(ctx, 2);
            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(lt_) {
            SEXP lhs = ostack_at(ctx, 1);
            SEXP rhs = ostack_at(ctx, 0);
            DO_RELOP(<);
            ostack_popn(ctx, 2);
            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(gt_) {
            SEXP lhs = ostack_at(ctx, 1);
            SEXP rhs = ostack_at(ctx, 0);
            DO_RELOP(>);
            ostack_popn(ctx, 2);
            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(le_) {
            SEXP lhs = ostack_at(ctx, 1);
            SEXP rhs = ostack_at(ctx, 0);
            DO_RELOP(<=);
            ostack_popn(ctx, 2);
            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(ge_) {
            SEXP lhs = ostack_at(ctx, 1);
            SEXP rhs = ostack_at(ctx, 0);
            DO_RELOP(>=);
            ostack_popn(ctx, 2);
            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(eq_) {
            SEXP lhs = ostack_at(ctx, 1);
            SEXP rhs = ostack_at(ctx, 0);
            DO_RELOP(==);
            ostack_popn(ctx, 2);
            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(identical_noforce_) {
            SEXP rhs = ostack_pop(ctx);
            SEXP lhs = ostack_pop(ctx);
            // This instruction does not force, but we should still compare
            // the actual promise value if it is already forced.
            // Especially important since all the inlined functions are probably
            // behind lazy loading stub promises.
            if (TYPEOF(rhs) == PROMSXP && PRVALUE(rhs) != R_UnboundValue)
                rhs = PRVALUE(rhs);
            if (TYPEOF(lhs) == PROMSXP && PRVALUE(lhs) != R_UnboundValue)
                lhs = PRVALUE(lhs);
            // Special case for closures: (level 1) deep compare with body
            // expression instead of body object, to ensure that a compiled
            // closure is equal to the uncompiled one
            if (lhs != rhs && TYPEOF(lhs) == CLOSXP && TYPEOF(rhs) == CLOSXP &&
                CLOENV(lhs) == CLOENV(rhs) && FORMALS(lhs) == FORMALS(rhs) &&
                BODY_EXPR(lhs) == BODY_EXPR(rhs))
                ostack_push(ctx, R_TrueValue);
            else
                ostack_push(ctx, rhs == lhs ? R_TrueValue : R_FalseValue);

            NEXT();
        }

        INSTRUCTION(ne_) {
            assert(R_PPStackTop >= 0);
            SEXP lhs = ostack_at(ctx, 1);
            SEXP rhs = ostack_at(ctx, 0);
            DO_RELOP(!=);
            ostack_popn(ctx, 2);
            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(not_) {
            SEXP val = ostack_at(ctx, 0);

            if (IS_SIMPLE_SCALAR(val, LGLSXP)) {
                if (*LOGICAL(val) == NA_LOGICAL) {
                    res = R_LogicalNAValue;
                } else {
                    res = *LOGICAL(val) == 0 ? R_TrueValue : R_FalseValue;
                }
            } else if (IS_SIMPLE_SCALAR(val, REALSXP)) {
                if (*REAL(val) == NA_REAL) {
                    res = R_LogicalNAValue;
                } else {
                    res = *REAL(val) == 0.0 ? R_TrueValue : R_FalseValue;
                }
            } else if (IS_SIMPLE_SCALAR(val, INTSXP)) {
                if (*INTEGER(val) == NA_INTEGER) {
                    res = R_LogicalNAValue;
                } else {
                    res = *INTEGER(val) == 0 ? R_TrueValue : R_FalseValue;
                }
            } else {
                UNOP_FALLBACK("!");
            }

            ostack_popn(ctx, 1);
            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(lgl_or_) {
            SEXP s2 = ostack_pop(ctx);
            SEXP s1 = ostack_pop(ctx);
            assert(TYPEOF(s2) == LGLSXP);
            assert(TYPEOF(s1) == LGLSXP);
            int x2 = XLENGTH(s2) == 0 ? NA_LOGICAL : LOGICAL(s2)[0];
            int x1 = XLENGTH(s1) == 0 ? NA_LOGICAL : LOGICAL(s1)[0];
            assert(x1 == 1 || x1 == 0 || x1 == NA_LOGICAL);
            assert(x2 == 1 || x2 == 0 || x2 == NA_LOGICAL);
            if (x1 == 1 || x2 == 1)
                ostack_push(ctx, R_TrueValue);
            else if (x1 == 0 && x2 == 0)
                ostack_push(ctx, R_FalseValue);
            else
                ostack_push(ctx, R_LogicalNAValue);
            NEXT();
        }

        INSTRUCTION(lgl_and_) {
            SEXP s2 = ostack_pop(ctx);
            SEXP s1 = ostack_pop(ctx);
            assert(TYPEOF(s2) == LGLSXP);
            assert(TYPEOF(s1) == LGLSXP);
            int x2 = XLENGTH(s2) == 0 ? NA_LOGICAL : LOGICAL(s2)[0];
            int x1 = XLENGTH(s1) == 0 ? NA_LOGICAL : LOGICAL(s1)[0];
            assert(x1 == 1 || x1 == 0 || x1 == NA_LOGICAL);
            assert(x2 == 1 || x2 == 0 || x2 == NA_LOGICAL);
            if (x1 == 1 && x2 == 1)
                ostack_push(ctx, R_TrueValue);
            else if (x1 == 0 || x2 == 0)
                ostack_push(ctx, R_FalseValue);
            else
                ostack_push(ctx, R_LogicalNAValue);
            NEXT();
        }

        INSTRUCTION(aslogical_) {
            SEXP val = ostack_top(ctx);
            int x1 = Rf_asLogical(val);
            assert(x1 == 1 || x1 == 0 || x1 == NA_LOGICAL);
            res = Rf_ScalarLogical(x1);
            ostack_pop(ctx);
            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(asbool_) {
            SEXP val = ostack_top(ctx);
            int cond = NA_LOGICAL;
            if (XLENGTH(val) > 1)
                Rf_warningcall(
                    getSrcAt(c, pc - 1, ctx),
                    "the condition has length > 1 and only the first "
                    "element will be used");

            if (XLENGTH(val) > 0) {
                switch (TYPEOF(val)) {
                case LGLSXP:
                    cond = LOGICAL(val)[0];
                    break;
                case INTSXP:
                    cond =
                        INTEGER(val)[0]; // relies on NA_INTEGER == NA_LOGICAL
                    break;
                default:
                    cond = Rf_asLogical(val);
                }
            }

            if (cond == NA_LOGICAL) {
                const char* msg =
                    XLENGTH(val)
                        ? (isLogical(val)
                               ? ("missing value where TRUE/FALSE needed")
                               : ("argument is not interpretable as logical"))
                        : ("argument is of length zero");
                Rf_errorcall(getSrcAt(c, pc - 1, ctx), msg);
            }

            ostack_pop(ctx);
            ostack_push(ctx, cond ? R_TrueValue : R_FalseValue);
            NEXT();
        }

        INSTRUCTION(ceil_) {
            castInt(true, c, pc, ctx);
            NEXT();
        }

        INSTRUCTION(floor_) {
            castInt(false, c, pc, ctx);
            NEXT();
        }

        INSTRUCTION(asast_) {
            SEXP val = ostack_pop(ctx);
            assert(TYPEOF(val) == PROMSXP);
            res = PRCODE(val);
            // if the code is EXTERNALSXP then it is rir Code object, get its
            // ast
            if (TYPEOF(res) == EXTERNALSXP)
                res = cp_pool_at(ctx, Code::unpack(res)->src);
            // otherwise return whatever we had, make sure we do not see
            // bytecode
            assert(TYPEOF(res) != BCODESXP);
            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(is_) {
            SEXP val = ostack_pop(ctx);
            Immediate i = readImmediate();
            advanceImmediate();
            bool res;
            switch (i) {
            case NILSXP:
            case LGLSXP:
            case REALSXP:
                res = TYPEOF(val) == i;
                break;

            case VECSXP:
                res = TYPEOF(val) == VECSXP || TYPEOF(val) == LISTSXP;
                break;

            case LISTSXP:
                res = TYPEOF(val) == LISTSXP || TYPEOF(val) == NILSXP;
                break;

            case static_cast<Immediate>(TypeChecks::RealNonObject):
                res = TYPEOF(val) == REALSXP && !isObject(val);
                break;
            case static_cast<Immediate>(TypeChecks::RealSimpleScalar):
                res = IS_SIMPLE_SCALAR(val, REALSXP);
                break;
            case static_cast<Immediate>(TypeChecks::IntegerNonObject):
                res = TYPEOF(val) == INTSXP && !isObject(val);
                break;
            case static_cast<Immediate>(TypeChecks::IntegerSimpleScalar):
                res = IS_SIMPLE_SCALAR(val, INTSXP);
                break;

            default:
                assert(false);
                res = false;
                break;
            }
            ostack_push(ctx, res ? R_TrueValue : R_FalseValue);
            NEXT();
        }

        INSTRUCTION(isobj_) {
            SEXP val = ostack_pop(ctx);
            ostack_push(ctx, isObject(val) ? R_TrueValue : R_FalseValue);
            NEXT();
        }

        INSTRUCTION(isstubenv_) {
            SEXP val = ostack_pop(ctx);
            ostack_push(ctx, LazyEnvironment::cast(val) ? R_TrueValue
                                                        : R_FalseValue);
            NEXT();
        }

        INSTRUCTION(missing_) {
            SEXP sym = readConst(ctx, readImmediate());
            advanceImmediate();
            SLOWASSERT(TYPEOF(sym) == SYMSXP);
            SLOWASSERT(!DDVAL(sym));
            assert(env);
            SEXP val = R_findVarLocInFrame(env, sym).cell;
            if (val == NULL)
                Rf_errorcall(getSrcAt(c, pc - 1, ctx),
                             "'missing' can only be used for arguments");

            if (MISSING(val) || CAR(val) == R_MissingArg) {
                ostack_push(ctx, R_TrueValue);
                NEXT();
            }

            val = CAR(val);

            if (TYPEOF(val) != PROMSXP) {
                ostack_push(ctx, R_FalseValue);
                NEXT();
            }

            val = findRootPromise(val);
            if (!isSymbol(PREXPR(val)))
                ostack_push(ctx, R_FalseValue);
            else {
                ostack_push(ctx, R_isMissing(PREXPR(val), PRENV(val))
                                     ? R_TrueValue
                                     : R_FalseValue);
            }
            NEXT();
        }

        INSTRUCTION(check_missing_) {
            SEXP val = ostack_top(ctx);
            if (val == R_MissingArg)
                Rf_error("argument is missing, with no default");
            NEXT();
        }

        INSTRUCTION(brobj_) {
            JumpOffset offset = readJumpOffset();
            advanceJump();
            if (isObject(ostack_top(ctx)))
                pc += offset;
            PC_BOUNDSCHECK(pc, c);
            NEXT();
        }

        INSTRUCTION(brtrue_) {
            JumpOffset offset = readJumpOffset();
            advanceJump();
            if (ostack_pop(ctx) == R_TrueValue) {
                pc += offset;
            }
            PC_BOUNDSCHECK(pc, c);
            NEXT();
        }

        INSTRUCTION(brfalse_) {
            JumpOffset offset = readJumpOffset();
            advanceJump();
            if (ostack_pop(ctx) == R_FalseValue) {
                pc += offset;
            }
            PC_BOUNDSCHECK(pc, c);
            NEXT();
        }

        INSTRUCTION(br_) {
            JumpOffset offset = readJumpOffset();
            advanceJump();
            pc += offset;
            PC_BOUNDSCHECK(pc, c);
            NEXT();
        }

        INSTRUCTION(extract1_1_) {
            SEXP val = ostack_at(ctx, 1);
            SEXP idx = ostack_at(ctx, 0);

            SEXP args = CONS_NR(val, CONS_NR(idx, R_NilValue));
            ostack_push(ctx, args);

            if (isObject(val)) {
                SEXP call = getSrcForCall(c, pc - 1, ctx);
                res = dispatchApply(call, val, args, symbol::Bracket, env, ctx);
                if (!res)
                    res =
                        do_subset_dflt(R_NilValue, symbol::Bracket, args, env);
            } else {
                res = do_subset_dflt(R_NilValue, symbol::Bracket, args, env);
            }

            ostack_popn(ctx, 3);

            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(extract1_2_) {
            SEXP val = ostack_at(ctx, 2);
            SEXP idx = ostack_at(ctx, 1);
            SEXP idx2 = ostack_at(ctx, 0);

            SEXP args = CONS_NR(val, CONS_NR(idx, CONS_NR(idx2, R_NilValue)));
            ostack_push(ctx, args);

            if (isObject(val)) {
                SEXP call = getSrcForCall(c, pc - 1, ctx);
                res = dispatchApply(call, val, args, symbol::Bracket, env, ctx);
                if (!res)
                    res =
                        do_subset_dflt(R_NilValue, symbol::Bracket, args, env);
            } else {
                res = do_subset_dflt(R_NilValue, symbol::Bracket, args, env);
            }

            ostack_popn(ctx, 4);

            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(extract2_1_) {
            SEXP val = ostack_at(ctx, 1);
            SEXP idx = ostack_at(ctx, 0);
            int i = -1;

            if (ATTRIB(val) != R_NilValue || ATTRIB(idx) != R_NilValue)
                goto fallback;

            switch (TYPEOF(idx)) {
            case REALSXP:
                if (STDVEC_LENGTH(idx) != 1 || *REAL(idx) == NA_REAL)
                    goto fallback;
                i = (int)*REAL(idx) - 1;
                break;
            case INTSXP:
                if (STDVEC_LENGTH(idx) != 1 || *INTEGER(idx) == NA_INTEGER)
                    goto fallback;
                i = *INTEGER(idx) - 1;
                break;
            case LGLSXP:
                if (STDVEC_LENGTH(idx) != 1 || *LOGICAL(idx) == NA_LOGICAL)
                    goto fallback;
                i = (int)*LOGICAL(idx) - 1;
                break;
            default:
                goto fallback;
            }

            if (i >= XLENGTH(val) || i < 0)
                goto fallback;

            switch (TYPEOF(val)) {

#define SIMPLECASE(vectype, vecaccess)                                         \
    case vectype: {                                                            \
        if (XLENGTH(val) == 1 && NO_REFERENCES(val)) {                         \
            res = val;                                                         \
        } else if (XLENGTH(idx) == 1 && NO_REFERENCES(idx)) {                  \
            TYPEOF(idx) = vectype;                                             \
            res = idx;                                                         \
            vecaccess(res)[0] = vecaccess(val)[i];                             \
        } else {                                                               \
            res = Rf_allocVector(vectype, 1);                                  \
            vecaccess(res)[0] = vecaccess(val)[i];                             \
        }                                                                      \
        break;                                                                 \
    }

                SIMPLECASE(REALSXP, REAL);
                SIMPLECASE(INTSXP, INTEGER);
                SIMPLECASE(LGLSXP, LOGICAL);
#undef SIMPLECASE

            case VECSXP: {
                res = VECTOR_ELT(val, i);
                break;
            }

            default:
                goto fallback;
            }

            ostack_popn(ctx, 2);
            ostack_push(ctx, res);
            R_Visible = (Rboolean) true;
            NEXT();

        // ---------
        fallback : {
            SEXP args = CONS_NR(val, CONS_NR(idx, R_NilValue));
            ostack_push(ctx, args);
            if (isObject(val)) {
                SEXP call = getSrcAt(c, pc - 1, ctx);
                res = dispatchApply(call, val, args, symbol::DoubleBracket, env,
                                    ctx);
                if (!res)
                    res =
                        do_subset2_dflt(call, symbol::DoubleBracket, args, env);
            } else {
                res = do_subset2_dflt(R_NilValue, symbol::DoubleBracket, args,
                                      env);
            }
            ostack_popn(ctx, 3);

            ostack_push(ctx, res);
            NEXT();
        }
        }

        INSTRUCTION(extract2_2_) {
            SEXP val = ostack_at(ctx, 2);
            SEXP idx = ostack_at(ctx, 1);
            SEXP idx2 = ostack_at(ctx, 0);

            SEXP args = CONS_NR(val, CONS_NR(idx, CONS_NR(idx2, R_NilValue)));
            ostack_push(ctx, args);

            if (isObject(val)) {
                SEXP call = getSrcForCall(c, pc - 1, ctx);
                res = dispatchApply(call, val, args, symbol::DoubleBracket, env,
                                    ctx);
                if (!res)
                    res =
                        do_subset2_dflt(call, symbol::DoubleBracket, args, env);
            } else {
                res = do_subset2_dflt(R_NilValue, symbol::DoubleBracket, args,
                                      env);
            }
            ostack_popn(ctx, 4);

            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(subassign1_1_) {
            SEXP idx = ostack_at(ctx, 0);
            SEXP vec = ostack_at(ctx, 1);
            SEXP val = ostack_at(ctx, 2);

            // Destructively modifies TOS, even if the refcount is 1. This is
            // intended, to avoid copying. Care need to be taken if `vec` is
            // used multiple times as a temporary.
            if (MAYBE_SHARED(vec)) {
                vec = Rf_duplicate(vec);
                ostack_set(ctx, 1, vec);
            }

            SEXP args = CONS_NR(vec, CONS_NR(idx, CONS_NR(val, R_NilValue)));
            SET_TAG(CDDR(args), symbol::value);
            PROTECT(args);

            res = nullptr;
            SEXP call = getSrcForCall(c, pc - 1, ctx);
            RCNTXT assignContext;
            Rf_begincontext(&assignContext, CTXT_RETURN, call, env, ENCLOS(env),
                            args, symbol::AssignBracket);
            if (isObject(vec)) {
                res = dispatchApply(call, vec, args, symbol::AssignBracket, env,
                                    ctx);
            }
            if (!res) {
                res = do_subassign_dflt(call, symbol::AssignBracket, args, env);
                // We duplicated the vector above, and there is a stvar
                // following
                SET_NAMED(res, 0);
            }
            Rf_endcontext(&assignContext);
            ostack_popn(ctx, 3);
            UNPROTECT(1);

            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(subassign1_2_) {
            SEXP idx2 = ostack_at(ctx, 0);
            SEXP idx1 = ostack_at(ctx, 1);
            SEXP mtx = ostack_at(ctx, 2);
            SEXP val = ostack_at(ctx, 3);

            // Destructively modifies TOS, even if the refcount is 1. This is
            // intended, to avoid copying. Care need to be taken if `vec` is
            // used multiple times as a temporary.
            if (MAYBE_SHARED(mtx)) {
                mtx = Rf_duplicate(mtx);
                ostack_set(ctx, 2, mtx);
            }

            SEXP args = CONS_NR(
                mtx, CONS_NR(idx1, CONS_NR(idx2, CONS_NR(val, R_NilValue))));
            SET_TAG(CDDDR(args), symbol::value);
            PROTECT(args);

            res = nullptr;
            SEXP call = getSrcForCall(c, pc - 1, ctx);
            RCNTXT assignContext;
            Rf_begincontext(&assignContext, CTXT_RETURN, call, env, ENCLOS(env),
                            args, symbol::AssignBracket);
            if (isObject(mtx)) {
                res = dispatchApply(call, mtx, args, symbol::AssignBracket, env,
                                    ctx);
            }

            if (!res) {
                res = do_subassign_dflt(call, symbol::AssignBracket, args, env);
                // We duplicated the matrix above, and there is a stvar
                // following
                SET_NAMED(res, 0);
            }
            Rf_endcontext(&assignContext);
            ostack_popn(ctx, 4);
            UNPROTECT(1);

            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(subassign2_1_) {
            SEXP idx = ostack_at(ctx, 0);
            SEXP vec = ostack_at(ctx, 1);
            SEXP val = ostack_at(ctx, 2);

            // Fast case
            if (NOT_SHARED(vec) && !isObject(vec)) {
                SEXPTYPE vectorT = TYPEOF(vec);
                SEXPTYPE valT = TYPEOF(val);
                SEXPTYPE idxT = TYPEOF(idx);

                // Fast case only if
                // 1. index is numerical and scalar
                // 2. vector is real and shape of value fits into real
                //      or vector is int and shape of value is int
                //      or vector is generic
                // 3. value fits into one cell of the vector
                if ((idxT == INTSXP || idxT == REALSXP) &&
                    (XLENGTH(idx) == 1) && // 1
                    ((vectorT == REALSXP &&
                      (valT == REALSXP || valT == INTSXP)) || // 2
                     (vectorT == INTSXP && (valT == INTSXP)) ||
                     (vectorT == VECSXP)) &&
                    (XLENGTH(val) == 1 || vectorT == VECSXP)) { // 3

                    int idx_ = -1;

                    if (idxT == REALSXP) {
                        if (*REAL(idx) != NA_REAL)
                            idx_ = (int)*REAL(idx) - 1;
                    } else {
                        if (*INTEGER(idx) != NA_INTEGER)
                            idx_ = *INTEGER(idx) - 1;
                    }

                    if (idx_ >= 0 && idx_ < XLENGTH(vec)) {
                        switch (vectorT) {
                        case REALSXP:
                            REAL(vec)
                            [idx_] = valT == REALSXP ? *REAL(val)
                                                     : (double)*INTEGER(val);
                            break;
                        case INTSXP:
                            INTEGER(vec)[idx_] = *INTEGER(val);
                            break;
                        case VECSXP:
                            // Avoid recursive vectors
                            if (val == vec)
                                val = Rf_shallow_duplicate(val);
                            SET_VECTOR_ELT(vec, idx_, val);
                            break;
                        }
                        ostack_popn(ctx, 3);

                        ostack_push(ctx, vec);
                        NEXT();
                    }
                }
            }

            // Destructively modifies TOS, even if the refcount is 1. This is
            // intended, to avoid copying. Care need to be taken if `vec` is
            // used multiple times as a temporary.
            if (MAYBE_SHARED(vec)) {
                vec = Rf_duplicate(vec);
                ostack_set(ctx, 1, vec);
            }

            SEXP args = CONS_NR(vec, CONS_NR(idx, CONS_NR(val, R_NilValue)));
            SET_TAG(CDDR(args), symbol::value);
            PROTECT(args);

            res = nullptr;
            SEXP call = getSrcForCall(c, pc - 1, ctx);

            RCNTXT assignContext;
            Rf_begincontext(&assignContext, CTXT_RETURN, call, env, ENCLOS(env),
                            args, symbol::AssignDoubleBracket);
            if (isObject(vec)) {
                res = dispatchApply(call, vec, args,
                                    symbol::AssignDoubleBracket, env, ctx);
            }

            if (!res) {
                res = do_subassign2_dflt(call, symbol::AssignDoubleBracket,
                                         args, env);
                // We duplicated the vector above, and there is a stvar
                // following
                SET_NAMED(res, 0);
            }
            Rf_endcontext(&assignContext);
            ostack_popn(ctx, 3);
            UNPROTECT(1);

            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(subassign2_2_) {
            SEXP idx2 = ostack_at(ctx, 0);
            SEXP idx1 = ostack_at(ctx, 1);
            SEXP mtx = ostack_at(ctx, 2);
            SEXP val = ostack_at(ctx, 3);

            // Fast case
            if (NOT_SHARED(mtx) && !isObject(mtx)) {
                SEXPTYPE matrixT = TYPEOF(mtx);
                SEXPTYPE valT = TYPEOF(val);
                SEXPTYPE idx1T = TYPEOF(idx1);
                SEXPTYPE idx2T = TYPEOF(idx2);

                // Fast case only if
                // 1. index is numerical and scalar
                // 2. matrix is real and shape of value fits into real
                //      or matrix is int and shape of value is int
                //      or matrix is generic
                // 3. value fits into one cell of the matrix
                if ((idx1T == INTSXP || idx1T == REALSXP) &&
                    (XLENGTH(idx1) == 1) && // 1
                    (idx2T == INTSXP || idx2T == REALSXP) &&
                    (XLENGTH(idx2) == 1) &&
                    ((matrixT == REALSXP &&
                      (valT == REALSXP || valT == INTSXP)) || // 2
                     (matrixT == INTSXP && (valT == INTSXP)) ||
                     (matrixT == VECSXP)) &&
                    (XLENGTH(val) == 1 || matrixT == VECSXP)) { // 3

                    int idx1_ = -1;
                    int idx2_ = -1;

                    if (idx1T == REALSXP) {
                        if (*REAL(idx1) != NA_REAL)
                            idx1_ = (int)*REAL(idx1) - 1;
                    } else {
                        if (*INTEGER(idx1) != NA_INTEGER)
                            idx1_ = *INTEGER(idx1) - 1;
                    }

                    if (idx2T == REALSXP) {
                        if (*REAL(idx2) != NA_REAL)
                            idx2_ = (int)*REAL(idx1) - 1;
                    } else {
                        if (*INTEGER(idx2) != NA_INTEGER)
                            idx2_ = *INTEGER(idx2) - 1;
                    }

                    if (idx1_ >= 0 && idx1_ < Rf_ncols(mtx) && idx2_ >= 0 &&
                        idx2_ < Rf_nrows(mtx)) {
                        int idx_ = idx1_ + (idx2_ * Rf_nrows(mtx));
                        SEXPTYPE mtxT = TYPEOF(mtx);
                        switch (mtxT) {
                        case REALSXP:
                            REAL(mtx)
                            [idx_] = valT == REALSXP ? *REAL(val)
                                                     : (double)*INTEGER(val);
                            break;
                        case INTSXP:
                            INTEGER(mtx)[idx_] = *INTEGER(val);
                            break;
                        case VECSXP:
                            // Avoid recursive vectors
                            if (val == mtx)
                                val = Rf_shallow_duplicate(val);
                            SET_VECTOR_ELT(mtx, idx_, val);
                            break;
                        }
                        ostack_popn(ctx, 4);

                        ostack_push(ctx, mtx);
                        NEXT();
                    }
                }
            }

            // Destructively modifies TOS, even if the refcount is 1. This is
            // intended, to avoid copying. Care need to be taken if `vec` is
            // used multiple times as a temporary.
            if (MAYBE_SHARED(mtx)) {
                mtx = Rf_duplicate(mtx);
                ostack_set(ctx, 2, mtx);
            }

            SEXP args = CONS_NR(
                mtx, CONS_NR(idx1, CONS_NR(idx2, CONS_NR(val, R_NilValue))));
            SET_TAG(CDDDR(args), symbol::value);
            PROTECT(args);

            res = nullptr;
            SEXP call = getSrcForCall(c, pc - 1, ctx);
            RCNTXT assignContext;
            Rf_begincontext(&assignContext, CTXT_RETURN, call, env, ENCLOS(env),
                            args, symbol::AssignDoubleBracket);
            if (isObject(mtx)) {
                res = dispatchApply(call, mtx, args,
                                    symbol::AssignDoubleBracket, env, ctx);
            }

            if (!res) {
                res = do_subassign2_dflt(call, symbol::AssignDoubleBracket,
                                         args, env);
                // We duplicated the matrix above, and there is a stvar
                // following
                SET_NAMED(res, 0);
            }
            Rf_endcontext(&assignContext);
            ostack_popn(ctx, 4);
            UNPROTECT(1);

            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(guard_fun_) {
            SEXP sym = readConst(ctx, readImmediate());
            advanceImmediate();
            res = readConst(ctx, readImmediate());
            advanceImmediate();
            advanceImmediate();
#ifndef UNSOUND_OPTS
            if (res != Rf_findFun(sym, env))
                Rf_error("Invalid Callee");
#endif
            NEXT();
        }

        INSTRUCTION(deopt_) {
            SEXP r = readConst(ctx, readImmediate());
            advanceImmediate();
            assert(TYPEOF(r) == RAWSXP);
            assert(XLENGTH(r) >= (int)sizeof(DeoptMetadata));
            auto m = (DeoptMetadata*)DATAPTR(r);

#if 0
            size_t pos = 0;
            for (size_t i = 0; i < m->numFrames; ++i) {
                std::cout << "Code " << m->frames[i].code << "\n";
                std::cout << "Frame " << i << ":\n";
                std::cout << "  - env (" << pos << ")\n";
                Rf_PrintValue(ostack_at(ctx, pos++));
                for( size_t j = 0; j < m->frames[i].stackSize; ++j) {
                    std::cout << "  - stack (" << pos << ") " << j << "\n";
                    Rf_PrintValue(ostack_at(ctx, pos++));
                }
            }
#endif

            if (!pir::Parameter::DEOPT_CHAOS) {
                // TODO: this version is still reachable from static call inline
                // caches. Thus we need to preserve it forever. We need some
                // dependency management here.
                Pool::insert(c->container());
                // remove the deoptimized function. Unless on deopt chaos,
                // always recompiling would just blow testing time...
                auto dt = DispatchTable::unpack(BODY(callCtxt->callee));
                dt->remove(c);
            }
            assert(m->numFrames >= 1);
            size_t stackHeight = 0;
            for (size_t i = 0; i < m->numFrames; ++i)
                stackHeight += m->frames[i].stackSize + 1;
            deoptFramesWithContext(ctx, callCtxt, m, R_NilValue,
                                   m->numFrames - 1, stackHeight, true);
            assert(false);
        }

        INSTRUCTION(seq_) {
            static SEXP prim = NULL;
            if (!prim) {
                // TODO: we could call seq.default here, but it messes up the
                // error call :(
                prim = Rf_findFun(Rf_install("seq"), R_GlobalEnv);
            }

            // TODO: add a real guard here...
            assert(prim == Rf_findFun(Rf_install("seq"), env));

            SEXP from = ostack_at(ctx, 2);
            SEXP to = ostack_at(ctx, 1);
            SEXP by = ostack_at(ctx, 0);
            res = NULL;

            if (IS_SIMPLE_SCALAR(from, INTSXP) &&
                IS_SIMPLE_SCALAR(to, INTSXP) && IS_SIMPLE_SCALAR(by, INTSXP)) {
                int f = *INTEGER(from);
                int t = *INTEGER(to);
                int b = *INTEGER(by);
                if (f != NA_INTEGER && t != NA_INTEGER && b != NA_INTEGER) {
                    if ((f < t && b > 0) || (t < f && b < 0)) {
                        int size = 1 + (t - f) / b;
                        res = Rf_allocVector(INTSXP, size);
                        int v = f;
                        for (int i = 0; i < size; ++i) {
                            INTEGER(res)[i] = v;
                            v += b;
                        }
                    } else if (f == t) {
                        res = Rf_allocVector(INTSXP, 1);
                        *INTEGER(res) = f;
                    }
                }
            }

            if (!res) {
                SLOWASSERT(!isObject(from));
                SEXP call = getSrcForCall(c, pc - 1, ctx);
                SEXP argslist =
                    CONS_NR(from, CONS_NR(to, CONS_NR(by, R_NilValue)));
                ostack_push(ctx, argslist);
                res = Rf_applyClosure(call, prim, argslist, env, R_NilValue);
                ostack_pop(ctx);
            }

            ostack_popn(ctx, 3);
            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(colon_) {

            SEXP lhs = ostack_at(ctx, 1);
            SEXP rhs = ostack_at(ctx, 0);
            res = NULL;

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
                        INT_MIN <= from && INT_MAX >= from &&
                        from == (int)from) {
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
                BINOP_FALLBACK(":");
            }

            ostack_popn(ctx, 2);
            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(names_) {
            ostack_push(ctx, Rf_getAttrib(ostack_pop(ctx), R_NamesSymbol));
            NEXT();
        }

        INSTRUCTION(set_names_) {
            SEXP val = ostack_pop(ctx);
            if (!isNull(val))
                Rf_setAttrib(ostack_top(ctx), R_NamesSymbol, val);
            NEXT();
        }

        INSTRUCTION(alloc_) {
            SEXP val = ostack_pop(ctx);
            assert(TYPEOF(val) == INTSXP);
            int type = readSignedImmediate();
            advanceImmediate();
            res = Rf_allocVector(type, INTEGER(val)[0]);
            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(length_) {
            SEXP val = ostack_pop(ctx);
            R_xlen_t len = XLENGTH(val);
            ostack_push(ctx, Rf_allocVector(INTSXP, 1));
            INTEGER(ostack_top(ctx))[0] = len;
            NEXT();
        }

        INSTRUCTION(for_seq_size_) {
            SEXP seq = ostack_at(ctx, 0);
            // TODO: we should extract the length just once at the begining of
            // the loop and generally have somthing more clever here...
            SEXP value = Rf_allocVector(INTSXP, 1);
            if (Rf_isVector(seq)) {
                INTEGER(value)[0] = LENGTH(seq);
            } else if (Rf_isList(seq) || isNull(seq)) {
                INTEGER(value)[0] = Rf_length(seq);
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
            ostack_push(ctx, value);
            NEXT();
        }

        INSTRUCTION(visible_) {
            R_Visible = TRUE;
            NEXT();
        }

        INSTRUCTION(invisible_) {
            R_Visible = FALSE;
            NEXT();
        }

        INSTRUCTION(ensure_named_) {
            SEXP val = ostack_top(ctx);
            ENSURE_NAMED(val);
            NEXT();
        }

        INSTRUCTION(set_shared_) {
            SEXP val = ostack_top(ctx);
            if (NAMED(val) < 2)
                SET_NAMED(val, 2);
            NEXT();
        }

        INSTRUCTION(beginloop_) {
            SLOWASSERT(env);
            int offset = readJumpOffset();
            advanceJump();
            loopTrampoline(c, ctx, env, callCtxt, pc, localsBase);
            pc += offset;
            assert(*pc == Opcode::endloop_);
            advanceOpcode();
            NEXT();
        }

        INSTRUCTION(endloop_) { return loopTrampolineMarker; }

        INSTRUCTION(return_) {
            res = ostack_top(ctx);
            // this restores stack pointer to the value from the target context
            Rf_findcontext(CTXT_BROWSER | CTXT_FUNCTION, env, res);
            // not reached
            assert(false);
        }

        INSTRUCTION(ret_) { goto eval_done; }

        INSTRUCTION(int3_) {
            asm("int3");
            NEXT();
        }

        INSTRUCTION(printInvocation_) {
            printf("Invocation count: %d\n", c->funInvocationCount);
            NEXT();
        }

        LASTOP;
    }

eval_done:
    return ostack_pop(ctx);
}

#pragma GCC diagnostic pop

SEXP evalRirCodeExtCaller(Code* c, InterpreterInstance* ctx, SEXP env) {
    return evalRirCode(c, ctx, env, nullptr);
}

SEXP evalRirCode(Code* c, InterpreterInstance* ctx, SEXP env,
                 const CallContext* callCtxt) {
    return evalRirCode(c, ctx, env, callCtxt, nullptr);
}

SEXP rirExpr(SEXP s) {
    if (auto c = Code::check(s)) {
        return src_pool_at(globalContext(), c->src);
    }
    if (auto f = Function::check(s)) {
        return src_pool_at(globalContext(), f->body()->src);
    }
    if (auto t = DispatchTable::check(s)) {
        // Default is the source of the first function in the dispatch table
        Function* f = t->baseline();
        return src_pool_at(globalContext(), f->body()->src);
    }
    return s;
}

SEXP rirApplyClosure(SEXP ast, SEXP op, SEXP arglist, SEXP rho,
                     SEXP suppliedvars) {
    auto ctx = globalContext();

    RList args(arglist);
    size_t nargs = 0;
    std::vector<Immediate> names;
    for (auto arg = args.begin(), end = args.end(); arg != end; ++arg) {
        ostack_push(ctx, *arg);
        if (arg.hasTag()) {
            names.resize(nargs + 1);
            names[nargs] = Pool::insert(arg.tag());
        }
        nargs++;
    }
    if (!names.empty()) {
        names.resize(nargs);
    }
    // Add extra arguments from object dispatching
    if (suppliedvars != R_NilValue) {
        auto extra = RList(suppliedvars);
        for (auto a = extra.begin(); a != extra.end(); ++a) {
            if (a.hasTag()) {
                auto var = Pool::insert(a.tag());
                if (std::find(names.begin(), names.end(), var) == names.end()) {
                    ostack_push(ctx, *a);
                    names.resize(nargs + 1);
                    names[nargs] = var;
                    nargs++;
                }
            }
        }
    }

    CallContext call(nullptr, op, nargs, ast, ostack_cell_at(ctx, nargs - 1),
                     nullptr, names.empty() ? nullptr : names.data(), rho,
                     Assumptions(), ctx);
    call.arglist = arglist;
    call.safeForceArgs();

    auto res = rirCall(call, ctx);
    ostack_popn(ctx, call.passedArgs);
    return res;
}

SEXP rirEval_f(SEXP what, SEXP env) {
    assert(TYPEOF(what) == EXTERNALSXP);

    // TODO: do we not need an RCNTXT here?

    if (auto code = Code::check(what)) {
        return evalRirCodeExtCaller(code, globalContext(), env);
    }

    if (auto table = DispatchTable::check(what)) {
        // TODO: add an adapter frame to be able to call something else than
        // the baseline version!
        Function* fun = table->baseline();
        fun->registerInvocation();

        return evalRirCodeExtCaller(fun->body(), globalContext(), env);
    }

    if (auto fun = Function::check(what)) {
        fun->registerInvocation();
        return evalRirCodeExtCaller(fun->body(), globalContext(), env);
    }

    assert(false && "Expected a code object or a dispatch table");
    return nullptr;
}
} // namespace rir
