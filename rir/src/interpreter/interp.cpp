#include "interp.h"
#include "ArgsLazyData.h"
#include "LazyEnvironment.h"
#include "R/Funtab.h"
#include "R/RList.h"
#include "R/Symbols.h"
#include "compiler/translations/rir_2_pir/rir_2_pir_compiler.h"
#include "ir/Deoptimization.h"
#include "ir/RuntimeFeedback_inl.h"
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
#define INSTRUCTION(name)                                                      \
    op_##name: /* debug(c, pc, #name, ostackLength(ctx) - bp, ctx); */
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
        /* debug(c, pc, #name, ostackLength(ctx) - bp, ctx); */
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
        SLOWASSERT(TYPEOF(promise) != PROMSXP);
        return promise;
    } else {
        SEXP res = forcePromise(promise);
        SLOWASSERT(TYPEOF(res) != PROMSXP && "promise returned promise");
        return res;
    }
}

static void jit(SEXP cls, SEXP name, InterpreterInstance* ctx) {
    SLOWASSERT(TYPEOF(cls) == CLOSXP);
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
        if (ostackSexpAt(ctx, i) == stub)
            ostackSetSexp(ctx, i, environment);
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

        PROTECT(name);
        PROTECT(result);
        SEXP arg = call.stackArgSexp(i, ctx);
        UNPROTECT(2);

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
                        SLOWASSERT(TYPEOF(arg) != PROMSXP);
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
                SLOWASSERT(TYPEOF(arg) != PROMSXP);
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
}

SEXP* keepAliveSEXPs(void* rirDataWrapper) {
    if (auto env = LazyEnvironment::cast(rirDataWrapper)) {
        return env->gcData();
    }
    assert(false);
}

SEXP lazyPromargsCreation(void* rirDataWrapper) {
    return ArgsLazyData::cast(rirDataWrapper)->createArgsLists();
}

SEXP lazyEnvCreation(void* rirDataWrapper) {
    return LazyEnvironment::cast(rirDataWrapper)->create();
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

R_bcstack_t evalRirCode(Code*, InterpreterInstance*, SEXP, const CallContext*,
                        Opcode*, R_bcstack_t* = nullptr);
static SEXP rirCallTrampoline_(RCNTXT& cntxt, const CallContext& call,
                               Code* code, SEXP env, InterpreterInstance* ctx) {
    int trampIn = ostackLength(ctx);
    if ((SETJMP(cntxt.cjmpbuf))) {
        assert(trampIn == ostackLength(ctx));
        if (R_ReturnedValue == R_RestartToken) {
            cntxt.callflag = CTXT_RETURN; /* turn restart off */
            R_ReturnedValue = R_NilValue; /* remove restart token */
            return stackObjToSexp(evalRirCode(code, ctx, cntxt.cloenv, &call));
        } else {
            return R_ReturnedValue;
        }
    }
    return stackObjToSexp(evalRirCode(code, ctx, env, &call));
}

static RIR_INLINE SEXP rirCallTrampoline(const CallContext& call, Function* fun,
                                         SEXP env, SEXP arglist,
                                         InterpreterInstance* ctx) {
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

R_bcstack_t evalRirCode(Code*, InterpreterInstance*, SEXP, const CallContext*,
                        Opcode*, R_bcstack_t*);
static void loopTrampoline(Code* c, InterpreterInstance* ctx, SEXP env,
                           const CallContext* callCtxt, Opcode* pc,
                           R_bcstack_t* localsBase) {
    SLOWASSERT(env);

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
    SEXP res =
        stackObjToSexp(evalRirCode(c, ctx, env, callCtxt, pc, localsBase));
    assert(res == loopTrampolineMarker);
    Rf_endcontext(&cntxt);
}

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
                return stackObjToSexp(
                    evalRirCode(c, ctx, cntxt.cloenv, callCtx, pc));
            } else {
                return R_ReturnedValue;
            }
        }
        return stackObjToSexp(
            evalRirCode(c, ctx, sysparent, callCtx, pc, localsBase));
    };

    // execute the inlined function
    auto res = trampoline();
    endClosureContext(&cntxt, res);
    return res;
}

static RIR_INLINE SEXP legacySpecialCall(const CallContext& call,
                                         InterpreterInstance* ctx) {
    SLOWASSERT(call.ast != R_NilValue);
    SLOWASSERT(TYPEOF(call.callerEnv) == ENVSXP);

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

    SLOWASSERT(TYPEOF(call.callee) == CLOSXP &&
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
                SLOWASSERT(c != nullptr &&
                           "No more compiled formals available.");
                SETCAR(a, createPromise(c, newrho));
                SET_MISSING(a, 2);
            }
            // Either just used the compiled formal or it was not needed.
            // Skip over current compiled formal and find the next default arg.
        }
        SLOWASSERT(CAR(f) != R_DotsSymbol || TYPEOF(CAR(a)) == DOTSXP);
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

static void addDynamicAssumptionsFromContext(CallContext& call,
                                             InterpreterInstance* ctx) {
    Assumptions& given = call.givenAssumptions;

    if (!call.hasNames())
        given.add(Assumption::CorrectOrderOfArguments);

    given.add(Assumption::NoExplicitlyMissingArgs);
    if (call.hasStackArgs()) {
        // Always true in this case, since we will pad missing args on the stack
        // later with R_MissingArg's
        given.add(Assumption::NotTooFewArguments);

        auto testArg = [&](size_t i) {
            R_bcstack_t arg = call.stackArg(i, ctx);
            bool notObj = true;
            bool isEager = true;
            if (stackObjSexpType(arg) == PROMSXP) {
                if (PRVALUE(arg.u.sxpval) == R_UnboundValue) {
                    notObj = false;
                    isEager = false;
                } else if (isObject(PRVALUE(arg.u.sxpval))) {
                    notObj = false;
                }
            } else if (arg.tag == STACK_OBJ_SEXP && isObject(arg.u.sxpval)) {
                notObj = false;
            } else if (arg.tag == STACK_OBJ_SEXP && arg.u.sxpval == R_MissingArg) {
                given.remove(Assumption::NoExplicitlyMissingArgs);
            }
            if (isEager)
                given.setEager(i);
            if (notObj)
                given.setNotObj(i);
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

    SLOWASSERT(signature.envCreation ==
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
    SLOWASSERT(call.hasStackArgs());
    if (signature.expectedNargs() > call.suppliedArgs) {
        for (size_t i = 0; i < signature.expectedNargs() - call.suppliedArgs;
             ++i)
            ostackPushSexp(ctx, R_MissingArg);
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
    SLOWASSERT(fun);

    return fun;
};

static unsigned PIR_WARMUP =
    getenv("PIR_WARMUP") ? atoi(getenv("PIR_WARMUP")) : 3;

// Call a RIR function. Arguments are still untouched.
RIR_INLINE SEXP rirCall(CallContext& call, InterpreterInstance* ctx) {
    SEXP body = BODY(call.callee);
    SLOWASSERT(DispatchTable::check(body));

    auto table = DispatchTable::unpack(body);

    addDynamicAssumptionsFromContext(call, ctx);
    Function* fun = dispatch(call, table);
    fun->registerInvocation();

    if (!fun->unoptimizable && fun->invocationCount() % PIR_WARMUP == 0) {
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

    SLOWASSERT(result);

    SLOWASSERT(!fun->deopt);
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
            R_bcstack_t arg = call.stackArg(0, ctx);
            if (arg.tag == STACK_OBJ_SEXP) {
                if (TYPEOF(arg.u.sxpval) == PROMSXP)
                    arg.u.sxpval = PRVALUE(arg.u.sxpval);
                if (arg.u.sxpval == R_UnboundValue)
                    message << "arg0 lazy";
                else if (arg.u.sxpval == R_MissingArg)
                    message << "arg0 missing";
                else
                    message << " arg0 : " << type2char(TYPEOF(arg.u.sxpval))
                            << " a " << (ATTRIB(arg.u.sxpval) != R_NilValue);
            }
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

static RIR_INLINE R_bcstack_t builtinCall(CallContext& call,
                                          InterpreterInstance* ctx) {
    if (call.hasStackArgs() && !call.hasNames()) {
        R_bcstack_t res = tryFastBuiltinCall(call, ctx);
        if (!stackObjIsNull(res))
            return res;
#ifdef DEBUG_SLOWCASES
        SLOWCASE_COUNTER.count("builtin", call, ctx);
#endif
    }
    return sexpToStackObj(legacyCall(call, ctx));
}

static RIR_INLINE R_bcstack_t specialCall(CallContext& call,
                                          InterpreterInstance* ctx) {
    if (call.hasStackArgs() && !call.hasNames()) {
        R_bcstack_t res = tryFastSpecialCall(call, ctx);
        if (!stackObjIsNull(res))
            return res;
#ifdef DEBUG_SLOWCASES
        SLOWCASE_COUNTER.count("special", call, ctx);
#endif
    }
    return sexpToStackObj(legacySpecialCall(call, ctx));
}

static R_bcstack_t doCall(CallContext& call, InterpreterInstance* ctx) {
    SLOWASSERT(call.callee);

    switch (TYPEOF(call.callee)) {
    case SPECIALSXP:
        return specialCall(call, ctx);
    case BUILTINSXP:
        return builtinCall(call, ctx);
    case CLOSXP: {
        if (TYPEOF(BODY(call.callee)) != EXTERNALSXP)
            return sexpToStackObj(legacyCall(call, ctx));
        return sexpToStackObj(rirCall(call, ctx));
    }
    default:
        Rf_error("Invalid Callee");
    };
    return sexpToStackObj(R_NilValue);
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

static R_INLINE int RInteger_plus(int x, int y, Rboolean* pnaflag) {
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

static R_INLINE int RInteger_minus(int x, int y, Rboolean* pnaflag) {
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
static R_INLINE int RInteger_times(int x, int y, Rboolean* pnaflag) {
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

#define INTEGER_OVERFLOW_WARNING "NAs produced by integer overflow"

#define CHECK_INTEGER_OVERFLOW(naflag)                                         \
    do {                                                                       \
        if (naflag) {                                                          \
            SEXP call = getSrcForCall(c, pc - 1, ctx);                         \
            Rf_warningcall(call, INTEGER_OVERFLOW_WARNING);                    \
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
        SEXP call = getSrcForCall(c, pc - 1, ctx);                             \
        PROTECT(call);                                                         \
        SEXP lhsSexp = ostackObjToSexpAt(lhs, ctx, 1);                         \
        PROTECT(lhsSexp);                                                      \
        SEXP rhsSexp = ostackObjToSexpAt(rhs, ctx, 0);                         \
        SEXP argslist = CONS_NR(lhsSexp, CONS_NR(rhsSexp, R_NilValue));        \
        UNPROTECT(2);                                                          \
        ostackPushSexp(ctx, argslist);                                         \
        if (flag < 2)                                                          \
            R_Visible = static_cast<Rboolean>(flag != 1);                      \
        SEXP res = blt(call, prim, argslist, env);                             \
        if (flag < 2)                                                          \
            R_Visible = static_cast<Rboolean>(flag != 1);                      \
        ostackPop(ctx);                                                        \
        STORE_BINOP(sexpToStackObj(res));                                      \
    } while (false)

#define STORE_BINOP(res)                                                       \
    do {                                                                       \
        ostackPop(ctx);                                                        \
        if (res.tag != STACK_OBJ_SEXP && lhs.tag == STACK_OBJ_SEXP &&          \
            trySetInPlace(lhs.u.sxpval, res)) {                                \
        } else if (res.tag != STACK_OBJ_SEXP && rhs.tag == STACK_OBJ_SEXP &&   \
                   trySetInPlace(rhs.u.sxpval, res)) {                         \
            ostackSet(ctx, 0, rhs);                                            \
        } else {                                                               \
            ostackSet(ctx, 0, res);                                            \
        }                                                                      \
    } while (false)

#define STORE_BINOP_FAST(res, l, r, Type, TYPE)                                \
    do {                                                                       \
        ostackPop(ctx);                                                        \
        if (l && lhs.tag == STACK_OBJ_SEXP && NO_REFERENCES(lhs.u.sxpval)) {   \
            *TYPE(lhs.u.sxpval) = res;                                         \
        } else if (r && rhs.tag == STACK_OBJ_SEXP &&                           \
                   NO_REFERENCES(rhs.u.sxpval)) {                              \
            *TYPE(rhs.u.sxpval) = res;                                         \
            ostackSet(ctx, 0, rhs);                                            \
        } else {                                                               \
            ostackSet##Type(ctx, 0, res);                                      \
        }                                                                      \
    } while (false)

#define DO_BINOP(op, Op2)                                                      \
    do {                                                                       \
        if (stackObjIsSimpleScalar(lhs, REALSXP)) {                            \
            if (stackObjIsSimpleScalar(rhs, REALSXP)) {                        \
                double l = tryStackObjToReal(lhs);                             \
                double r = tryStackObjToReal(rhs);                             \
                double real_res =                                              \
                    (l == NA_REAL || r == NA_REAL) ? NA_REAL : l op r;         \
                STORE_BINOP_FAST(real_res, true, true, Real, REAL);            \
            } else if (stackObjIsSimpleScalar(rhs, INTSXP)) {                  \
                double l = tryStackObjToReal(lhs);                             \
                int r = tryStackObjToInteger(rhs);                             \
                double real_res =                                              \
                    (l == NA_REAL || r == NA_INTEGER) ? NA_REAL : l op r;      \
                STORE_BINOP_FAST(real_res, true, false, Real, REAL);           \
            } else {                                                           \
                BINOP_FALLBACK(#op);                                           \
            }                                                                  \
        } else if (stackObjIsSimpleScalar(lhs, INTSXP)) {                      \
            if (stackObjIsSimpleScalar(rhs, INTSXP)) {                         \
                int l = tryStackObjToInteger(lhs);                             \
                int r = tryStackObjToInteger(rhs);                             \
                Rboolean naflag = FALSE;                                       \
                int int_res = Op2(l, r, &naflag);                              \
                CHECK_INTEGER_OVERFLOW(naflag);                                \
                STORE_BINOP_FAST(int_res, true, true, Int, INTEGER);           \
            } else if (stackObjIsSimpleScalar(rhs, REALSXP)) {                 \
                int l = tryStackObjToInteger(lhs);                             \
                double r = tryStackObjToReal(rhs);                             \
                double real_res =                                              \
                    (l == NA_INTEGER || r == NA_REAL) ? NA_REAL : l op r;      \
                STORE_BINOP_FAST(real_res, false, true, Real, REAL);           \
            } else {                                                           \
                BINOP_FALLBACK(#op);                                           \
            }                                                                  \
        } else {                                                               \
            BINOP_FALLBACK(#op);                                               \
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

static R_INLINE int RInteger_uplus(int x, Rboolean* pnaflag) {
    if (x == NA_INTEGER)
        return NA_INTEGER;

    return x;
}

static R_INLINE int RInteger_uminus(int x, Rboolean* pnaflag) {
    if (x == NA_INTEGER)
        return NA_INTEGER;

    return -x;
}

#define STORE_UNOP(res)                                                        \
    do {                                                                       \
        if (res.tag != STACK_OBJ_SEXP && val.tag == STACK_OBJ_SEXP &&          \
            trySetInPlace(val.u.sxpval, res)) {                                \
            ;                                                                  \
        } else {                                                               \
            ostackSet(ctx, 0, res);                                            \
        }                                                                      \
    } while (false)

#define STORE_UNOP_FAST(res, Type, TYPE)                                       \
    do {                                                                       \
        if (val.tag == STACK_OBJ_SEXP && NO_REFERENCES(val.u.sxpval)) {        \
            *TYPE(val.u.sxpval) = res;                                         \
        } else {                                                               \
            ostackSet##Type(ctx, 0, res);                                      \
        }                                                                      \
    } while (false)

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
        PROTECT(call);                                                         \
        SEXP valSexp = ostackObjToSexpAt(val, ctx, 0);                         \
        PROTECT(valSexp);                                                      \
        SEXP argslist = CONS_NR(valSexp, R_NilValue);                          \
        UNPROTECT(2);                                                          \
        ostackPushSexp(ctx, argslist);                                         \
        if (flag < 2)                                                          \
            R_Visible = static_cast<Rboolean>(flag != 1);                      \
        SEXP res = blt(call, prim, argslist, env);                             \
        if (flag < 2)                                                          \
            R_Visible = static_cast<Rboolean>(flag != 1);                      \
        ostackPop(ctx);                                                        \
        STORE_UNOP(sexpToStackObj(res));                                       \
    } while (false)

#define DO_UNOP(op, Op2)                                                       \
    do {                                                                       \
        Rboolean naflag = FALSE;                                               \
        if (stackObjIsSimpleScalar(val, REALSXP)) {                            \
            double x = tryStackObjToReal(val);                                 \
            double res = (x == NA_REAL) ? NA_REAL : op x;                      \
            STORE_UNOP_FAST(res, Real, REAL);                                  \
        } else if (stackObjIsSimpleScalar(val, INTSXP)) {                      \
            int res = Op2(tryStackObjToInteger(val), &naflag);                 \
            CHECK_INTEGER_OVERFLOW(naflag);                                    \
            STORE_UNOP_FAST(res, Int, INTEGER);                                \
        } else {                                                               \
            UNOP_FALLBACK(#op);                                                \
            break;                                                             \
        }                                                                      \
    } while (false)

#define DO_RELOP(op)                                                           \
    do {                                                                       \
        if (stackObjIsSimpleScalar(lhs, LGLSXP)) {                             \
            if (stackObjIsSimpleScalar(rhs, LGLSXP)) {                         \
                int l = tryStackObjToLogical(lhs);                             \
                int r = tryStackObjToLogical(rhs);                             \
                if (l == NA_LOGICAL || r == NA_LOGICAL) {                      \
                    STORE_BINOP_FAST(NA_LOGICAL, true, true, Logical,          \
                                     LOGICAL);                                 \
                } else {                                                       \
                    STORE_BINOP_FAST(l op r, true, true, Logical, LOGICAL);    \
                }                                                              \
                break;                                                         \
            }                                                                  \
        } else if (stackObjIsSimpleScalar(lhs, REALSXP)) {                     \
            if (stackObjIsSimpleScalar(rhs, REALSXP)) {                        \
                double l = tryStackObjToReal(lhs);                             \
                double r = tryStackObjToReal(rhs);                             \
                if (l == NA_REAL || r == NA_REAL) {                            \
                    STORE_BINOP(logicalStackObj(NA_LOGICAL));                  \
                } else {                                                       \
                    STORE_BINOP(logicalStackObj(l op r));                      \
                }                                                              \
                break;                                                         \
            } else if (stackObjIsSimpleScalar(rhs, INTSXP)) {                  \
                double l = tryStackObjToReal(lhs);                             \
                int r = tryStackObjToInteger(rhs);                             \
                if (l == NA_REAL || r == NA_INTEGER) {                         \
                    STORE_BINOP(logicalStackObj(NA_LOGICAL));                  \
                } else {                                                       \
                    STORE_BINOP(logicalStackObj(l op r));                      \
                }                                                              \
                break;                                                         \
            }                                                                  \
        } else if (stackObjIsSimpleScalar(lhs, INTSXP)) {                      \
            if (stackObjIsSimpleScalar(rhs, INTSXP)) {                         \
                int l = tryStackObjToInteger(lhs);                             \
                int r = tryStackObjToInteger(rhs);                             \
                if (l == NA_INTEGER || r == NA_INTEGER) {                      \
                    STORE_BINOP(logicalStackObj(NA_LOGICAL));                  \
                } else {                                                       \
                    STORE_BINOP(logicalStackObj(l op r));                      \
                }                                                              \
                break;                                                         \
            } else if (stackObjIsSimpleScalar(rhs, REALSXP)) {                 \
                int l = tryStackObjToInteger(lhs);                             \
                double r = tryStackObjToReal(rhs);                             \
                if (l == NA_INTEGER || r == NA_REAL) {                         \
                    STORE_BINOP(logicalStackObj(NA_LOGICAL));                  \
                } else {                                                       \
                    STORE_BINOP(logicalStackObj(l op r));                      \
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

extern SEXP Rf_deparse1(SEXP call, Rboolean abbrev, int opts);

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

// Assumes val is popped off stack, since it could be converted into an SEXP
static void setVar(SEXP sym, R_bcstack_t val, SEXP env, bool super) {
    PROTECT(sym);
    SEXP valSexp = stackObjToSexp(val); // Value should be popped off stack
    UNPROTECT(1);
    INCREMENT_NAMED(valSexp);
    PROTECT(valSexp);
    if (super) {
        Rf_setVar(sym, valSexp, env);
    } else {
        Rf_defineVar(sym, valSexp, env);
    }
    UNPROTECT(1);
}

// Assumes val is popped off stack, since it could be converted into an SEXP
static void cachedSetVar(R_bcstack_t val, SEXP env, Immediate idx,
                         InterpreterInstance* ctx, BindingCache* bindingCache,
                         bool keepMissing = false) {
    SEXP loc = cachedGetBindingCell(env, idx, ctx, bindingCache);
    if (loc && !BINDING_IS_LOCKED(loc) && !IS_ACTIVE_BINDING(loc)) {
        SEXP cur = CAR(loc);
        if (val.tag == STACK_OBJ_SEXP && val.u.sxpval == cur) {
            return;
        } else if (val.tag != STACK_OBJ_SEXP && trySetInPlace(cur, val)) {
            return;
        }
        PROTECT(loc);
        SEXP valSexp = stackObjToSexp(val); // Value should be popped off stack
        UNPROTECT(1);
        INCREMENT_NAMED(valSexp);
        SETCAR(loc, valSexp);
        if (!keepMissing && MISSING(loc))
            SET_MISSING(loc, 0);
        return;
    }

    SEXP sym = cp_pool_at(ctx, idx);
    SLOWASSERT(TYPEOF(sym) == SYMSXP);
    setVar(sym, val, env, false);
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-align"

// This happens since enabling -fno-exceptions, but the error message is
// terrible, can't find out where in the evalRirCode function
#pragma GCC diagnostic ignored "-Wstrict-overflow"

RCNTXT* nextFunctionContext(RCNTXT* cptr = R_GlobalContext) {
    while (cptr->nextcontext != NULL) {
        if (cptr->callflag & CTXT_FUNCTION) {
            return cptr;
        }
        cptr = cptr->nextcontext;
    }
    assert(false);
}

RCNTXT* firstFunctionContextWithDelayedEnv() {
    auto cptr = R_GlobalContext;
    RCNTXT* candidate = nullptr;
    while (cptr->nextcontext != NULL) {
        if (cptr->callflag & CTXT_FUNCTION) {
            if (cptr->cloenv == symbol::delayedEnv)
                candidate = cptr;
            else
                break;
        }
        cptr = cptr->nextcontext;
    }
    return candidate;
}

RCNTXT* findFunctionContextFor(SEXP e) {
    auto cptr = R_GlobalContext;
    while (cptr->nextcontext != NULL) {
        if (cptr->callflag & CTXT_FUNCTION && cptr->cloenv == e) {
            return cptr;
        }
        cptr = cptr->nextcontext;
    }
    return nullptr;
}

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
    SEXP deoptEnv = ostackSexpAt(ctx, stackHeight);
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
    SLOWASSERT(TYPEOF(deoptEnv) == ENVSXP);

    auto frameBaseSize = ostackLength(ctx) - excessStack;
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
            cntxt->nodestack = ostackCellAt(ctx, excessStack - 1);
            if ((SETJMP(cntxt->cjmpbuf))) {
                assert((size_t)ostackLength(ctx) == frameBaseSize);
                if (R_ReturnedValue == R_RestartToken) {
                    cntxt->callflag = CTXT_RETURN; /* turn restart off */
                    R_ReturnedValue = R_NilValue;  /* remove restart token */
                    return evalRirCode(code, ctx, cntxt->cloenv, callCtxt);
                } else {
                    return sexpToStackObj(R_ReturnedValue);
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
        size_t extraDeoptArgNum = innermostFrame ? 1 : 2;
        assert((size_t)ostackLength(ctx) ==
               frameBaseSize + f.stackSize + extraDeoptArgNum);
        R_bcstack_t res = nullStackObj;
        if (!innermostFrame)
            res = ostackPop(ctx);
        SEXP e = ostackPopSexp(ctx);
        assert(e == deoptEnv);
        if (!innermostFrame)
            ostackPush(ctx, res);
        code->registerInvocation();
        return evalRirCode(code, ctx, cntxt->cloenv, callCtxt, f.pc);
    };

    R_bcstack_t res = trampoline();
    SLOWASSERT((size_t)ostackLength(ctx) == frameBaseSize);

    // Dont end the outermost context (unless it was a fake one) to be able to
    // jump out below
    if (!outermostFrame || !originalCntxt) {
        res = sexpToStackObj(stackObjToSexp(res));
        endClosureContext(cntxt, res.u.sxpval);
    }

    if (outermostFrame) {
        // long-jump out of all the inlined contexts
        Rf_findcontext(CTXT_BROWSER | CTXT_FUNCTION,
                       nextFunctionContext()->cloenv, stackObjToSexp(res));
        assert(false);
    }

    ostackPush(ctx, res);
}

R_bcstack_t evalRirCode(Code* c, InterpreterInstance* ctx, SEXP env,
                        const CallContext* callCtxt, Opcode* initialPC,
                        R_bcstack_t* localsBase) {
    SLOWASSERT(env != symbol::delayedEnv || (callCtxt != nullptr));

#ifdef THREADED_CODE
    static void* opAddr[static_cast<uint8_t>(Opcode::num_of)] = {
#define DEF_INSTR(name, ...) (__extension__ && op_##name),
#include "ir/insns.h"
#undef DEF_INSTR
    };
#endif

    SLOWASSERT(c->info.magic == CODE_MAGIC);

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
    ostackEnsureSize(ctx, c->stackLength + 5);

    Opcode* pc = initialPC ? initialPC : c->code();

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
            SEXP ast = ostackSexpAt(ctx, 1);
            SEXP op = ostackSexpAt(ctx, 0);
            SLOWASSERT(TYPEOF(env) == ENVSXP);
            SLOWASSERT(TYPEOF(op) == CLOSXP);
            ostackPopn(ctx, 2);
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

        INSTRUCTION(pop_context_) {
            return ostackPop(ctx);
        }

        INSTRUCTION(mk_env_) {
            size_t n = readImmediate();
            advanceImmediate();
            R_bcstack_t parent = ostackPop(ctx);
            SLOWASSERT(stackObjSexpType(parent) == ENVSXP &&
                       "Non-environment used as environment parent.");
            SEXP arglist = R_NilValue;
            auto names = (Immediate*)pc;
            advanceImmediateN(n);
            PROTECT(parent.u.sxpval);
            for (long i = n - 1; i >= 0; --i) {
                PROTECT(arglist);
                SEXP val = ostackPopSexp(ctx);
                UNPROTECT(1);
                SEXP name = cp_pool_at(ctx, names[i]);
                arglist = CONS_NR(val, arglist);
                SET_TAG(arglist, name);
                SET_MISSING(arglist, val == R_MissingArg ? 2 : 0);
            }
            UNPROTECT(1);
            SEXP res = Rf_NewEnvironment(R_NilValue, arglist, parent.u.sxpval);

            if (auto cptr = firstFunctionContextWithDelayedEnv()) {
                cptr->cloenv = res;
                if (cptr->promargs == symbol::delayedArglist)
                    cptr->promargs = arglist;
            }

            ostackPushSexp(ctx, res);
            NEXT();
        }

        INSTRUCTION(mk_stub_env_) {
            // TODO: There is a potential safety problem because we are not
            // preserving the args and parent SEXP. Doing it here is not an
            // option becase R_Preserve is slow. We must find a simple story so
            // that the gc trace rir wrappers.
            size_t n = readImmediate();
            advanceImmediate();
            // Do we need to preserve parent and the arg vals?
            SEXP parent = ostackPopSexp(ctx);
            assert(TYPEOF(parent) == ENVSXP &&
                   "Non-environment used as environment parent.");
            auto names = pc;
            advanceImmediateN(n);
            std::vector<SEXP>* args = new std::vector<SEXP>();
            for (size_t i = 0; i < n; ++i)
                args->push_back(ostackPopSexp(ctx));
            auto envStub =
                new LazyEnvironment(args, parent, names, ctx, localsBase);
            envStubs.push_back(envStub);
            SEXP res = (SEXP)envStub;

            if (auto cptr = firstFunctionContextWithDelayedEnv())
                cptr->cloenv = res;

            ostackPushSexp(ctx, res);
            NEXT();
        }

        INSTRUCTION(parent_env_) {
            // Can only be used for pir. In pir we always have a closure that
            // stores the lexical envrionment
            SLOWASSERT(callCtxt);
            ostackPushSexp(ctx, CLOENV(callCtxt->callee));
            NEXT();
        }

        INSTRUCTION(get_env_) {
            SLOWASSERT(env);
            ostackPushSexp(ctx, env);
            NEXT();
        }

        INSTRUCTION(set_env_) {
            SEXP e = ostackPopSexp(ctx);
            changeEnv(e);
            NEXT();
        }

        INSTRUCTION(ldfun_) {
            SEXP sym = readConst(ctx, readImmediate());
            advanceImmediate();
            SEXP res = Rf_findFun(sym, env);
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
            ostackPushSexp(ctx, res);
            NEXT();
        }

        INSTRUCTION(ldvar_) {
            Immediate id = readImmediate();
            advanceImmediate();
            SEXP res = cachedGetVar(env, id, ctx, bindingCache);
            R_Visible = TRUE;

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

            ostackPushSexp(ctx, res);
            NEXT();
        }

        INSTRUCTION(ldvar_noforce_) {
            Immediate id = readImmediate();
            advanceImmediate();
            SEXP res = cachedGetVar(env, id, ctx, bindingCache);
            R_Visible = TRUE;

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

            ostackPushSexp(ctx, res);
            NEXT();
        }

        INSTRUCTION(ldvar_super_) {
            SEXP sym = readConst(ctx, readImmediate());
            advanceImmediate();
            SEXP res = Rf_findVar(sym, ENCLOS(env));
            R_Visible = TRUE;

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

            ostackPushSexp(ctx, res);
            NEXT();
        }

        INSTRUCTION(ldvar_noforce_super_) {
            SEXP sym = readConst(ctx, readImmediate());
            advanceImmediate();
            SEXP res = Rf_findVar(sym, ENCLOS(env));
            R_Visible = TRUE;

            if (res == R_UnboundValue) {
                Rf_error("object \"%s\" not found", CHAR(PRINTNAME(sym)));
            } else if (res == R_MissingArg) {
                Rf_error("argument \"%s\" is missing, with no default",
                         CHAR(PRINTNAME(sym)));
            }

            if (res != R_NilValue)
                ENSURE_NAMED(res);

            ostackPushSexp(ctx, res);
            NEXT();
        }

        INSTRUCTION(ldddvar_) {
            SEXP sym = readConst(ctx, readImmediate());
            advanceImmediate();
            SEXP res = Rf_ddfindVar(sym, env);
            R_Visible = TRUE;

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

            ostackPushSexp(ctx, res);
            NEXT();
        }

        INSTRUCTION(ldlval_) {
            Immediate id = readImmediate();
            advanceImmediate();
            SEXP res = cachedGetBindingCell(env, id, ctx, bindingCache);
            SLOWASSERT(res);
            res = CAR(res);
            SLOWASSERT(res != R_UnboundValue);

            R_Visible = TRUE;

            if (TYPEOF(res) == PROMSXP)
                res = PRVALUE(res);

            SLOWASSERT(res != R_UnboundValue);
            SLOWASSERT(res != R_MissingArg);

            if (res != R_NilValue)
                ENSURE_NAMED(res);

            ostackPushSexp(ctx, res);
            NEXT();
        }

        INSTRUCTION(ldarg_) {
            Immediate idx = readImmediate();
            advanceImmediate();
            SLOWASSERT(callCtxt);

            if (callCtxt->hasStackArgs()) {
                ostackPush(ctx, callCtxt->stackArg(idx, ctx));
            } else {
                SEXP res;
                if (callCtxt->missingArg(idx)) {
                    res = R_MissingArg;
                } else {
                    Code* arg = callCtxt->implicitArg(idx);
                    res = createPromise(arg, callCtxt->callerEnv);
                }
                ostackPushSexp(ctx, res);
            }
            NEXT();
        }

        INSTRUCTION(ldloc_) {
            Immediate offset = readImmediate();
            advanceImmediate();
            R_bcstack_t res = locals.load(offset);
            ostackPush(ctx, res);
            NEXT();
        }

        INSTRUCTION(stvar_) {
            Immediate id = readImmediate();
            advanceImmediate();
            R_bcstack_t val = ostackPop(ctx);

            cachedSetVar(val, env, id, ctx, bindingCache);

            NEXT();
        }

        INSTRUCTION(starg_) {
            Immediate id = readImmediate();
            advanceImmediate();
            R_bcstack_t val = ostackPop(ctx);

            cachedSetVar(val, env, id, ctx, bindingCache, true);

            NEXT();
        }

        INSTRUCTION(stvar_super_) {
            SEXP sym = readConst(ctx, readImmediate());
            advanceImmediate();
            SLOWASSERT(TYPEOF(sym) == SYMSXP);
            R_bcstack_t val = ostackPop(ctx);
            setVar(sym, val, ENCLOS(env), true);
            NEXT();
        }

        INSTRUCTION(stloc_) {
            Immediate offset = readImmediate();
            advanceImmediate();
            locals.store(offset, ostackTop(ctx));
            ostackPop(ctx);
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
            auto lll = ostackLength(ctx);
            int ttt = R_PPStackTop;
#endif

            // Callee is TOS
            // Arguments and names are immediate given as promise code indices.
            size_t n = readImmediate();
            advanceImmediate();
            size_t ast = readImmediate();
            advanceImmediate();
            Assumptions given(readImmediate());
            advanceImmediate();
            auto arguments = (Immediate*)pc;
            advanceImmediateN(n);
            auto names = (Immediate*)pc;
            advanceImmediateN(n);
            CallContext call(c, ostackSexpAt(ctx, 0), n, ast, arguments, names,
                             env, given, ctx);
            R_bcstack_t res = doCall(call, ctx);
            ostackPop(ctx); // callee
            ostackPush(ctx, res);

            SLOWASSERT(ttt == R_PPStackTop);
            SLOWASSERT(lll == ostackLength(ctx));
            NEXT();
        }

        INSTRUCTION(record_call_) {
            ObservedCallees* feedback = (ObservedCallees*)pc;
            SEXP callee = ostackSexpAt(ctx, 0);
            feedback->record(c, callee);
            pc += sizeof(ObservedCallees);
            NEXT();
        }

        INSTRUCTION(record_binop_) {
            ObservedValues* feedback = (ObservedValues*)pc;
            R_bcstack_t l = ostackAt(ctx, 1);
            R_bcstack_t r = ostackAt(ctx, 0);
            feedback[0].record(l);
            feedback[1].record(r);
            pc += 2 * sizeof(ObservedValues);
            NEXT();
        }

        INSTRUCTION(call_implicit_) {
#ifdef ENABLE_SLOWASSERT
            auto lll = ostackLength(ctx);
            int ttt = R_PPStackTop;
#endif

            // Callee is TOS
            // Arguments are immediate given as promise code indices.
            size_t n = readImmediate();
            advanceImmediate();
            size_t ast = readImmediate();
            advanceImmediate();
            Assumptions given(readImmediate());
            advanceImmediate();
            auto arguments = (Immediate*)pc;
            advanceImmediateN(n);
            CallContext call(c, ostackSexpAt(ctx, 0), n, ast, arguments, env,
                             given, ctx);
            R_bcstack_t res = doCall(call, ctx);
            ostackPop(ctx); // callee
            ostackPush(ctx, res);

            SLOWASSERT(ttt == R_PPStackTop);
            SLOWASSERT(lll == ostackLength(ctx));
            NEXT();
        }

        INSTRUCTION(call_) {
#ifdef ENABLE_SLOWASSERT
            auto lll = ostackLength(ctx);
            int ttt = R_PPStackTop;
#endif

            // Stack contains [callee, arg1, ..., argn]
            Immediate n = readImmediate();
            advanceImmediate();
            size_t ast = readImmediate();
            advanceImmediate();
            Assumptions given(readImmediate());
            advanceImmediate();
            CallContext call(c, ostackSexpAt(ctx, n), n, ast,
                             ostackCellAt(ctx, n - 1), env, given, ctx);
            R_bcstack_t res = doCall(call, ctx);
            ostackPopn(ctx, call.passedArgs + 1);
            ostackPush(ctx, res);
            SLOWASSERT(ttt == R_PPStackTop);
            SLOWASSERT(lll - call.suppliedArgs == (unsigned)ostackLength(ctx));
            NEXT();
        }

        INSTRUCTION(named_call_) {
#ifdef ENABLE_SLOWASSERT
            auto lll = ostackLength(ctx);
            int ttt = R_PPStackTop;
#endif

            // Stack contains [callee, arg1, ..., argn]
            Immediate n = readImmediate();
            advanceImmediate();
            size_t ast = readImmediate();
            advanceImmediate();
            Assumptions given(readImmediate());
            advanceImmediate();
            auto names = (Immediate*)pc;
            advanceImmediateN(n);
            CallContext call(c, ostackSexpAt(ctx, n), n, ast,
                             ostackCellAt(ctx, n - 1), names, env, given, ctx);
            R_bcstack_t res = doCall(call, ctx);
            ostackPopn(ctx, call.passedArgs + 1);
            ostackPush(ctx, res);

            SLOWASSERT(ttt == R_PPStackTop);
            SLOWASSERT(lll - call.suppliedArgs == (unsigned)ostackLength(ctx));
            NEXT();
        }

        INSTRUCTION(call_builtin_) {
#ifdef ENABLE_SLOWASSERT
            auto lll = ostackLength(ctx);
            int ttt = R_PPStackTop;
#endif

            // Stack contains [arg1, ..., argn], callee is immediate
            Immediate n = readImmediate();
            advanceImmediate();
            Immediate ast = readImmediate();
            advanceImmediate();
            SEXP callee = cp_pool_at(ctx, readImmediate());
            advanceImmediate();
            CallContext call(c, callee, n, ast, ostackCellAt(ctx, n - 1), env,
                             Assumptions(), ctx);
            R_bcstack_t res = builtinCall(call, ctx);
            ostackPopn(ctx, call.passedArgs);
            ostackPush(ctx, res);

            SLOWASSERT(ttt == R_PPStackTop);
            SLOWASSERT(lll - call.suppliedArgs + 1 ==
                       (unsigned)ostackLength(ctx));
            NEXT();
        }

        INSTRUCTION(static_call_) {
#ifdef ENABLE_SLOWASSERT
            auto lll = ostackLength(ctx);
            int ttt = R_PPStackTop;
#endif

            // Stack contains [arg1, ..., argn], callee is immediate
            Immediate n = readImmediate();
            advanceImmediate();
            Immediate ast = readImmediate();
            advanceImmediate();
            Assumptions given(readImmediate());
            advanceImmediate();
            SEXP callee = cp_pool_at(ctx, readImmediate());
            advanceImmediate();
            SEXP version = cp_pool_at(ctx, readImmediate());
            CallContext call(c, callee, n, ast, ostackCellAt(ctx, n - 1), env,

                             given, ctx);
            auto fun = Function::unpack(version);
            SEXP res;
            addDynamicAssumptionsFromContext(call, ctx);
            bool dispatchFail = !matches(call, fun->signature());
            if (fun->invocationCount() % PIR_WARMUP == 0)
                if (addDynamicAssumptionsForOneTarget(call, fun->signature()) !=
                    fun->signature().assumptions)
                    // We have more assumptions available, let's recompile
                    dispatchFail = true;

            if (dispatchFail) {
                auto dt = DispatchTable::unpack(BODY(callee));
                fun = dispatch(call, dt);
                // Patch inline cache
                (*(Immediate*)pc) = Pool::insert(fun->container());
                SLOWASSERT(fun != dt->baseline());
            }
            advanceImmediate();

            ArgsLazyData lazyArgs(&call, ctx);
            fun->registerInvocation();
            supplyMissingArgs(call, fun);
            res = rirCallTrampoline(call, fun, symbol::delayedEnv,
                                    (SEXP)&lazyArgs, ctx);
            ostackPopn(ctx, call.passedArgs);
            ostackPushSexp(ctx, res);

            SLOWASSERT(ttt == R_PPStackTop);
            SLOWASSERT(lll - call.suppliedArgs + 1 ==
                       (unsigned)ostackLength(ctx));
            NEXT();
        }

        INSTRUCTION(close_) {
            SEXP srcref = ostackSexpAt(ctx, 0);
            PROTECT(srcref);
            SEXP body = ostackSexpAt(ctx, 1);
            PROTECT(body);
            SEXP formals = ostackSexpAt(ctx, 2);
            PROTECT(formals);
            SEXP res = Rf_allocSExp(CLOSXP);
            UNPROTECT(3);
            SLOWASSERT(DispatchTable::check(body));
            SET_FORMALS(res, formals);
            SET_BODY(res, body);
            SET_CLOENV(res, env);
            Rf_setAttrib(res, Rf_install("srcref"), srcref);
            ostackPopn(ctx, 3);
            ostackPushSexp(ctx, res);
            NEXT();
        }

        INSTRUCTION(isfun_) {
            R_bcstack_t val = ostackTop(ctx);

            switch (stackObjSexpType(val)) {
            case CLOSXP:
                jit(val.u.sxpval, R_NilValue, ctx);
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
            PROTECT(prom);
            SET_PRVALUE(prom, ostackPopSexp(ctx));
            UNPROTECT(1);
            ostackPushSexp(ctx, prom);
            NEXT();
        }

        INSTRUCTION(force_) {
            if (stackObjSexpType(ostackTop(ctx)) == PROMSXP) {
                SEXP val = ostackPop(ctx).u.sxpval;
                // If the promise is already evaluated then push the value
                // inside the promise onto the stack, otherwise push the value
                // from forcing the promise
                ostackPushSexp(ctx, promiseValue(val, ctx));
            }
            NEXT();
        }

        INSTRUCTION(push_) {
            SEXP res = readConst(ctx, readImmediate());
            advanceImmediate();
            ostackPushSexp(ctx, res);
            NEXT();
        }

        INSTRUCTION(push_code_) {
            Immediate n = readImmediate();
            advanceImmediate();
            ostackPushSexp(ctx, c->getPromise(n)->container());
            NEXT();
        }

        INSTRUCTION(dup_) {
            ostackPush(ctx, ostackTop(ctx));
            NEXT();
        }

        INSTRUCTION(dup2_) {
            ostackPush(ctx, ostackAt(ctx, 1));
            ostackPush(ctx, ostackAt(ctx, 1));
            NEXT();
        }

        INSTRUCTION(pop_) {
            ostackPop(ctx);
            NEXT();
        }

        INSTRUCTION(swap_) {
            R_bcstack_t lhs = ostackPop(ctx);
            R_bcstack_t rhs = ostackPop(ctx);
            ostackPush(ctx, lhs);
            ostackPush(ctx, rhs);
            NEXT();
        }

        INSTRUCTION(put_) {
            Immediate i = readImmediate();
            advanceImmediate();
            R_bcstack_t* pos = ostackCellAt(ctx, 0);
            R_bcstack_t val = *pos;
            while (i--) {
                *pos = *(pos - 1);
                pos--;
            }
            *pos = val;
            NEXT();
        }

        INSTRUCTION(pick_) {
            Immediate i = readImmediate();
            advanceImmediate();
            R_bcstack_t* pos = ostackCellAt(ctx, i);
            R_bcstack_t val = *pos;
            while (i--) {
                *pos = *(pos + 1);
                pos++;
            }
            *pos = val;
            NEXT();
        }

        INSTRUCTION(pull_) {
            Immediate i = readImmediate();
            advanceImmediate();
            R_bcstack_t val = ostackAt(ctx, i);
            ostackPush(ctx, val);
            NEXT();
        }

        INSTRUCTION(add_) {
            R_bcstack_t lhs = ostackAt(ctx, 1);
            R_bcstack_t rhs = ostackAt(ctx, 0);
            DO_BINOP(+, RInteger_plus);
            NEXT();
        }

        INSTRUCTION(uplus_) {
            R_bcstack_t val = ostackAt(ctx, 0);
            DO_UNOP(+, RInteger_uplus);
            NEXT();
        }

        INSTRUCTION(inc_) {
            R_bcstack_t val = ostackPop(ctx);
            SLOWASSERT(stackObjIsSimpleScalar(val, INTSXP));
            int i;
            switch (val.tag) {
            case STACK_OBJ_INT:
                i = val.u.ival + 1;
                ostackPushInt(ctx, i);
                break;
            case STACK_OBJ_SEXP:
                if (MAYBE_SHARED(val.u.sxpval)) {
                    i = *INTEGER(val.u.sxpval) + 1;
                    ostackPushInt(ctx, i);
                } else {
                    (*INTEGER(val.u.sxpval))++;
                }
                break;
            default:
                assert(false);
            }
            NEXT();
        }

        INSTRUCTION(sub_) {
            R_bcstack_t lhs = ostackAt(ctx, 1);
            R_bcstack_t rhs = ostackAt(ctx, 0);
            DO_BINOP(-, RInteger_minus);
            NEXT();
        }

        INSTRUCTION(uminus_) {
            R_bcstack_t val = ostackAt(ctx, 0);
            DO_UNOP(-, RInteger_uminus);
            NEXT();
        }

        INSTRUCTION(mul_) {
            R_bcstack_t lhs = ostackAt(ctx, 1);
            R_bcstack_t rhs = ostackAt(ctx, 0);
            DO_BINOP(*, RInteger_times);
            NEXT();
        }

        INSTRUCTION(div_) {
            R_bcstack_t lhs = ostackAt(ctx, 1);
            R_bcstack_t rhs = ostackAt(ctx, 0);

            if (stackObjIsSimpleScalar(lhs, REALSXP) &&
                stackObjIsSimpleScalar(rhs, REALSXP)) {
                double l = tryStackObjToReal(lhs);
                double r = tryStackObjToReal(rhs);
                double real_res =
                    (l == NA_REAL || r == NA_REAL) ? NA_REAL : l / r;
                STORE_BINOP_FAST(real_res, true, true, Real, REAL);
            } else if (stackObjIsSimpleScalar(lhs, INTSXP) &&
                       stackObjIsSimpleScalar(rhs, INTSXP)) {
                int l = tryStackObjToInteger(lhs);
                int r = tryStackObjToInteger(rhs);
                double real_res;
                if (l == NA_INTEGER || r == NA_INTEGER)
                    real_res = NA_REAL;
                else
                    real_res = (double)l / (double)r;
                STORE_BINOP(realStackObj(real_res));
            } else {
                BINOP_FALLBACK("/");
            }
            NEXT();
        }

        INSTRUCTION(idiv_) {
            R_bcstack_t lhs = ostackAt(ctx, 1);
            R_bcstack_t rhs = ostackAt(ctx, 0);

            if (stackObjIsSimpleScalar(lhs, REALSXP) &&
                stackObjIsSimpleScalar(rhs, REALSXP)) {
                double l = tryStackObjToReal(lhs);
                double r = tryStackObjToReal(rhs);
                double real_res =
                    (l == NA_REAL || r == NA_REAL) ? NA_REAL : myfloor(l, r);
                STORE_BINOP_FAST(real_res, true, true, Real, REAL);
            } else if (stackObjIsSimpleScalar(lhs, INTSXP) &&
                       stackObjIsSimpleScalar(rhs, INTSXP)) {
                int l = tryStackObjToInteger(lhs);
                int r = tryStackObjToInteger(rhs);
                int int_res;
                if (l == NA_INTEGER || r == NA_INTEGER)
                    int_res = NA_REAL;
                else
                    int_res = (int)floor((double)l / (double)r);
                STORE_BINOP_FAST(int_res, true, true, Int, INTEGER);
            } else {
                BINOP_FALLBACK("%/%");
            }
            NEXT();
        }

        INSTRUCTION(mod_) {
            R_bcstack_t lhs = ostackAt(ctx, 1);
            R_bcstack_t rhs = ostackAt(ctx, 0);

            if (stackObjIsSimpleScalar(lhs, REALSXP) &&
                stackObjIsSimpleScalar(rhs, REALSXP)) {
                double real_res =
                    myfmod(tryStackObjToReal(lhs), tryStackObjToReal(rhs));
                STORE_BINOP_FAST(real_res, true, true, Real, REAL);
            } else if (stackObjIsSimpleScalar(lhs, INTSXP) &&
                       stackObjIsSimpleScalar(rhs, INTSXP)) {
                int l = tryStackObjToInteger(lhs);
                int r = tryStackObjToInteger(rhs);
                int int_res;
                if (l == NA_INTEGER || r == NA_INTEGER || r == 0) {
                    int_res = NA_INTEGER;
                } else {
                    int_res = (l >= 0 && r > 0)
                                  ? l % r
                                  : (int)myfmod((double)l, (double)r);
                }
                STORE_BINOP_FAST(int_res, true, true, Int, INTEGER);
            } else {
                BINOP_FALLBACK("%%");
            }
            NEXT();
        }

        INSTRUCTION(pow_) {
            R_bcstack_t lhs = ostackAt(ctx, 1);
            R_bcstack_t rhs = ostackAt(ctx, 0);
            BINOP_FALLBACK("^");
            NEXT();
        }

        INSTRUCTION(lt_) {
            R_bcstack_t lhs = ostackAt(ctx, 1);
            R_bcstack_t rhs = ostackAt(ctx, 0);
            DO_RELOP(<);
            NEXT();
        }

        INSTRUCTION(gt_) {
            R_bcstack_t lhs = ostackAt(ctx, 1);
            R_bcstack_t rhs = ostackAt(ctx, 0);
            DO_RELOP(>);
            NEXT();
        }

        INSTRUCTION(le_) {
            R_bcstack_t lhs = ostackAt(ctx, 1);
            R_bcstack_t rhs = ostackAt(ctx, 0);
            DO_RELOP(<=);
            NEXT();
        }

        INSTRUCTION(ge_) {
            R_bcstack_t lhs = ostackAt(ctx, 1);
            R_bcstack_t rhs = ostackAt(ctx, 0);
            DO_RELOP(>=);
            NEXT();
        }

        INSTRUCTION(eq_) {
            R_bcstack_t lhs = ostackAt(ctx, 1);
            R_bcstack_t rhs = ostackAt(ctx, 0);
            DO_RELOP(==);
            NEXT();
        }

        INSTRUCTION(identical_noforce_) {
            R_bcstack_t rhs = ostackPop(ctx);
            R_bcstack_t lhs = ostackPop(ctx);
            // This instruction does not force, but we should still compare
            // the actual promise value if it is already forced.
            // Especially important since all the inlined functions are probably
            // behind lazy loading stub promises.
            if (stackObjSexpType(rhs) == PROMSXP &&
                PRVALUE(rhs.u.sxpval) != R_UnboundValue)
                rhs = sexpToStackObj(PRVALUE(rhs.u.sxpval));
            if (stackObjSexpType(lhs) == PROMSXP &&
                PRVALUE(lhs.u.sxpval) != R_UnboundValue)
                lhs = sexpToStackObj(PRVALUE(lhs.u.sxpval));
            bool res = stackObjsIdentical(lhs, rhs);
            ostackPushLogical(ctx, res);
            NEXT();
        }

        INSTRUCTION(ne_) {
            SLOWASSERT(R_PPStackTop >= 0);
            R_bcstack_t lhs = ostackAt(ctx, 1);
            R_bcstack_t rhs = ostackAt(ctx, 0);
            DO_RELOP(!=);
            NEXT();
        }

        INSTRUCTION(not_) {
            R_bcstack_t val = ostackAt(ctx, 0);

            if (stackObjIsSimpleScalar(val, INTSXP)) {
                int x = tryStackObjToInteger(val);
                int logical_res;
                if (x == NA_INTEGER) {
                    logical_res = NA_LOGICAL;
                } else {
                    logical_res = (x == 0);
                }
                STORE_UNOP(logicalStackObj(logical_res));
            } else if (stackObjIsSimpleScalar(val, REALSXP)) {
                double x = tryStackObjToReal(val);
                int logical_res;
                if (x == NA_REAL) {
                    logical_res = NA_LOGICAL;
                } else {
                    logical_res = (x == 0);
                }
                STORE_UNOP(logicalStackObj(logical_res));
            } else if (stackObjIsSimpleScalar(val, LGLSXP)) {
                int x = tryStackObjToLogical(val);
                int logical_res;
                if (x == NA_LOGICAL) {
                    logical_res = NA_LOGICAL;
                } else {
                    logical_res = (x == 0);
                }
                STORE_UNOP_FAST(logical_res, Logical, LOGICAL);
            } else {
                UNOP_FALLBACK("!");
            }

            NEXT();
        }

        INSTRUCTION(lgl_or_) {
            R_bcstack_t rhs = ostackPop(ctx);
            R_bcstack_t lhs = ostackPop(ctx);
#ifdef USE_TYPED_STACK
            SLOWASSERT(stackObjSexpType(lhs) == LGLSXP &&
                       stackObjSexpType(rhs) == LGLSXP);
            int x1 = tryStackObjToLogicalNa(lhs);
            int x2 = tryStackObjToLogicalNa(rhs);
#else
            int x1 = *LOGICAL(lhs.u.sxpval);
            int x2 = *LOGICAL(rhs.u.sxpval);
#endif
            SLOWASSERT(x1 == 1 || x1 == 0 || x1 == NA_LOGICAL);
            SLOWASSERT(x2 == 1 || x2 == 0 || x2 == NA_LOGICAL);
            int logical_res;
            if (x1 == 1 || x2 == 1) {
                logical_res = 1;
            } else if (x1 == 0 && x2 == 0) {
                logical_res = 0;
            } else {
                logical_res = NA_LOGICAL;
            }
            ostackPushLogical(ctx, logical_res);
            NEXT();
        }

        INSTRUCTION(lgl_and_) {
            R_bcstack_t rhs = ostackPop(ctx);
            R_bcstack_t lhs = ostackPop(ctx);
#ifdef USE_TYPED_STACK
            SLOWASSERT(stackObjSexpType(lhs) == LGLSXP &&
                       stackObjSexpType(rhs) == LGLSXP);
            int x1 = tryStackObjToLogicalNa(lhs);
            int x2 = tryStackObjToLogicalNa(rhs);
#else
            int x1 = *LOGICAL(lhs.u.sxpval);
            int x2 = *LOGICAL(rhs.u.sxpval);
#endif
            SLOWASSERT(x1 == 1 || x1 == 0 || x1 == NA_LOGICAL);
            SLOWASSERT(x2 == 1 || x2 == 0 || x2 == NA_LOGICAL);
            int logical_res;
            if (x1 == 1 && x2 == 1) {
                logical_res = 1;
            } else if (x1 == 0 || x2 == 0) {
                logical_res = 0;
            } else {
                logical_res = NA_LOGICAL;
            }
            ostackPushLogical(ctx, logical_res);
            NEXT();
        }

        INSTRUCTION(aslogical_) {
            R_bcstack_t val = ostackTop(ctx);
            int logical_res = stackObjAsLogical(val);
            STORE_UNOP(logicalStackObj(logical_res));
            NEXT();
        }

        INSTRUCTION(asbool_) {
            R_bcstack_t val = ostackTop(ctx);

            int logical_res;
            if (stackObjIsSimpleScalar(val, REALSXP)) {
                // TODO: Is this right?
                logical_res = (tryStackObjToReal(val) != 0.0);
            } else if (stackObjIsSimpleScalar(val, INTSXP)) {
                // TODO: Is this right?
                logical_res = (tryStackObjToInteger(val) != 0);
            } else if (stackObjIsSimpleScalar(val, LGLSXP)) {
                NEXT();
            } else {
                if (XLENGTH(val.u.sxpval) > 1)
                    Rf_warningcall(
                        getSrcAt(c, pc - 1, ctx),
                        "the condition has length > 1 and only the first "
                        "element will be used");

                if (XLENGTH(val.u.sxpval) == 0) {
                    Rf_errorcall(getSrcAt(c, pc - 1, ctx),
                                 "argument is of length zero");
                } else {
                    logical_res = Rf_asLogical(val.u.sxpval);
                    if (logical_res == NA_LOGICAL) {
                        if (!isLogical(val.u.sxpval)) {
                            Rf_errorcall(
                                getSrcAt(c, pc - 1, ctx),
                                "argument is not interpretable as logical");
                        } else {
                            Rf_errorcall(
                                getSrcAt(c, pc - 1, ctx),
                                "missing value where TRUE/FALSE needed");
                        }
                    }
                }
            }
            STORE_UNOP(logicalStackObj(logical_res));
            NEXT();
        }

        INSTRUCTION(asast_) {
            R_bcstack_t val = ostackPop(ctx);
            SLOWASSERT(stackObjSexpType(val) == PROMSXP);
            SEXP res = PRCODE(val.u.sxpval);
            // if the code is EXTERNALSXP then it is rir Code object, get its
            // ast
            if (TYPEOF(res) == EXTERNALSXP)
                res = cp_pool_at(ctx, Code::unpack(res)->src);
            // otherwise return whatever we had, make sure we do not see
            // bytecode
            SLOWASSERT(TYPEOF(res) != BCODESXP);
            ostackPushSexp(ctx, res);
            NEXT();
        }

        INSTRUCTION(is_) {
            R_bcstack_t val = ostackPop(ctx);
            SEXPTYPE type = stackObjSexpType(val);
            Immediate i = readImmediate();
            advanceImmediate();
            int logical_res;
            switch (i) {
            case NILSXP:
            case LGLSXP:
            case REALSXP:
                logical_res = type == i;
                break;

            case VECSXP:
                logical_res = type == VECSXP || type == LISTSXP;
                break;

            case LISTSXP:
                logical_res = type == LISTSXP || type == NILSXP;
                break;

            default:
                assert(false);
                break;
            }
            ostackPushLogical(ctx, logical_res);
            NEXT();
        }

        INSTRUCTION(isobj_) {
            R_bcstack_t val = ostackPop(ctx);
            int logical_res =
                val.tag == STACK_OBJ_SEXP && isObject(val.u.sxpval);
            ostackPushLogical(ctx, logical_res);
            NEXT();
        }

        INSTRUCTION(isstubenv_) {
            R_bcstack_t val = ostackPop(ctx);
            ostackPushLogical(ctx, val.tag == STACK_OBJ_SEXP &&
                                       LazyEnvironment::cast(val.u.sxpval));
            NEXT();
        }

        INSTRUCTION(missing_) {
            SEXP sym = readConst(ctx, readImmediate());
            advanceImmediate();
            SLOWASSERT(TYPEOF(sym) == SYMSXP);
            SLOWASSERT(!DDVAL(sym));
            SLOWASSERT(env);
            SEXP val = R_findVarLocInFrame(env, sym).cell;
            if (val == NULL)
                Rf_errorcall(getSrcAt(c, pc - 1, ctx),
                             "'missing' can only be used for arguments");

            if (MISSING(val) || CAR(val) == R_MissingArg) {
                ostackPushLogical(ctx, true);
                NEXT();
            }

            val = CAR(val);

            if (TYPEOF(val) != PROMSXP) {
                ostackPushLogical(ctx, false);
                NEXT();
            }

            val = findRootPromise(val);
            if (!isSymbol(PREXPR(val)))
                ostackPushLogical(ctx, false);
            else {
                ostackPush(
                    ctx, logicalStackObj(R_isMissing(PREXPR(val), PRENV(val))));
            }
            NEXT();
        }

        INSTRUCTION(check_missing_) {
            R_bcstack_t val = ostackTop(ctx);
            if (val.tag == STACK_OBJ_SEXP && val.u.sxpval == R_MissingArg)
                Rf_error("argument is missing, with no default");
            NEXT();
        }

        INSTRUCTION(brobj_) {
            R_bcstack_t val = ostackTop(ctx);
            JumpOffset offset = readJumpOffset();
            advanceJump();
            if (val.tag == STACK_OBJ_SEXP && isObject(val.u.sxpval))
                pc += offset;
            PC_BOUNDSCHECK(pc, c);
            NEXT();
        }

        INSTRUCTION(brtrue_) {
            R_bcstack_t val = ostackPop(ctx);
            JumpOffset offset = readJumpOffset();
            advanceJump();
            if (tryStackObjToLogical(val) == 1) {
                pc += offset;
            }
            PC_BOUNDSCHECK(pc, c);
            NEXT();
        }

        INSTRUCTION(brfalse_) {
            R_bcstack_t val = ostackPop(ctx);
            JumpOffset offset = readJumpOffset();
            advanceJump();
            if (tryStackObjToLogical(val) == 0) {
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
            SEXP val = ostackSexpAt(ctx, 1);
            PROTECT(val);
            SEXP idx = ostackSexpAt(ctx, 0);
            PROTECT(idx);
            SEXP args = CONS_NR(val, CONS_NR(idx, R_NilValue));
            UNPROTECT(2);
            ostackPushSexp(ctx, args);

            SEXP res;
            if (isObject(val)) {
                SEXP call = getSrcForCall(c, pc - 1, ctx);
                res = dispatchApply(call, val, args, symbol::Bracket, env, ctx);
                if (!res)
                    res =
                        do_subset_dflt(R_NilValue, symbol::Bracket, args, env);
            } else {
                res = do_subset_dflt(R_NilValue, symbol::Bracket, args, env);
            }

            ostackPopn(ctx, 3);

            R_Visible = TRUE;
            ostackPushSexp(ctx, res);
            NEXT();
        }

        INSTRUCTION(extract1_2_) {
            SEXP val = ostackSexpAt(ctx, 2);
            PROTECT(val);
            SEXP idx = ostackSexpAt(ctx, 1);
            PROTECT(idx);
            SEXP idx2 = ostackSexpAt(ctx, 0);
            PROTECT(idx2);
            SEXP args = CONS_NR(val, CONS_NR(idx, CONS_NR(idx2, R_NilValue)));
            UNPROTECT(3);
            ostackPushSexp(ctx, args);

            SEXP res;
            if (isObject(val)) {
                SEXP call = getSrcForCall(c, pc - 1, ctx);
                res = dispatchApply(call, val, args, symbol::Bracket, env, ctx);
                if (!res)
                    res =
                        do_subset_dflt(R_NilValue, symbol::Bracket, args, env);
            } else {
                res = do_subset_dflt(R_NilValue, symbol::Bracket, args, env);
            }

            ostackPopn(ctx, 4);

            R_Visible = TRUE;
            ostackPushSexp(ctx, res);
            NEXT();
        }

        INSTRUCTION(extract2_1_) {
            SEXP val = ostackSexpAt(ctx, 1);
            R_bcstack_t idx = ostackAt(ctx, 0);
            int i = -1;

            if (ATTRIB(val) != R_NilValue)
                goto fallback;

            if (stackObjIsSimpleScalar(idx, INTSXP)) {
                if (tryStackObjToInteger(idx) == NA_INTEGER)
                    goto fallback;
                i = tryStackObjToInteger(idx) - 1;
            } else if (stackObjIsSimpleScalar(idx, REALSXP)) {
                if (tryStackObjToReal(idx) == NA_REAL)
                    goto fallback;
                i = tryStackObjToReal(idx) - 1;
            } else if (stackObjIsSimpleScalar(idx, LGLSXP)) {
                if (tryStackObjToLogical(idx) == NA_LOGICAL)
                    goto fallback;
                i = tryStackObjToLogical(idx) - 1;
            } else {
                goto fallback;
            }

            if (i >= XLENGTH(val) || i < 0)
                goto fallback;

            R_bcstack_t res;
            switch (TYPEOF(val)) {

#define SIMPLECASE(vectype, vecaccess, veccreate)                              \
    case vectype: {                                                            \
        if (XLENGTH(val) == 1 && NO_REFERENCES(val)) {                         \
            res = sexpToStackObj(val);                                         \
        } else {                                                               \
            res = veccreate##StackObj(vecaccess(val)[i]);                      \
        }                                                                      \
        break;                                                                 \
    }

                SIMPLECASE(REALSXP, REAL, real);
                SIMPLECASE(INTSXP, INTEGER, int);
                SIMPLECASE(LGLSXP, LOGICAL, logical);
#undef SIMPLECASE

            case VECSXP: {
                res = sexpToStackObj(VECTOR_ELT(val, i));
                break;
            }

            default:
                goto fallback;
            }

            R_Visible = TRUE;
            ostackPopn(ctx, 2);
            ostackPush(ctx, res);
            NEXT();

        // ---------
        fallback : {
            SEXP res;
            PROTECT(val);
            SEXP idxSexp = ostackObjToSexpAt(idx, ctx, 1);
            PROTECT(idxSexp);
            SEXP args = CONS_NR(val, CONS_NR(idxSexp, R_NilValue));
            UNPROTECT(2);
            ostackPushSexp(ctx, args);
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
            ostackPopn(ctx, 3);

            R_Visible = TRUE;
            ostackPushSexp(ctx, res);
            NEXT();
        }
        }

        // TODO: Fast case
        INSTRUCTION(extract2_2_) {
            SEXP val = ostackSexpAt(ctx, 2);
            PROTECT(val);
            SEXP idx = ostackSexpAt(ctx, 1);
            PROTECT(idx);
            SEXP idx2 = ostackSexpAt(ctx, 0);
            PROTECT(idx2);
            SEXP args = CONS_NR(val, CONS_NR(idx, CONS_NR(idx2, R_NilValue)));
            UNPROTECT(3);
            ostackPushSexp(ctx, args);

            SEXP res;
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

            ostackPopn(ctx, 4);
            R_Visible = TRUE;
            ostackPushSexp(ctx, res);
            NEXT();
        }

        INSTRUCTION(subassign1_1_) {
            SEXP idx = ostackSexpAt(ctx, 0);
            PROTECT(idx);
            SEXP vec = ostackSexpAt(ctx, 1);
            PROTECT(vec);
            SEXP val = ostackSexpAt(ctx, 2);
            UNPROTECT(2);

            if (MAYBE_SHARED(vec)) {
                vec = Rf_duplicate(vec);
                ostackSetSexp(ctx, 1, vec);
            }

            SEXP args = CONS_NR(vec, CONS_NR(idx, CONS_NR(val, R_NilValue)));
            SET_TAG(CDDR(args), symbol::value);
            PROTECT(args);

            SEXP res = nullptr;
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
            ostackPopn(ctx, 3);
            UNPROTECT(1);

            ostackPushSexp(ctx, res);
            NEXT();
        }

        INSTRUCTION(subassign1_2_) {
            SEXP idx2 = ostackSexpAt(ctx, 0);
            PROTECT(idx2);
            SEXP idx1 = ostackSexpAt(ctx, 1);
            PROTECT(idx1);
            SEXP mtx = ostackSexpAt(ctx, 2);
            PROTECT(mtx);
            SEXP val = ostackSexpAt(ctx, 3);
            UNPROTECT(3);

            if (MAYBE_SHARED(mtx)) {
                mtx = Rf_duplicate(mtx);
                ostackSetSexp(ctx, 2, mtx);
            }

            SEXP args = CONS_NR(
                mtx, CONS_NR(idx1, CONS_NR(idx2, CONS_NR(val, R_NilValue))));
            SET_TAG(CDDDR(args), symbol::value);
            PROTECT(args);

            SEXP res = nullptr;
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
            ostackPopn(ctx, 4);
            UNPROTECT(1);

            ostackPushSexp(ctx, res);
            NEXT();
        }

        INSTRUCTION(subassign2_1_) {
            R_bcstack_t idx = ostackAt(ctx, 0);
            SEXP vec = ostackSexpAt(ctx, 1);
            R_bcstack_t val = ostackAt(ctx, 2);

            // Fast case, only if:
            // 1. vector isn't shared or an object
            // 2. vector is real and shape of value fits into real
            //      or vector is int and shape of value is int
            //      or vector is generic
            // 3. index is numerical and scalar, and in the vector's range
            if (NOT_SHARED(vec) && !isObject(vec)) { // 1
                SEXPTYPE vectorT = TYPEOF(vec);

                if ((vectorT == REALSXP &&
                     (stackObjIsSimpleScalar(val, INTSXP) ||
                      stackObjIsSimpleScalar(val, REALSXP))) ||
                    (vectorT == INTSXP &&
                     stackObjIsSimpleScalar(val, INTSXP)) ||
                    vectorT == VECSXP) { // 2
                    int idx_ = tryStackObjToIdx(idx);

                    if (idx_ >= 0 && idx_ < XLENGTH(vec)) {
                        switch (vectorT) {
                        case REALSXP:
                            REAL(vec)
                            [idx_] = stackObjIsSimpleScalar(val, REALSXP)
                                         ? tryStackObjToReal(val)
                                         : (double)tryStackObjToInteger(val);
                            break;
                        case INTSXP:
                            INTEGER(vec)[idx_] = tryStackObjToInteger(val);
                            break;
                        case VECSXP:
                            PROTECT(vec);
                            SET_VECTOR_ELT(vec, idx_,
                                           ostackObjToSexpAt(val, ctx, 2));
                            UNPROTECT(1);
                            break;
                        default:
                            assert(false);
                        }
                        ostackPopn(ctx, 3);

                        ostackPushSexp(ctx, vec);
                        NEXT();
                    }
                }
            }

            if (MAYBE_SHARED(vec)) {
                vec = Rf_duplicate(vec);
                ostackSetSexp(ctx, 1, vec);
            }

            PROTECT(vec);
            SEXP idxSexp = ostackObjToSexpAt(idx, ctx, 0);
            PROTECT(idxSexp);
            SEXP valSexp = ostackObjToSexpAt(val, ctx, 2);
            PROTECT(valSexp);
            SEXP args =
                CONS_NR(vec, CONS_NR(idxSexp, CONS_NR(valSexp, R_NilValue)));
            SET_TAG(CDDR(args), symbol::value);
            UNPROTECT(3);
            PROTECT(args);

            SEXP res = nullptr;
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
            ostackPopn(ctx, 3);
            UNPROTECT(1);

            ostackPushSexp(ctx, res);
            NEXT();
        }

        INSTRUCTION(subassign2_2_) {
            R_bcstack_t idx2 = ostackAt(ctx, 0);
            R_bcstack_t idx1 = ostackAt(ctx, 1);
            SEXP mtx = ostackSexpAt(ctx, 2);
            R_bcstack_t val = ostackAt(ctx, 3);

            // Fast case, only if:
            // 1. matrix isn't shared or an object
            // 2. matrix is real and shape of value fits into real
            //      or matrix is int and shape of value is int
            //      or matrix is generic
            // 3. index is numerical and scalar, and in the matrix's 2D range
            if (NOT_SHARED(mtx) && !isObject(mtx)) { // 1
                SEXPTYPE matrixT = TYPEOF(mtx);

                if ((matrixT == REALSXP &&
                     (stackObjIsSimpleScalar(val, INTSXP) ||
                      stackObjIsSimpleScalar(val, REALSXP))) ||
                    (matrixT == INTSXP &&
                     stackObjIsSimpleScalar(val, INTSXP)) ||
                    matrixT == VECSXP) { // 2
                    int idx1_ = tryStackObjToIdx(idx1);
                    int idx2_ = tryStackObjToIdx(idx2);

                    if (idx1_ >= 0 && idx1_ < Rf_ncols(mtx) && idx2_ >= 0 &&
                        idx2_ < Rf_nrows(mtx)) {
                        int idx_ = idx1_ + (idx2_ * Rf_nrows(mtx));
                        switch (matrixT) {
                        case REALSXP:
                            REAL(mtx)
                            [idx_] = stackObjIsSimpleScalar(val, REALSXP)
                                         ? tryStackObjToReal(val)
                                         : (double)tryStackObjToInteger(val);
                            break;
                        case INTSXP:
                            INTEGER(mtx)[idx_] = tryStackObjToInteger(val);
                            break;
                        case VECSXP:
                            PROTECT(mtx);
                            SET_VECTOR_ELT(mtx, idx_,
                                           ostackObjToSexpAt(val, ctx, 2));
                            UNPROTECT(1);
                            break;
                        default:
                            assert(false);
                        }
                        ostackPopn(ctx, 4);

                        ostackPushSexp(ctx, mtx);
                        NEXT();
                    }
                }
            }

            if (MAYBE_SHARED(mtx)) {
                mtx = Rf_duplicate(mtx);
                ostackSetSexp(ctx, 2, mtx);
            }

            PROTECT(mtx);
            SEXP idx1Sexp = ostackObjToSexpAt(idx1, ctx, 1);
            PROTECT(idx1Sexp);
            SEXP idx2Sexp = ostackObjToSexpAt(idx2, ctx, 0);
            PROTECT(idx2Sexp);
            SEXP valSexp = ostackObjToSexpAt(val, ctx, 3);
            PROTECT(valSexp);
            SEXP args = CONS_NR(
                mtx, CONS_NR(idx1Sexp,
                             CONS_NR(idx2Sexp, CONS_NR(valSexp, R_NilValue))));
            SET_TAG(CDDDR(args), symbol::value);
            UNPROTECT(4);
            PROTECT(args);

            SEXP res = nullptr;
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
            ostackPopn(ctx, 4);
            UNPROTECT(1);

            ostackPushSexp(ctx, res);
            NEXT();
        }

        INSTRUCTION(guard_fun_) {
#ifndef UNSOUND_OPTS
            SEXP sym = readConst(ctx, readImmediate());
#endif
            advanceImmediate();
#ifndef UNSOUND_OPTS
            SEXP res = readConst(ctx, readImmediate());
#endif
            advanceImmediate();
            advanceImmediate();
#ifndef UNSOUND_OPTS
            assert(res == Rf_findFun(sym, env) && "guard_fun_ fail");
#endif
            NEXT();
        }

        INSTRUCTION(deopt_) {
            SEXP r = readConst(ctx, readImmediate());
            advanceImmediate();
            SLOWASSERT(TYPEOF(r) == RAWSXP);
            SLOWASSERT(XLENGTH(r) >= (int)sizeof(DeoptMetadata));
            auto m = (DeoptMetadata*)DATAPTR(r);

#if 0
            size_t pos = 0;
            for (size_t i = 0; i < m->numFrames; ++i) {
                std::cout << "Code " << m->frames[i].code << "\n";
                std::cout << "Frame " << i << ":\n";
                std::cout << "  - env (" << pos << ")\n";
                Rf_PrintValue(ostackAt(ctx, pos++));
                for( size_t j = 0; j < m->frames[i].stackSize; ++j) {
                    std::cout << "  - stack (" << pos << ") " << j << "\n";
                    Rf_PrintValue(ostackSexpAt(ctx, pos++));
                }
            }
#endif

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
            SLOWASSERT(prim == Rf_findFun(Rf_install("seq"), env));

            R_bcstack_t from = ostackAt(ctx, 2);
            R_bcstack_t to = ostackAt(ctx, 1);
            R_bcstack_t by = ostackAt(ctx, 0);
            bool has_res = false;
            R_bcstack_t res;

            if (stackObjIsSimpleScalar(from, INTSXP) &&
                stackObjIsSimpleScalar(to, INTSXP) &&
                stackObjIsSimpleScalar(by, INTSXP)) {
                int f = tryStackObjToInteger(from);
                int t = tryStackObjToInteger(to);
                int b = tryStackObjToInteger(by);
                if (f != NA_INTEGER && t != NA_INTEGER && b != NA_INTEGER) {
                    if ((f < t && b > 0) || (t < f && b < 0)) {
                        int size = 1 + (t - f) / b;
                        SEXP resSexp = Rf_allocVector(INTSXP, size);
                        int v = f;
                        for (int i = 0; i < size; ++i) {
                            INTEGER(resSexp)[i] = v;
                            v += b;
                        }
                        has_res = true;
                        res = sexpToStackObj(resSexp);
                    } else if (f == t) {
                        has_res = true;
                        res = intStackObj(f);
                    }
                }
            }

            if (!has_res) {
                SLOWASSERT(from.tag != STACK_OBJ_SEXP ||
                           !isObject(from.u.sxpval));
                SEXP call = getSrcForCall(c, pc - 1, ctx);
                PROTECT(call);
                SEXP fromSexp = ostackObjToSexpAt(from, ctx, 2);
                PROTECT(fromSexp);
                SEXP toSexp = ostackObjToSexpAt(to, ctx, 1);
                PROTECT(toSexp);
                SEXP bySexp = ostackObjToSexpAt(by, ctx, 0);
                PROTECT(bySexp);
                SEXP argslist = CONS_NR(
                    fromSexp, CONS_NR(toSexp, CONS_NR(bySexp, R_NilValue)));
                UNPROTECT(4);
                ostackPushSexp(ctx, argslist);
                res = sexpToStackObj(
                    Rf_applyClosure(call, prim, argslist, env, R_NilValue));
                ostackPop(ctx);
            }

            ostackPopn(ctx, 3);
            ostackPush(ctx, res);
            NEXT();
        }

        INSTRUCTION(colon_) {
            R_bcstack_t lhs = ostackAt(ctx, 1);
            R_bcstack_t rhs = ostackAt(ctx, 0);
            SEXP res = nullptr;

            if (stackObjIsSimpleScalar(lhs, INTSXP)) {
                int from = tryStackObjToInteger(lhs);
                if (stackObjIsSimpleScalar(rhs, INTSXP)) {
                    int to = tryStackObjToInteger(rhs);
                    if (from != NA_INTEGER && to != NA_INTEGER) {
                        res = seq_int(from, to);
                    }
                } else if (stackObjIsSimpleScalar(rhs, REALSXP)) {
                    double to = tryStackObjToReal(rhs);
                    if (from != NA_INTEGER && to != NA_REAL && R_FINITE(to) &&
                        INT_MIN <= to && INT_MAX >= to && to == (int)to) {
                        res = seq_int(from, (int)to);
                    }
                }
            } else if (stackObjIsSimpleScalar(lhs, REALSXP)) {
                double from = tryStackObjToReal(lhs);
                if (stackObjIsSimpleScalar(rhs, INTSXP)) {
                    int to = tryStackObjToInteger(rhs);
                    if (from != NA_REAL && to != NA_INTEGER && R_FINITE(from) &&
                        INT_MIN <= from && INT_MAX >= from &&
                        from == (int)from) {
                        res = seq_int((int)from, to);
                    }
                } else if (stackObjIsSimpleScalar(rhs, REALSXP)) {
                    double to = tryStackObjToReal(rhs);
                    if (from != NA_REAL && to != NA_REAL && R_FINITE(from) &&
                        R_FINITE(to) && INT_MIN <= from && INT_MAX >= from &&
                        INT_MIN <= to && INT_MAX >= to && from == (int)from &&
                        to == (int)to) {
                        res = seq_int((int)from, (int)to);
                    }
                }
            }

            if (res == NULL) {
                BINOP_FALLBACK(":");
            } else {
                STORE_BINOP(sexpToStackObj(res));
            }
            NEXT();
        }

        INSTRUCTION(names_) {
            SEXP val = ostackPopSexp(ctx);
            ostackPushSexp(ctx, Rf_getAttrib(val, R_NamesSymbol));
            NEXT();
        }

        INSTRUCTION(set_names_) {
            SEXP name = ostackPopSexp(ctx);
            if (!isNull(name)) {
                PROTECT(name);
                SEXP val = ostackPopSexp(ctx);
                UNPROTECT(1);
                Rf_setAttrib(val, R_NamesSymbol, name);
                ostackPushSexp(ctx, val);
            }
            NEXT();
        }

        INSTRUCTION(alloc_) {
            R_bcstack_t val = ostackPop(ctx);
            SLOWASSERT(stackObjIsSimpleScalar(val, INTSXP));
            int type = readSignedImmediate();
            advanceImmediate();
            int size = tryStackObjToInteger(val);
            SEXP res = Rf_allocVector(type, size);
            ostackPushSexp(ctx, res);
            NEXT();
        }

        INSTRUCTION(length_) {
            R_bcstack_t val = ostackPop(ctx);
            R_xlen_t len = stackObjLength(val);
            ostackPushInt(ctx, len);
            NEXT();
        }

        INSTRUCTION(for_seq_size_) {
            R_bcstack_t seq = ostackAt(ctx, 0);
            // TODO: we should extract the length just once at the begining of
            // the loop and generally have somthing more clever here...
            int value;
            if (stackObjIsVector(seq)) {
                value = stackObjLength(seq);
            } else if (seq.tag == STACK_OBJ_SEXP &&
                       (Rf_isList(seq.u.sxpval) || isNull(seq.u.sxpval))) {
                value = Rf_length(seq.u.sxpval);
            } else {
                Rf_errorcall(R_NilValue, "invalid for() loop sequence");
            }
            // TODO: Even when the for loop sequence is an object, R won't
            // dispatch on it. Since in RIR we use the normals extract2_1
            // BC on it, we would. To prevent this we strip the object
            // flag here. What we should do instead, is use a non-dispatching
            // extract BC.
            if (seq.tag == STACK_OBJ_SEXP && isObject(seq.u.sxpval)) {
                SEXP seqSexp = Rf_duplicate(seq.u.sxpval);
                SET_OBJECT(seqSexp, 0);
                ostackSetSexp(ctx, 0, seqSexp);
            }
            ostackPushInt(ctx, value);
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
            R_bcstack_t val = ostackTop(ctx);
            if (val.tag == STACK_OBJ_SEXP) {
                ENSURE_NAMED(val.u.sxpval);
            }
            NEXT();
        }

        INSTRUCTION(set_shared_) {
            R_bcstack_t val = ostackTop(ctx);
            if (val.tag == STACK_OBJ_SEXP) {
                INCREMENT_NAMED(val.u.sxpval);
            }
            NEXT();
        }

        INSTRUCTION(make_unique_) {
            R_bcstack_t val = ostackTop(ctx);
            if (val.tag == STACK_OBJ_SEXP && MAYBE_SHARED(val.u.sxpval)) {
                val = sexpToStackObj(Rf_shallow_duplicate(val.u.sxpval));
                ostackSet(ctx, 0, val);
                SET_NAMED(val.u.sxpval, 1);
            }
            NEXT();
        }

        INSTRUCTION(beginloop_) {
            SLOWASSERT(env);
            int offset = readJumpOffset();
            advanceJump();
            loopTrampoline(c, ctx, env, callCtxt, pc, localsBase);
            pc += offset;
            SLOWASSERT(*pc == Opcode::endloop_);
            advanceOpcode();
            NEXT();
        }

        INSTRUCTION(endloop_) { return sexpToStackObj(loopTrampolineMarker); }

        INSTRUCTION(return_) {
            SEXP res = ostackSexpAt(ctx, 0);
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
    return ostackPop(ctx);
}

#pragma GCC diagnostic pop

SEXP evalRirCodeExtCaller(Code* c, InterpreterInstance* ctx, SEXP env) {
    return stackObjToSexp(evalRirCode(c, ctx, env, nullptr));
}

R_bcstack_t evalRirCode(Code* c, InterpreterInstance* ctx, SEXP env,
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

SEXP rirApplyClosure(SEXP ast, SEXP op, SEXP arglist, SEXP rho) {
    auto ctx = globalContext();

    RList args(arglist);
    size_t nargs = 0;
    std::vector<Immediate> names;
    for (auto arg = args.begin(), end = args.end(); arg != end; ++arg) {
        ostackPushSexp(ctx, *arg);
        if (arg.hasTag()) {
            names.resize(nargs + 1);
            names[nargs] = Pool::insert(arg.tag());
        }
        nargs++;
    }
    if (!names.empty()) {
        names.resize(nargs);
    }

    CallContext call(nullptr, op, nargs, ast, ostackCellAt(ctx, nargs - 1),
                     nullptr, names.empty() ? nullptr : names.data(), rho,
                     Assumptions(), ctx);
    call.arglist = arglist;

    auto res = rirCall(call, ctx);
    ostackPopn(ctx, call.passedArgs);
    return res;
}

SEXP rirEval_f(SEXP what, SEXP env) {
    SLOWASSERT(TYPEOF(what) == EXTERNALSXP);

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
}
} // namespace rir
