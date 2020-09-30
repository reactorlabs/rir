#include "interp.h"
#include "ArgsLazyData.h"
#include "LazyEnvironment.h"
#include "R/Funtab.h"
#include "R/Printing.h"
#include "R/RList.h"
#include "R/Symbols.h"
#include "cache.h"
#include "compiler/compiler.h"
#include "compiler/parameter.h"
#include "event_counters.h"
#include "ir/Deoptimization.h"
#include "runtime/TypeFeedback_inl.h"
#include "safe_force.h"
#include "utils/Pool.h"

#include <assert.h>
#include <deque>
#include <libintl.h>
#include <set>
#include <unordered_set>

#define NOT_IMPLEMENTED assert(false)

#undef eval

extern "C" {
extern SEXP Rf_NewEnvironment(SEXP, SEXP, SEXP);
extern Rboolean R_Visible;
}

namespace rir {

// #define PRINT_INTERP
// #define PRINT_STACK_SIZE 10
#ifdef PRINT_INTERP
static void printInterp(Opcode* pc, Code* c, InterpreterInstance* ctx) {
#ifdef PRINT_STACK_SIZE
    // Prevent printing instructions (and recursing) while printing stack
    static bool printingStackSize = false;
    if (printingStackSize)
        return;

    // Print stack
    printingStackSize = true;
    std::cout << "#; Stack:";
    for (int i = 0;; i++) {
        SEXP sexp = ostack_at(ctx, i);
        if (sexp == nullptr || ostack_length(ctx) - i == 0)
            break;
        else if (i == PRINT_STACK_SIZE) {
            std::cout << " ...";
            break;
        }
        std::cout << " " << dumpSexp(sexp);
    }
    std::cout << "\n";
    printingStackSize = false;
#endif
    // Print source
    unsigned sidx = c->getSrcIdxAt(pc, true);
    if (sidx != 0) {
        SEXP src = src_pool_at(ctx, sidx);
        std::cout << "#; " << dumpSexp(src) << "\n";
    }
    // Print bc
    BC bc = BC::decode(pc, c);
    std::cout << "#";
    bc.print(std::cout);
}

static void printLastop() { std::cout << "> lastop\n"; }
#endif

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
    op_##name: /* debug(c, pc, #name, ostack_length(ctx) - bp, ctx); */
#ifdef PRINT_INTERP
#define NEXT()                                                                 \
    (__extension__({                                                           \
        printInterp(pc, c, ctx);                                               \
        goto* opAddr[static_cast<uint8_t>(advanceOpcode())];                   \
    }))
#define LASTOP                                                                 \
    { printLastop(); }
#else
#define NEXT()                                                                 \
    (__extension__({ goto* opAddr[static_cast<uint8_t>(advanceOpcode())]; }))
#define LASTOP                                                                 \
    {}
#endif
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

static RCNTXT* deoptimizationStartedAt = nullptr;

static bool isDeoptimizing() {
    if (!deoptimizationStartedAt)
        return false;
    RCNTXT* cur = (RCNTXT*)R_GlobalContext;
    while (cur) {
        if (cur == deoptimizationStartedAt)
            return true;
        cur = cur->nextcontext;
    }
    deoptimizationStartedAt = nullptr;
    return false;
}
static void startDeoptimizing() {
    deoptimizationStartedAt = (RCNTXT*)R_GlobalContext;
}
static void endDeoptimizing() { deoptimizationStartedAt = nullptr; }

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

SEXP evalRirCode(Code*, InterpreterInstance*, SEXP, const CallContext*, Opcode*,
                 R_bcstack_t*, BindingCache*);

static RIR_INLINE SEXP createPromise(Code* code, SEXP env) {
    SEXP p = Rf_mkPROMISE(code->container(), env);
    return p;
}

typedef struct RPRSTACK {
    SEXP promise;
    struct RPRSTACK* next;
} RPRSTACK;
extern "C" struct RPRSTACK* R_PendingPromises;

RIR_INLINE SEXP evaluatePromise(SEXP e, InterpreterInstance* ctx, Opcode* pc) {
    // if already evaluated, return the value
    if (PRVALUE(e) && PRVALUE(e) != R_UnboundValue) {
        e = PRVALUE(e);
        assert(TYPEOF(e) != PROMSXP);
        return e;
    } else if (TYPEOF(PRCODE(e)) != EXTERNALSXP) {
        assert(!pc);
        return forcePromise(e);
    } else {
        SEXP val;
        if (PRSEEN(e)) {
            if (PRSEEN(e) == 1)
                errorcall(NULL,
                          "promise already under evaluation: recursive default "
                          "argument reference or earlier problems?");
            else {
                /* set PRSEEN to 1 to avoid infinite recursion */
                SET_PRSEEN(e, 1);
                warningcall(NULL, "restarting interrupted promise evaluation");
            }
        }
        SET_PRSEEN(e, 1);

        RPRSTACK prstack;
        prstack.promise = e;
        prstack.next = R_PendingPromises;
        R_PendingPromises = &prstack;
        val = evalRirCode(Code::unpack(PRCODE(e)), ctx, e->u.promsxp.env,
                          nullptr, pc, nullptr, nullptr);
        R_PendingPromises = prstack.next;
        SET_PRSEEN(e, 0);
        SET_PRVALUE(e, val);
        ENSURE_NAMEDMAX(val);
        SET_PRENV(e, R_NilValue);

        assert(TYPEOF(val) != PROMSXP && "promise returned promise");
        return val;
    }
}

SEXP rirForcePromise(SEXP e) { return evaluatePromise(e, globalContext()); }

void jit(SEXP cls, SEXP name, InterpreterInstance* ctx) {
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

static SEXP materializeCallerEnv(CallContext& callCtx,
                                 InterpreterInstance* ctx) {
    if (auto le = LazyEnvironment::check(callCtx.callerEnv)) {
        if (le->materialized())
            callCtx.callerEnv = le->materialized();
        else
            callCtx.callerEnv = materialize(callCtx.callerEnv);
    }
    SLOWASSERT(callCtx.callerEnv == symbol::delayedEnv ||
               TYPEOF(callCtx.callerEnv) == ENVSXP ||
               callCtx.callerEnv == R_NilValue);
    return callCtx.callerEnv;
}

SEXP createLegacyArgsListFromStackValues(size_t length, const R_bcstack_t* args,
                                         const Immediate* names,
                                         bool eagerCallee,
                                         InterpreterInstance* ctx) {
    assert(args && "Cannot materialize promargs for statically reordered "
                   "arguments. Static Call to UseMethod function?");
    SEXP result = R_NilValue;
    SEXP pos = result;

    for (size_t i = 0; i < length; ++i) {

        SEXP name = names ? cp_pool_at(ctx, names[i]) : R_NilValue;

        SEXP arg = ostack_at_cell(args + i);

        if (eagerCallee && TYPEOF(arg) == PROMSXP) {
            arg = evaluatePromise(arg, ctx);
        }
        // This is to ensure we pass named arguments to GNU-R builtins because
        // who knows what assumptions does GNU-R do??? We SHOULD test this.
        if (TYPEOF(arg) != PROMSXP)
            ENSURE_NAMED(arg);
        __listAppend(&result, &pos, arg, name);
    }

    if (result != R_NilValue)
        UNPROTECT(1);

    return result;
}

SEXP materialize(SEXP rirDataWrapper) {
    auto createEnvironment = [](InterpreterInstance* ctx, SEXP wrapper_) {
        auto wrapper = LazyEnvironment::unpack(wrapper_);
        assert(!wrapper->materialized());

        SEXP arglist = R_NilValue;
        auto names = wrapper->names;
        for (size_t i = 0; i < wrapper->nargs; ++i) {
            SEXP val = wrapper->getArg(i);
            if (val == R_UnboundValue)
                continue;
            SEXP name = cp_pool_at(ctx, names[i]);
            bool isMissing = wrapper->missing[i];
            if (TYPEOF(name) == LISTSXP) {
                name = CAR(name);
            }
            arglist = CONS_NR(val, arglist);
            SET_TAG(arglist, name);
            SET_MISSING(arglist, isMissing ? 2 : 0);
        }

        SEXP environment =
            Rf_NewEnvironment(R_NilValue, arglist, wrapper->getParent());
        wrapper->materialized(environment);
        return environment;
    };

    if (auto promargs = ArgsLazyDataContent::check(rirDataWrapper)) {
        return promargs->createArgsLists();
    } else if (auto lazyEnv = LazyEnvironment::check(rirDataWrapper)) {
        auto newEnv = createEnvironment(globalContext(), rirDataWrapper);
        Rf_setAttrib(newEnv, symbol::delayedEnv, rirDataWrapper);
        lazyEnv->clear();
        RCNTXT* cur = (RCNTXT*)R_GlobalContext;
        while (cur) {
            if (cur->cloenv == rirDataWrapper)
                cur->cloenv = newEnv;
            if (cur->sysparent == rirDataWrapper)
                cur->sysparent = newEnv;
            cur = cur->nextcontext;
        }
        return newEnv;
    }
    assert(false);
    return nullptr;
}

static RIR_INLINE SEXP createLegacyLazyArgsList(CallContext& call,
                                                InterpreterInstance* ctx) {
    return createLegacyArgsListFromStackValues(
        call.suppliedArgs, call.stackArgs, call.names, false, ctx);
}

static RIR_INLINE SEXP createLegacyArgsList(CallContext& call,
                                            InterpreterInstance* ctx) {
    return createLegacyArgsListFromStackValues(call.suppliedArgs,
                                               call.stackArgs, call.names,
                                               call.hasEagerCallee(), ctx);
}

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

#define UI_COUNT_DELTA 1000

static unsigned int count = 0;

// Interrupt Signal Checker - Allows for Ctrl - C functionality to exit out
// of infinite loops
void checkUserInterrupt() {
    if (++count > UI_COUNT_DELTA) {
        R_CheckUserInterrupt();
        R_RunPendingFinalizers();
        count = 0;
    }
}

void recordDeoptReason(SEXP val, const DeoptReason& reason) {
    Opcode* pos = (Opcode*)reason.srcCode + reason.originOffset;
    switch (reason.reason) {
    case DeoptReason::DeadBranchReached: {
        assert(*pos == Opcode::record_test_);
        ObservedTest* feedback = (ObservedTest*)(pos + 1);
        feedback->seen = ObservedTest::Both;
        break;
    }
    case DeoptReason::Typecheck: {
        assert(*pos == Opcode::record_type_);
        ObservedValues* feedback = (ObservedValues*)(pos + 1);
        feedback->record(val);
        if (TYPEOF(val) == PROMSXP) {
            if (PRVALUE(val) == R_UnboundValue &&
                feedback->stateBeforeLastForce < ObservedValues::promise)
                feedback->stateBeforeLastForce = ObservedValues::promise;
            else if (feedback->stateBeforeLastForce <
                     ObservedValues::evaluatedPromise)
                feedback->stateBeforeLastForce =
                    ObservedValues::evaluatedPromise;
        }
        break;
    }
    case DeoptReason::Calltarget: {
        assert(*pos == Opcode::record_call_);
        ObservedCallees* feedback = (ObservedCallees*)(pos + 1);
        feedback->record(reason.srcCode, val);
        assert(feedback->taken > 0);
        break;
    }
    case DeoptReason::EnvStubMaterialized: {
        reason.srcCode->flags.set(Code::NeedsFullEnv);
        break;
    }
    case DeoptReason::None:
        assert(false);
        break;
    }
}

const static SEXP loopTrampolineMarker = (SEXP)0x7007;
static void loopTrampoline(Code* c, InterpreterInstance* ctx, SEXP env,
                           const CallContext* callCtxt, Opcode* pc,
                           R_bcstack_t* localsBase, BindingCache* cache) {
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
    SEXP res = evalRirCode(c, ctx, env, callCtxt, pc, localsBase, cache);
    assert(res == loopTrampolineMarker);
    Rf_endcontext(&cntxt);
}

static SEXP inlineContextTrampoline(Code* c, const CallContext* callCtx,
                                    SEXP ast, SEXP sysparent, SEXP op,
                                    InterpreterInstance* ctx, Opcode* pc,
                                    R_bcstack_t* localsBase,
                                    BindingCache* cache, int32_t stackOffset) {
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
                return evalRirCode(c, ctx, cntxt.cloenv, callCtx, pc, nullptr,
                                   nullptr);
            } else {
                // Non-local return through Rf_findcontext restores the ostack
                // to the size at the push_context_ location. However that is
                // not what we want, since there might be a stack difference
                // between push and pop context. Thus we need to account for
                // the relative stack height difference between push and pop
                // context here. (This diff is calculated by the CodeVerifier)
                R_BCNodeStackTop += stackOffset;
                return R_ReturnedValue;
            }
        }
        return evalRirCode(c, ctx, sysparent, callCtx, pc, localsBase, cache);
    };

    // execute the inlined function
    auto res = trampoline();
    endClosureContext(&cntxt, res);
    return res;
}

static RIR_INLINE SEXP legacySpecialCall(CallContext& call,
                                         InterpreterInstance* ctx) {
    assert(call.ast != R_NilValue);

    // get the ccode
    CCODE f = getBuiltin(call.callee);
    int flag = getFlag(call.callee);
    R_Visible = static_cast<Rboolean>(flag != 1);
    // call it with the AST only
    SEXP result = f(call.ast, call.callee, CDR(call.ast),
                    materializeCallerEnv(call, ctx));
    if (flag < 2)
        R_Visible = static_cast<Rboolean>(flag != 1);
    return result;
}

static RIR_INLINE SEXP legacyCallWithArgslist(CallContext& call, SEXP argslist,
                                              InterpreterInstance* ctx) {
    if (TYPEOF(call.callee) == BUILTINSXP) {
        // get the ccode
        CCODE f = getBuiltin(call.callee);
        int flag = getFlag(call.callee);
        if (flag < 2)
            R_Visible = static_cast<Rboolean>(flag != 1);
        // call it
        SEXP result =
            f(call.ast, call.callee, argslist, materializeCallerEnv(call, ctx));
        if (flag < 2)
            R_Visible = static_cast<Rboolean>(flag != 1);
        return result;
    }

    assert(TYPEOF(call.callee) == CLOSXP &&
           TYPEOF(BODY(call.callee)) != EXTERNALSXP);
    return Rf_applyClosure(call.ast, call.callee, argslist,
                           materializeCallerEnv(call, ctx), R_NilValue);
}

static RIR_INLINE SEXP legacyCall(CallContext& call, InterpreterInstance* ctx) {
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
}

static SEXP findRootPromise(SEXP p) {
    if (TYPEOF(p) == PROMSXP) {
        while (TYPEOF(PREXPR(p)) == PROMSXP) {
            p = PREXPR(p);
        }
    }
    return p;
}

void inferCurrentContext(CallContext& call, size_t formalNargs,
                         InterpreterInstance* ctx) {
    Context& given = call.givenContext;

    if (call.suppliedArgs <= formalNargs) {
        given.add(Assumption::NotTooManyArguments);
        given.numMissing(formalNargs - call.suppliedArgs);
    }

    given.add(Assumption::NoExplicitlyMissingArgs);

    auto testArg = [&](size_t i) {
        SEXP arg = call.stackArg(i);
        bool isEager = true;

        // An explicitly missing arg, such as f(,1)
        if (arg == R_MissingArg)
            given.remove(Assumption::NoExplicitlyMissingArgs);

        bool reflectionPossible = false;

        if (TYPEOF(arg) == PROMSXP) {
            auto prom = arg;
            arg = PRVALUE(arg);

            // For Lazy promises, lets try to figure out where it points to.
            if (arg == R_UnboundValue) {
                reflectionPossible = true;
                isEager = false;
                // If this is a simple promise, that just looks up an eager
                // value we do not reset the no-reflection flag. The callee
                // can assume that (as long as he does not trigger any other
                // reflection) evaluating this promise does not trigger
                // reflection either.
                while (true) {
                    SEXP sym = nullptr;
                    auto pr = Code::check(PREXPR(prom));
                    SEXP v = PRVALUE(prom);

                    if (v == R_MissingArg) {
                        reflectionPossible = false;
                        break;
                    }

                    // Let's try to find out if this promise is a trivial
                    // expression (i.e. just a name lookup) and if that lookup
                    // can be easily resolved.
                    if (v == R_UnboundValue) {
                        if (TYPEOF(PREXPR(prom)) == SYMSXP) {
                            sym = PREXPR(prom);
                        } else if (pr) {
                            sym = pr->trivialExpr;
                        }
                        if (sym) {
                            if (auto le = LazyEnvironment::check(
                                    prom->u.promsxp.env)) {
                                v = le->getArg(sym);
                            } else {
                                v = Rf_findVar(sym, PRENV(prom));
                            }
                        }
                    }

                    if (pr && pr->flags.contains(Code::NoReflection))
                        reflectionPossible = false;

                    // This is truly lazy and we did not manage to lookup
                    // anything
                    if (v == R_UnboundValue)
                        break;

                    if (TYPEOF(v) != PROMSXP) {
                        reflectionPossible = false;
                        arg = v;
                        break;
                    }
                    prom = v;
                }
            }
        }

        // Eager in the context translates to the notLazy().notMissing()
        // type in pir. Thus we need to ensure that we don't set it for
        // wrapped missings.
        if (arg != R_MissingArg && isEager) {
            given.setEager(i);
            SLOWASSERT(TYPEOF(call.stackArg(i)) != PROMSXP ||
                       PRVALUE(call.stackArg(i)) != R_UnboundValue);
        }

        if (!reflectionPossible) {
            given.setNonRefl(i);
        }

        // Without isEager, these are the results of executing a trivial
        // expression, given no reflective change happens.
        if (arg != R_UnboundValue) {
            if (!isObject(arg))
                given.setNotObj(i);
            if (IS_SIMPLE_SCALAR(arg, REALSXP))
                given.setSimpleReal(i);
            if (IS_SIMPLE_SCALAR(arg, INTSXP))
                given.setSimpleInt(i);
        }
    };

    given.add(Assumption::CorrectOrderOfArguments);

    SEXP formals = FORMALS(call.callee);
    for (size_t i = 0; i < call.suppliedArgs; ++i) {
        testArg(i);
        if (call.hasNames()) {
            auto name = call.name(i, ctx);
            if (name != R_NilValue && name != TAG(formals))
                given.remove(Assumption::CorrectOrderOfArguments);
            formals = CDR(formals);
        }
    }
}

// Watch out: this changes call.nargs! To clean up after the call, you need to
// pop call.nargs number of arguments (which now might be more than the number
// of actually supplied arguments).
static RIR_INLINE void supplyMissingArgs(CallContext& call,
                                         const Function* fun) {
    auto context = fun->context();
    auto expected = fun->expectedNargs();
    assert(expected >= call.suppliedArgs ||
           !context.includes(Assumption::NotTooManyArguments));
    assert(expected == call.suppliedArgs ||
           !context.includes(Assumption::NoExplicitlyMissingArgs));
    if (expected > call.suppliedArgs) {
        for (size_t i = 0; i < expected - call.suppliedArgs; ++i)
            ostack_push(ctx, R_MissingArg);
        call.passedArgs = expected;
    }
}

unsigned pir::Parameter::RIR_WARMUP =
    getenv("PIR_WARMUP") ? atoi(getenv("PIR_WARMUP")) : 3;
unsigned pir::Parameter::DEOPT_ABANDON =
    getenv("PIR_DEOPT_ABANDON") ? atoi(getenv("PIR_DEOPT_ABANDON")) : 10;

static unsigned serializeCounter = 0;

// Call a RIR function. Arguments are still untouched.
RIR_INLINE SEXP rirCall(CallContext& call, InterpreterInstance* ctx) {
    SEXP body = BODY(call.callee);
    bool bodyPreserved = false;
    if (pir::Parameter::RIR_SERIALIZE_CHAOS) {
        serializeCounter++;
        if (serializeCounter == pir::Parameter::RIR_SERIALIZE_CHAOS) {
            body = copyBySerial(body);
            PROTECT(body);
            bodyPreserved = true;
            serializeCounter = 0;
        }
    }
    assert(DispatchTable::check(body));

    auto table = DispatchTable::unpack(body);

    inferCurrentContext(call, table->baseline()->signature().formalNargs(),
                        ctx);
    Function* fun = dispatch(call, table);
    fun->registerInvocation();

    if (!isDeoptimizing() && RecompileHeuristic(table, fun)) {
        Context given = call.givenContext;
        // addDynamicAssumptionForOneTarget compares arguments with the
        // signature of the current dispatch target. There the number of
        // arguments might be off. But we want to force compiling a new version
        // exactly for this number of arguments, thus we need to add this as an
        // explicit assumption.

        fun->clearDisabledAssumptions(given);
        if (RecompileCondition(table, fun, given)) {
            if (given.includes(pir::Compiler::minimalContext)) {
                DoRecompile(fun, call.ast, call.callee, given, ctx);
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
        SEXP env;
        if (call.givenContext.includes(Assumption::StaticallyArgmatched)) {
            auto formals = FORMALS(call.callee);
            env = Rf_NewEnvironment(formals, arglist, CLOENV(call.callee));

            // Add missing arguments. Statically argmatched means that still
            // some missing args might need to be supplied.
            if (!call.givenContext.includes(
                    Assumption::NoExplicitlyMissingArgs) ||
                call.passedArgs != fun->nargs()) {
                auto f = formals;
                auto a = arglist;
                SEXP prevA = nullptr;
                size_t pos = 0;
                while (f != R_NilValue) {
                    if (a == R_NilValue) {
                        a = CONS_NR(R_MissingArg, R_NilValue);
                        SET_TAG(a, TAG(f));
                        SET_MISSING(a, 2);
                        if (auto dflt = fun->defaultArg(pos)) {
                            SETCAR(a, createPromise(dflt, env));
                        }
                        if (prevA) {
                            SETCDR(prevA, a);
                        } else {
                            assert(arglist == R_NilValue);
                            SET_FRAME(env, a);
                        }
                    } else if (CAR(a) == R_MissingArg) {
                        if (auto dflt = fun->defaultArg(pos))
                            SETCAR(a, createPromise(dflt, env));
                    }

                    f = CDR(f);
                    prevA = a;
                    a = CDR(a);
                    pos++;
                }
            }

            // Currently we cannot recreate the original arglist if we
            // statically reordered arguments. TODO this needs to be fixed
            // by remembering the original order.
            if (auto a = ArgsLazyDataContent::check(arglist))
                a->args = nullptr;
            else
                arglist = symbol::delayedArglist;
        } else {
            env = closureArgumentAdaptor(call, arglist, R_NilValue);
        }
        PROTECT(env);
        result = rirCallTrampoline(call, fun, env, arglist, ctx);
        UNPROTECT(2);
    } else {
            // Instead of a SEXP with the argslist we create an
            // structure with the information needed to recreate
            // the list lazily if the gnu-r interpreter needs it
            ArgsLazyData lazyArgs(call.suppliedArgs, call.stackArgs, call.names,
                                  ctx);
            if (!arglist)
                arglist = (SEXP)&lazyArgs;

            // Currently we cannot recreate the original arglist if we
            // statically reordered arguments. TODO this needs to be fixed
            // by remembering the original order.
            if (call.givenContext.includes(Assumption::StaticallyArgmatched))
                lazyArgs.content.args = nullptr;
            supplyMissingArgs(call, fun);
            result = rirCallTrampoline(call, fun, arglist, ctx);
    }

    if (bodyPreserved)
        UNPROTECT(1);

    assert(result);

    assert(!fun->flags.contains(Function::Deopt));
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

SEXP builtinCall(CallContext& call, InterpreterInstance* ctx) {
    if (!call.hasNames()) {
        SEXP res = tryFastBuiltinCall(call, ctx);
        if (res) {
            int flag = getFlag(call.callee);
            if (flag < 2)
                R_Visible = static_cast<Rboolean>(flag != 1);
            return res;
        }
#ifdef DEBUG_SLOWCASES
        SLOWCASE_COUNTER.count("builtin", call, ctx);
#endif
    }
    return legacyCall(call, ctx);
}

static RIR_INLINE SEXP specialCall(CallContext& call,
                                   InterpreterInstance* ctx) {
    if (!call.hasNames()) {
        SEXP res = tryFastSpecialCall(call, ctx);
        if (res)
            return res;
#ifdef DEBUG_SLOWCASES
        SLOWCASE_COUNTER.count("special", call, ctx);
#endif
    }
    return legacySpecialCall(call, ctx);
}

SEXP doCall(CallContext& call, InterpreterInstance* ctx) {
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

SEXP dispatchApply(SEXP ast, SEXP obj, SEXP actuals, SEXP selector,
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

// TODO implement division and maybe others
enum class Binop { PLUSOP, MINUSOP, TIMESOP };
enum class Unop { PLUSOP, MINUSOP };
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
                case Binop::PLUSOP:                                            \
                    int_res =                                                  \
                        R_integer_plus(*INTEGER(lhs), *INTEGER(rhs), &naflag); \
                    break;                                                     \
                case Binop::MINUSOP:                                           \
                    int_res = R_integer_minus(*INTEGER(lhs), *INTEGER(rhs),    \
                                              &naflag);                        \
                    break;                                                     \
                case Binop::TIMESOP:                                           \
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
            case Unop::PLUSOP:                                                 \
                *INTEGER(res) = R_integer_uplus(*INTEGER(val), &naflag);       \
                break;                                                         \
            case Unop::MINUSOP:                                                \
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

SEXP seq_int(int n1, int n2) {
    int n = n1 <= n2 ? n2 - n1 + 1 : n1 - n2 + 1;
    SEXP ans = Rf_allocVector(INTSXP, n);
    int* data = INTEGER(ans);
    int64_t current = n1;
    int64_t end = n2;
    if (current <= end) {
        while (current <= end)
            *data++ = current++;
    } else {
        while (current >= end)
            *data++ = current--;
    }
    return ans;
}

bool isMissing(SEXP symbol, SEXP environment, Code* code, Opcode* pc) {
    SEXP val = R_findVarLocInFrame(environment, symbol).cell;
    if (val == NULL) {
        if (code)
            Rf_errorcall(getSrcAt(code, pc - 1, globalContext()),
                         "'missing' can only be used for arguments");
        else
            Rf_errorcall(R_NilValue,
                         "'missing' can only be used for arguments");
    }

    if (MISSING(val) || CAR(val) == R_MissingArg)
        return true;

    val = CAR(val);
    if (TYPEOF(val) != PROMSXP)
        return false;

    val = findRootPromise(val);
    if (!isSymbol(PREXPR(val))) {
        return false;
    } else {
        if (PREXPR(val) == R_MissingArg)
            return true;
        if (auto le = LazyEnvironment::check(val->u.promsxp.env)) {
            return le->isMissing(PREXPR(val));
        }
        return R_isMissing(PREXPR(val), PRENV(val));
    }
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
                            RCNTXT* currentContext) {
    size_t excessStack = stackHeight;

    FrameInfo& f = deoptData->frames[pos];
    stackHeight -= f.stackSize + 1;
    SEXP deoptEnv = ostack_at(ctx, stackHeight);
    auto code = f.code;
    code->registerInvocation();

    bool outermostFrame = pos == deoptData->numFrames - 1;
    bool innermostFrame = pos == 0;
    bool inPromise = f.inPromise;

    if (outermostFrame)
        startDeoptimizing();

    if (auto le = LazyEnvironment::check(deoptEnv)) {
        if (le->materialized())
            deoptEnv = le->materialized();
    }

    RCNTXT fake;
    RCNTXT* cntxt;
    if (inPromise) {
        cntxt = currentContext;
    } else {
        RCNTXT* originalCntxt = findFunctionContextFor(deoptEnv);
        if (originalCntxt) {
            cntxt = originalCntxt;
        } else {
            // NOTE: this assert triggers if we can't find the context of the
            // current function. Usually the reason is that a wrong environment
            // is stored in the context.
            assert(!outermostFrame && "Cannot find outermost function context");
            // If the inlinee had no context, we need to synthesize one
            // TODO: need to add ast and closure to the deopt metadata to create
            // a complete context
            cntxt = &fake;
            initClosureContext(R_NilValue, cntxt, deoptEnv, sysparent,
                               FRAME(sysparent), R_NilValue);
        }
    }

    if (auto le = LazyEnvironment::check(deoptEnv)) {
        assert(!le->materialized());
        deoptEnv = materialize(deoptEnv);
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
        if (!outermostFrame && !inPromise) {
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
                                   stackHeight, cntxt);
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
        assert(
            ostack_top() == deoptEnv ||
            (LazyEnvironment::check(ostack_top()) &&
             LazyEnvironment::check(ostack_top())->materialized() == deoptEnv));
        ostack_pop(ctx);
        if (!innermostFrame)
            ostack_push(ctx, res);
        if (inPromise) {
            SEXP p = createPromise(code, deoptEnv);
            PROTECT(p);
            auto r = evaluatePromise(p, ctx, f.pc);
            UNPROTECT(1);
            return r;
        }
        return evalRirCode(code, ctx, cntxt->cloenv, callCtxt, f.pc, nullptr,
                           nullptr);
    };

    SEXP res = trampoline();
    assert((size_t)ostack_length(ctx) == frameBaseSize);

    if (!outermostFrame) {
        if (!inPromise)
            endClosureContext(cntxt, res);
    } else {
        // Deopt in promise is only possible if the promise is not the outermost
        // frame. The reason is that the promise does not have a context
        // associated and we can therefore not jump out of deoptimized promise
        // evaluation. Therefore promises should only occur in inner frames
        // during deoptimization.
        assert(!inPromise);
        endDeoptimizing();
        assert(findFunctionContextFor(deoptEnv) == cntxt);
        // long-jump out of all the inlined contexts
        Rf_findcontext(CTXT_BROWSER | CTXT_FUNCTION, cntxt->cloenv, res);
        assert(false);
    }

    ostack_push(ctx, res);
}

#ifdef ENABLE_EVENT_COUNTERS
static unsigned EnvAllocated =
    EventCounters::instance().registerCounter("env allocated");
static unsigned EnvStubAllocated =
    EventCounters::instance().registerCounter("envstub allocated");
#endif

size_t expandDotDotDotCallArgs(InterpreterInstance* ctx, size_t n,
                               Immediate* names_, SEXP env, bool explicitDots) {
    std::vector<SEXP> args;
    std::vector<SEXP> names;
    bool hasNames = false;
    for (size_t i = 0; i < n; ++i) {
        auto arg = ostack_at(ctx, n - i - 1);
        auto name = cp_pool_at(ctx, names_[i]);
        if (name != R_DotsSymbol) {
            args.push_back(arg);
            names.push_back(name);
            if (name != R_NilValue)
                hasNames = true;
        } else {
            SEXP ellipsis = arg;
            if (ellipsis == R_DotsSymbol)
                ellipsis = Rf_findVar(R_DotsSymbol, env);

            if (TYPEOF(ellipsis) == PROMSXP)
                ellipsis = evaluatePromise(ellipsis, ctx);

            if (TYPEOF(ellipsis) == DOTSXP) {
                while (ellipsis != R_NilValue) {
                    auto arg = CAR(ellipsis);
                    if (TYPEOF(arg) == LANGSXP || TYPEOF(arg) == SYMSXP)
                        arg = Rf_mkPROMISE(arg, env);
                    args.push_back(arg);
                    names.push_back(TAG(ellipsis));
                    if (TAG(ellipsis) != R_NilValue)
                        hasNames = true;
                    ellipsis = CDR(ellipsis);
                }
            } else if (ellipsis == R_MissingArg) {
                // empty ... occurring in the middle of an argument list needs
                // to be explicit, since pir optimized functions expect it that
                // way.
                if (explicitDots) {
                    args.push_back(R_MissingArg);
                    names.push_back(R_NilValue);
                }
            } else if (ellipsis == R_NilValue) {
            } else {
                Rf_PrintValue(ellipsis);
                assert(ellipsis == R_UnboundValue);
            }
        }
    }
    if (hasNames) {
        SEXP namesStore =
            Rf_allocVector(RAWSXP, sizeof(Immediate) * names.size());
        ostack_popn(ctx, n);
        ostack_push(ctx, namesStore);
        {
            Immediate* nstore = (Immediate*)DATAPTR(namesStore);
            for (const auto& n : names) {
                *nstore = Pool::insert(n);
                nstore++;
            }
        }
    } else {
        ostack_popn(ctx, n);
        ostack_push(ctx, R_NilValue);
    }

    for (const auto& a : args)
        ostack_push(ctx, a);
    return args.size();
}

bool isColonFastcase(SEXP lhs, SEXP rhs) {
    // In order for the fastcase 2 conditions must be met:
    // - Either lhs or rhs must not be factors
    // - rhs must not be coerced to INT_MIN or INT_MAX (because we expect lhs >
    // rhs or lhs < rhs). To ensure this:
    //   - lhs must not be a number which gets coerced into an integer
    //   (implying, it can't be a real/complex which is integral)
    //   - rhs must not be a number which gets coerced into INT_MIN or INT_MAX
    //   (implying, it can't be the real/complex representation of INT_MIN or
    //   INT_MAX)
    if (inherits(lhs, "factor") && inherits(rhs, "factor"))
        return false;

    // TODO(o):
    // I don't like this part of the condition. It prevents us from constant
    // folding the colonEffects instruction. Can we do this differently?
    if (XLENGTH(lhs) == 0 || XLENGTH(rhs) == 0)
        return true;

    switch (TYPEOF(lhs)) {
    case INTSXP:
    case LGLSXP:
        break;
    case REALSXP:
        if (!doubleCanBeCastedToInteger(*REAL(lhs)))
            return true;
        break;
    case CPLXSXP:
        if (!doubleCanBeCastedToInteger(COMPLEX(lhs)->r))
            return true;
        break;
    default:
        break;
    }

    // This is the case where LHS might be an INTSXP
    int coerced = 0;
    switch (TYPEOF(rhs)) {
    case INTSXP:
    case LGLSXP:
        coerced = *INTEGER(rhs);
        break;
    case REALSXP:
        if (!doubleCanBeCastedToInteger(*REAL(rhs)))
            return false;
        coerced = (int)*REAL(rhs);
        break;
    case CPLXSXP:
        if (!doubleCanBeCastedToInteger(COMPLEX(rhs)->r))
            return false;
        coerced = (int)COMPLEX(rhs)->r;
        break;
    default:
        return true;
    }
    return coerced != INT_MAX;
}

bool doubleCanBeCastedToInteger(double n) {
    double intpart;
    return n > ((double)INT_MIN - 1) && n < ((double)INT_MAX + 1) &&
           std::modf(n, &intpart) == 0.0;
}

// int because the llvm backend represents bools as int
int colonInputEffects(SEXP lhs, SEXP rhs, unsigned srcIdx) {
    auto getSrc = [&]() { return src_pool_at(globalContext(), srcIdx); };

    // 1. decide fastcase
    bool fastcase = isColonFastcase(lhs, rhs);

    // 2. in fastcase, run type error effects
    if (fastcase) {
        int lhsLen = Rf_length(lhs);
        int rhsLen = Rf_length(rhs);
        if (lhsLen == 0 || rhsLen == 0)
            Rf_errorcall(getSrc(), "argument of length 0");
        if (lhsLen > 1)
            Rf_warningcall(getSrc(),
                           ngettext("numerical expression has %d element: "
                                    "only the first used",
                                    "numerical expression has %d elements: "
                                    "only the first used",
                                    (int)lhsLen),
                           (int)lhsLen);
        if (rhsLen > 1)
            Rf_warningcall(getSrc(),
                           ngettext("numerical expression has %d element: "
                                    "only the first used",
                                    "numerical expression has %d elements: "
                                    "only the first used",
                                    (int)rhsLen),
                           (int)rhsLen);
    }

    return fastcase;
}

SEXP colonCastLhs(SEXP lhs) {
    double lhsNum = Rf_asReal(lhs);

    if (std::isnan(lhsNum)) {
        Rf_error("NA/NaN argument");
    }
    SEXP result = doubleCanBeCastedToInteger(lhsNum)
                      ? Rf_ScalarInteger((int)lhsNum)
                      : Rf_ScalarReal(lhsNum);
    return result;
}

SEXP colonCastRhs(SEXP newLhs, SEXP rhs) {
    double newLhsNum = Rf_asReal(newLhs);
    double rhsNum = Rf_asReal(rhs);

    if (std::isnan(rhsNum)) {
        Rf_error("NA/NaN argument");
    }
    double newRhsNum = (newLhsNum <= rhsNum)
                           ? (newLhsNum + floor(rhsNum - newLhsNum) + 1)
                           : (newLhsNum - floor(newLhsNum - rhsNum) - 1);
    // nan RHS - should've went to slowcase
    assert(TYPEOF(newLhs) != INTSXP ||
           (newRhsNum >= INT_MIN && newRhsNum <= INT_MAX));
    SEXP result = (TYPEOF(newLhs) == INTSXP) ? Rf_ScalarInteger((int)newRhsNum)
                                             : Rf_ScalarReal(newRhsNum);
    return result;
}

SEXP evalRirCode(Code* c, InterpreterInstance* ctx, SEXP env,
                 const CallContext* callCtxt, Opcode* initialPC,
                 R_bcstack_t* localsBase, BindingCache* cache) {
    assert(env != symbol::delayedEnv || (callCtxt != nullptr));

    checkUserInterrupt();
    if (!initialPC && c->nativeCode) {
        return c->nativeCode(c, callCtxt ? (void*)callCtxt->stackArgs : nullptr,
                             env, callCtxt ? callCtxt->callee : nullptr);
    }

#ifdef THREADED_CODE
    static void* opAddr[static_cast<uint8_t>(Opcode::num_of)] = {
#define DEF_INSTR(name, ...) (__extension__ && op_##name),
#include "ir/insns.h"
#undef DEF_INSTR
    };
#endif

    assert(c->info.magic == CODE_MAGIC);
    bool existingLocals = localsBase;

    BindingCache* bindingCache;
    if (cache) {
        bindingCache = cache;
    } else {
        bindingCache = (BindingCache*)(alloca(sizeof(BindingCache) +
                                              sizeof(BindingCacheEntry) *
                                                  c->bindingCacheSize));
        bindingCache->length = c->bindingCacheSize;
        // Optimized functions explicitly manage the cache
        if (env != symbol::delayedEnv)
            clearCache(bindingCache);

#ifdef ENABLE_EVENT_COUNTERS
        if (ENABLE_EVENT_COUNTERS && env != symbol::delayedEnv)
            EventCounters::instance().count(EnvAllocated);
#endif
    }

    if (!existingLocals) {
        // Zero the region of the locals to avoid keeping stuff alive and to
        // zero all the type tags. Note: this trick does not work with the stack
        // in general, since there intermediate callees might set the type tags
        // to something else.
        memset(R_BCNodeStackTop, 0, sizeof(*R_BCNodeStackTop) * c->localsCount);
        localsBase = R_BCNodeStackTop;
    }
    Locals locals(localsBase, c->localsCount, existingLocals);

    // make sure there is enough room on the stack
    // there is some slack of 5 to make sure the call instruction can store
    // some intermediate values on the stack
    ostack_ensureSize(ctx, c->stackLength + 5);

    Opcode* pc;

    if (initialPC) {
        pc = initialPC;
    } else {
        R_Visible = TRUE;
        pc = c->code();
    }
    SEXP res;

    auto changeEnv = [&](SEXP e) {
        assert((TYPEOF(e) == ENVSXP || LazyEnvironment::check(e)) &&
               "Expected an environment");
        if (auto le = LazyEnvironment::check(e)) {
            if (le->materialized())
                e = le->materialized();
        }

        if (e != env)
            env = e;
    };

    // This is used in loads for recording if the loaded value was a promise
    // and if it was forced. Looks at the next instruction, if it's a force,
    // marks how this load behaved.
    auto recordForceBehavior = [&](SEXP s) {
        // Bail if this load not recorded or we are in already optimized code
        if (*pc != Opcode::record_type_)
            return;

        ObservedValues::StateBeforeLastForce state =
            ObservedValues::StateBeforeLastForce::unknown;
        if (TYPEOF(s) != PROMSXP)
            state = ObservedValues::StateBeforeLastForce::value;
        else if (PRVALUE(s) != R_UnboundValue)
            state = ObservedValues::StateBeforeLastForce::evaluatedPromise;
        else
            state = ObservedValues::StateBeforeLastForce::promise;

        ObservedValues* feedback = (ObservedValues*)(pc + 1);
        if (feedback->stateBeforeLastForce < state)
            feedback->stateBeforeLastForce = state;
    };

    // main loop
    BEGIN_MACHINE {

        INSTRUCTION(invalid_) assert(false && "wrong or unimplemented opcode");

        INSTRUCTION(nop_) NEXT();

        INSTRUCTION(push_context_) {
            SEXP ast = ostack_at(ctx, 1);
            SEXP op = ostack_at(ctx, 0);
            assert(LazyEnvironment::check(env) || TYPEOF(env) == ENVSXP);
            assert(TYPEOF(op) == CLOSXP);
            ostack_popn(ctx, 2);
            int offset = readJumpOffset();
            advanceJump();
            auto curPc = pc;

            // After returning from the inlined context we need to skip all the
            // instructions inside the context. Otherwise we would execute them
            // twice. Effectively this updates our pc to match the one the
            // pop_context_ had in the inlined context.
            pc += offset;
            SLOWASSERT(*pc == Opcode::pop_context_);
            advanceOpcode();
            auto stackOffset = readSignedImmediate();
            advanceImmediate();

            // Recursively call myself through a inlineContextTrampoline. The
            // trampoline creates an RCNTXT, and then continues executing the
            // same code.
            res =
                inlineContextTrampoline(c, callCtxt, ast, env, op, ctx, curPc,
                                        localsBase, bindingCache, stackOffset);

            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(pop_context_) {
            advanceImmediate();
            return ostack_pop(ctx);
        }

        INSTRUCTION(mk_dotlist_) {
            size_t n = readImmediate();
            advanceImmediate();
            SEXP res = R_NilValue;
            auto names = (Immediate*)pc;
            advanceImmediateN(n);
            assert(n > 0);
            for (long i = n - 1; i >= 0; --i) {
                SEXP val = ostack_pop(ctx);
                INCREMENT_NAMED(val);
                SEXP name = cp_pool_at(ctx, names[i]);
                res = CONS_NR(val, res);
                SET_TAG(res, name);
            }

            SET_TYPEOF(res, DOTSXP);
            ostack_push(ctx, res);

            NEXT();
        }

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
            bool hasDots = false;
            for (long i = n - 1; i >= 0; --i) {
                SEXP val = ostack_pop(ctx);
                INCREMENT_NAMED(val);
                SEXP name = cp_pool_at(ctx, names[i]);
                bool isMissing = val == R_MissingArg;
                if (TYPEOF(name) == LISTSXP) {
                    isMissing = true;
                    name = CAR(name);
                }
                if (name == R_DotsSymbol)
                    hasDots = true;
                arglist = CONS_NR(val, arglist);
                SET_TAG(arglist, name);
                hasMissing = hasMissing || isMissing;
                SET_MISSING(arglist, isMissing ? 2 : 0);
            }
            res = Rf_NewEnvironment(R_NilValue, arglist, parent);

            if (contextPos > 0) {
                if (auto cptr = getFunctionContext(contextPos - 1)) {
                    cptr->cloenv = res;
                    // TODO proper handling of promargs if we have ... args
                    if (!hasDots && cptr->promargs == symbol::delayedArglist) {
                        auto promargs = arglist;
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
            ostack_push(ctx, res);
            UNPROTECT(1);

#ifdef ENABLE_EVENT_COUNTERS
            if (ENABLE_EVENT_COUNTERS)
                EventCounters::instance().count(EnvAllocated);
#endif

            NEXT();
        }

        INSTRUCTION(clear_binding_cache_) {
            size_t start = readImmediate();
            advanceImmediate();
            size_t len = readImmediate();
            advanceImmediate();
            if (len) {
                SLOWASSERT(start + len <= bindingCache->length);
                memset(&bindingCache->entry[start], 0,
                       sizeof(BindingCacheEntry) * len);
            }
            NEXT();
        }

        INSTRUCTION(mk_stub_env_) {
            size_t n = readImmediate();
            advanceImmediate();
            int contextPos = readSignedImmediate();
            advanceImmediate();
            // Do we need to preserve parent and the arg vals?
            SEXP parent = ostack_pop(ctx);
            assert(TYPEOF(parent) == ENVSXP &&
                   "Non-environment used as environment parent.");
            auto names = (Immediate*)pc;
            advanceImmediateN(n);
            SEXP wrapper =
                LazyEnvironment::New(parent, n, names, ctx)->container();

            ostack_push(ctx, wrapper);
            if (contextPos > 0) {
                if (auto cptr = getFunctionContext(contextPos - 1))
                    cptr->cloenv = wrapper;
            }

#ifdef ENABLE_EVENT_COUNTERS
            if (ENABLE_EVENT_COUNTERS)
                EventCounters::instance().count(EnvStubAllocated);
#endif
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

        INSTRUCTION(materialize_env_) {
            auto lazyEnv = LazyEnvironment::check(env);
            if (lazyEnv && !lazyEnv->materialized())
                env = materialize(env);
            ostack_push(ctx, env);
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
            assert(!LazyEnvironment::check(env));
            R_varloc_t loc = R_findVarLocInFrame(env, cp_pool_at(ctx, id));
            bool isLocal = !R_VARLOC_IS_NULL(loc);
            SEXP res = nullptr;

            if (isLocal && CAR(loc.cell) != R_UnboundValue) {
                res = CAR(loc.cell);
            } else {
                SEXP sym = cp_pool_at(ctx, id);
                res = Rf_findVar(sym, ENCLOS(env));
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
            recordForceBehavior(res);
            if (TYPEOF(res) == PROMSXP)
                res = evaluatePromise(res, ctx);

            if (res != R_NilValue) {
                if (isLocal)
                    ENSURE_NAMED(res);
                else if (NAMED(res) < 2)
                    SET_NAMED(res, 2);
            }

            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(ldvar_for_update_cache_) {
            Immediate id = readImmediate();
            advanceImmediate();
            Immediate cacheIndex = readImmediate();
            advanceImmediate();
            assert(!LazyEnvironment::check(env));
            SEXP loc = getCellFromCache(env, id, cacheIndex, ctx, bindingCache);
            bool isLocal = loc;
            SEXP res = nullptr;

            if (isLocal && CAR(loc) != R_UnboundValue) {
                res = CAR(loc);
            } else {
                SEXP sym = cp_pool_at(ctx, id);
                res = Rf_findVar(sym, ENCLOS(env));
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
            recordForceBehavior(res);
            if (TYPEOF(res) == PROMSXP)
                res = evaluatePromise(res, ctx);

            if (res != R_NilValue) {
                if (isLocal)
                    ENSURE_NAMED(res);
                else if (NAMED(res) < 2)
                    SET_NAMED(res, 2);
            }

            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(ldvar_noforce_stubbed_) {
            unsigned pos = readImmediate();
            advanceImmediate();

            auto le = LazyEnvironment::check(env);
            assert(le);
            assert(!le->materialized());

            auto res = le->getArg(pos);
            if (res == R_UnboundValue) {
                auto sym = Pool::get(le->names[pos]);
                res = Rf_findVar(sym, le->getParent());
            }

            if (res == R_UnboundValue) {
                Rf_error("object \"%s\" not found",
                         CHAR(PRINTNAME(Pool::get(le->names[pos]))));
            } else if (res == R_MissingArg) {
                Rf_error("argument \"%s\" is missing, with no default",
                         CHAR(PRINTNAME(Pool::get(le->names[pos]))));
            }

            if (res != R_NilValue)
                ENSURE_NAMED(res);

            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(ldvar_) {
            SEXP sym = readConst(ctx, readImmediate());
            advanceImmediate();
            assert(!LazyEnvironment::check(env));
            res = Rf_findVar(sym, env);
            R_Visible = TRUE;

            recordForceBehavior(res);

            if (res == R_UnboundValue) {
                Rf_error("object \"%s\" not found", CHAR(PRINTNAME(sym)));
            } else if (res == R_MissingArg) {
                Rf_error("argument \"%s\" is missing, with no default",
                         CHAR(PRINTNAME(sym)));
            } else if (TYPEOF(res) == PROMSXP) {
                // if promise, evaluate & return
                res = evaluatePromise(res, ctx);
            }

            if (res != R_NilValue)
                ENSURE_NAMED(res);

            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(ldvar_cached_) {
            Immediate id = readImmediate();
            advanceImmediate();
            Immediate cacheIndex = readImmediate();
            advanceImmediate();
            assert(!LazyEnvironment::check(env));
            res = cachedGetVar(env, id, cacheIndex, ctx, bindingCache);
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
            recordForceBehavior(res);
            if (TYPEOF(res) == PROMSXP)
                res = evaluatePromise(res, ctx);

            if (res != R_NilValue)
                ENSURE_NAMED(res);

            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(ldvar_noforce_) {
            SEXP sym = readConst(ctx, readImmediate());
            advanceImmediate();
            assert(!LazyEnvironment::check(env));
            res = Rf_findVar(sym, env);

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

        INSTRUCTION(ldvar_noforce_cached_) {
            Immediate id = readImmediate();
            advanceImmediate();
            Immediate cacheIndex = readImmediate();
            advanceImmediate();
            assert(!LazyEnvironment::check(env));
            res = cachedGetVar(env, id, cacheIndex, ctx, bindingCache);

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
            assert(!LazyEnvironment::check(env));
            res = Rf_findVar(sym, ENCLOS(env));

            if (res == R_UnboundValue) {
                Rf_error("object \"%s\" not found", CHAR(PRINTNAME(sym)));
            } else if (res == R_MissingArg) {
                Rf_error("argument \"%s\" is missing, with no default",
                         CHAR(PRINTNAME(sym)));
            }

            // if promise, evaluate & return
            recordForceBehavior(res);
            if (TYPEOF(res) == PROMSXP)
                res = evaluatePromise(res, ctx);

            if (res != R_NilValue)
                ENSURE_NAMED(res);

            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(ldvar_noforce_super_) {
            SEXP sym = readConst(ctx, readImmediate());
            advanceImmediate();
            assert(!LazyEnvironment::check(env));
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
            recordForceBehavior(res);
            if (TYPEOF(res) == PROMSXP)
                res = evaluatePromise(res, ctx);

            if (res != R_NilValue)
                ENSURE_NAMED(res);

            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(ldarg_) {
            Immediate idx = readImmediate();
            advanceImmediate();
            assert(callCtxt);

            ostack_push(ctx, callCtxt->stackArg(idx));
            NEXT();
        }

        INSTRUCTION(ldloc_) {
            Immediate offset = readImmediate();
            advanceImmediate();
            res = locals.load(offset);
            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(starg_stubbed_) {
            unsigned pos = readImmediate();
            advanceImmediate();
            SEXP val = ostack_top(ctx);

            auto le = LazyEnvironment::check(env);
            assert(le);
            assert(!le->materialized());
            if (le->getArg(pos) != val) {
                INCREMENT_NAMED(val);
                le->setArg(pos, val, false);
            }
            ostack_pop(ctx);
            NEXT();
        }

        INSTRUCTION(stvar_stubbed_) {
            unsigned pos = readImmediate();
            advanceImmediate();
            SEXP val = ostack_top(ctx);

            auto le = LazyEnvironment::check(env);
            assert(le);
            assert(!le->materialized());
            if (le->getArg(pos) != val) {
                INCREMENT_NAMED(val);
            } else {
                ENSURE_NAMED(val);
            }
            if (le->getArg(pos) != val) {
                le->setArg(pos, val, true);
            } else if (le->missing[pos]) {
                le->missing[pos] = false;
            }
            ostack_pop(ctx);
            NEXT();
        }

        INSTRUCTION(stvar_) {
            SEXP sym = readConst(ctx, readImmediate());
            advanceImmediate();
            SLOWASSERT(TYPEOF(sym) == SYMSXP);
            SEXP val = ostack_top(ctx);

            assert(!LazyEnvironment::check(env));

            rirDefineVarWrapper(sym, val, env);
            ostack_pop(ctx);
            NEXT();
        }

        INSTRUCTION(stvar_cached_) {
            Immediate id = readImmediate();
            advanceImmediate();
            Immediate cacheIndex = readImmediate();
            advanceImmediate();
            SEXP val = ostack_pop(ctx);

            assert(!LazyEnvironment::check(env));

            cachedSetVar(val, env, id, cacheIndex, ctx, bindingCache);
            NEXT();
        }

        INSTRUCTION(starg_) {
            Immediate id = readImmediate();
            advanceImmediate();
            SEXP val = ostack_top(ctx);

            assert(!LazyEnvironment::check(env));

            SEXP sym = cp_pool_at(ctx, id);
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
                    ostack_pop(ctx);
                    NEXT();
                }
            }

            rirDefineVarWrapper(sym, val, env);
            ostack_pop(ctx);

            NEXT();
        }

        INSTRUCTION(starg_cached_) {
            Immediate id = readImmediate();
            advanceImmediate();
            Immediate cacheIndex = readImmediate();
            advanceImmediate();
            SEXP val = ostack_pop(ctx);

            assert(!LazyEnvironment::check(env));
            cachedSetVar(val, env, id, cacheIndex, ctx, bindingCache, true);

            NEXT();
        }

        INSTRUCTION(stvar_super_) {
            SEXP sym = readConst(ctx, readImmediate());
            advanceImmediate();
            SLOWASSERT(TYPEOF(sym) == SYMSXP);
            SEXP val = ostack_pop(ctx);
            auto le = LazyEnvironment::check(env);
            assert(!le || !le->materialized());
            SEXP superEnv;
            if (le)
                superEnv = le->getParent();
            else
                superEnv = ENCLOS(env);
            rirSetVarWrapper(sym, val, superEnv);
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

        INSTRUCTION(record_call_) {
            ObservedCallees* feedback = (ObservedCallees*)pc;
            SEXP callee = ostack_top(ctx);
            feedback->record(c, callee);
            pc += sizeof(ObservedCallees);
            NEXT();
        }

        INSTRUCTION(record_deopt_) {
            SEXP val = ostack_pop(ctx);
            DeoptReason* res = (DeoptReason*)pc;
            pc += sizeof(DeoptReason);
            recordDeoptReason(val, *res);
            NEXT();
        }

        INSTRUCTION(record_test_) {
            ObservedTest* feedback = (ObservedTest*)pc;
            SEXP t = ostack_top(ctx);
            feedback->record(t);
            pc += sizeof(ObservedTest);
            NEXT();
        }

        INSTRUCTION(record_type_) {
            ObservedValues* feedback = (ObservedValues*)pc;
            SEXP t = ostack_top(ctx);
            feedback->record(t);
            pc += sizeof(ObservedValues);
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
            Context given(pc);
            pc += sizeof(Context);

            CallContext call(c, ostack_at(ctx, n), n, ast,
                             ostack_cell_at(ctx, (long)n - 1), env, given, ctx);
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
            Context given(pc);
            pc += sizeof(Context);
            auto names = (Immediate*)pc;
            advanceImmediateN(n);
            CallContext call(c, ostack_at(ctx, n), n, ast,
                             ostack_cell_at(ctx, (long)n - 1), names, env,
                             given, ctx);
            res = doCall(call, ctx);
            ostack_popn(ctx, call.passedArgs + 1);
            ostack_push(ctx, res);

            SLOWASSERT(ttt == R_PPStackTop);
            SLOWASSERT(lll - call.suppliedArgs == (unsigned)ostack_length(ctx));
            NEXT();
        }

        INSTRUCTION(call_dots_) {
#ifdef ENABLE_SLOWASSERT
            int ttt = R_PPStackTop;
#endif

            // Stack contains [callee, arg1, ..., argn]
            Immediate n = readImmediate();
            advanceImmediate();
            size_t ast = readImmediate();
            advanceImmediate();
            Context given(pc);
            pc += sizeof(Context);
            auto names_ = (Immediate*)pc;
            advanceImmediateN(n);

            SEXP callee = ostack_at(ctx, n);
            Immediate* names = names_;
            int pushed = 0;
            if (TYPEOF(callee) != SPECIALSXP) {
                n = expandDotDotDotCallArgs(
                    ctx, n, names_, env,
                    given.includes(Assumption::StaticallyArgmatched));
                auto namesStore = ostack_at(ctx, n);
                if (namesStore == R_NilValue)
                    names = nullptr;
                else
                    names = (Immediate*)DATAPTR(namesStore);
                pushed = 1;
            }
            CallContext call(c, callee, n, ast,
                             ostack_cell_at(ctx, (long)n - 1), names, env,
                             given, ctx);
            res = doCall(call, ctx);
            ostack_popn(ctx, call.passedArgs + 1 + pushed);
            ostack_push(ctx, res);

            SLOWASSERT(ttt == R_PPStackTop);
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
            CallContext call(c, callee, n, ast,
                             ostack_cell_at(ctx, (long)n - 1), env, Context(),
                             ctx);
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
            Context staticContext(pc);
            pc += sizeof(Context);
            SEXP callee = cp_pool_at(ctx, readImmediate());
            advanceImmediate();
            SEXP version = cp_pool_at(ctx, readImmediate());
            CallContext call(c, callee, n, ast,
                             ostack_cell_at(ctx, (long)n - 1), env,
                             staticContext, ctx);
            auto fun = Function::unpack(version);
            inferCurrentContext(call, fun->signature().formalNargs(), ctx);
            auto flags = fun->flags;
            bool dispatchFail =
                flags.contains(Function::Dead) || !matches(call, fun);
            auto dt = DispatchTable::unpack(BODY(callee));
            auto given = call.givenContext;
            fun->clearDisabledAssumptions(given);
            if (!isDeoptimizing() && !dispatchFail &&
                RecompileHeuristic(dt, fun, 3) &&
                RecompileCondition(dt, fun, given)) {
                DoRecompile(fun, call.ast, call.callee, given, ctx);
                dispatchFail = true;
            }

            if (dispatchFail) {
                fun = dispatch(call, dt);
                // Patch inline cache
                (*(Immediate*)pc) = Pool::insert(fun->container());
            }
            advanceImmediate();

            SLOWASSERT(!fun->flags.contains(Function::Dead));
            SLOWASSERT(dt->dispatch(fun->context()) == fun);
            if (fun->signature().envCreation ==
                FunctionSignature::Environment::CallerProvided) {
                res = rirCall(call, ctx);
            } else {
                ArgsLazyData lazyArgs(call.suppliedArgs, call.stackArgs,
                                      call.names, ctx);
                // Currently we cannot recreate the original arglist if we
                // statically reordered arguments. TODO this needs to be fixed
                // by remembering the original order.
                if (given.includes(Assumption::StaticallyArgmatched))
                    lazyArgs.content.args = nullptr;
                supplyMissingArgs(call, fun);
                fun->registerInvocation();
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
            Rf_setAttrib(res, symbol::srcref, srcref);
            ostack_popn(ctx, 3);
            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(check_closure_) {
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

        INSTRUCTION(mk_eager_promise_) {
            Immediate id = readImmediate();
            advanceImmediate();
            SEXP prom = Rf_mkPROMISE(c->getPromise(id)->container(), env);
            SEXP val = ostack_pop(ctx);
            assert(TYPEOF(val) != PROMSXP);
            ENSURE_NAMEDMAX(val);
            SET_PRVALUE(prom, val);
            ostack_push(ctx, prom);
            NEXT();
        }

        INSTRUCTION(mk_promise_) {
            Immediate id = readImmediate();
            advanceImmediate();
            SEXP prom = Rf_mkPROMISE(c->getPromise(id)->container(), env);
            ostack_push(ctx, prom);
            NEXT();
        }

        INSTRUCTION(update_promise_) {
            auto val = ostack_pop(ctx);
            auto prom = ostack_pop(ctx);
            SLOWASSERT(TYPEOF(prom) == PROMSXP);
            SLOWASSERT(TYPEOF(val) != PROMSXP);
            ENSURE_NAMEDMAX(val);
            SET_PRVALUE(prom, val);
            NEXT();
        }

        INSTRUCTION(force_) {
            if (TYPEOF(ostack_top(ctx)) == PROMSXP) {
                SEXP val = ostack_pop(ctx);
                // If the promise is already evaluated then push the value
                // inside the promise onto the stack, otherwise push the value
                // from forcing the promise
                ostack_push(ctx, evaluatePromise(val, ctx));
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
            SEXP val = pos->u.sxpval;
            while (i--) {
                pos->u.sxpval = (pos - 1)->u.sxpval;
                pos--;
            }
            pos->u.sxpval = val;
            NEXT();
        }

        INSTRUCTION(pick_) {
            Immediate i = readImmediate();
            advanceImmediate();
            R_bcstack_t* pos = ostack_cell_at(ctx, i);
            SEXP val = pos->u.sxpval;
            while (i--) {
                pos->u.sxpval = (pos + 1)->u.sxpval;
                pos++;
            }
            pos->u.sxpval = val;
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
            DO_BINOP(+, Binop::PLUSOP);
            NEXT();
        }

        INSTRUCTION(uplus_) {
            SEXP val = ostack_at(ctx, 0);
            DO_UNOP(+, Unop::PLUSOP);
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

        INSTRUCTION(sub_) {
            SEXP lhs = ostack_at(ctx, 1);
            SEXP rhs = ostack_at(ctx, 0);
            DO_BINOP(-, Binop::MINUSOP);
            NEXT();
        }

        INSTRUCTION(uminus_) {
            SEXP val = ostack_at(ctx, 0);
            DO_UNOP(-, Unop::MINUSOP);
            NEXT();
        }

        INSTRUCTION(mul_) {
            SEXP lhs = ostack_at(ctx, 1);
            SEXP rhs = ostack_at(ctx, 0);
            DO_BINOP(*, Binop::TIMESOP);
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
            // TODO
            // 1. currently aslogical_ is used for &&, || only, and this checking
            //    is to mimic the behavior of builtin &&, ||. Technically asLogical
            //    is less strict than this.
            // 2. the error message also doesn't suggest which argument is wrong, or
            //    which boolean operation it was. To achieve the exact behavior, one
            //    could potentially compile this check in `ir/Compiler.cpp`
            if (!Rf_isNumber(val)) {
                SEXP call = getSrcAt(c, pc - 1, ctx);
                Rf_errorcall(call, "argument has the wrong type for && or ||");
            }
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

        INSTRUCTION(colon_input_effects_) {
            SEXP lhs = ostack_at(ctx, 1);
            SEXP rhs = ostack_at(ctx, 0);

            bool fastcase = colonInputEffects(lhs, rhs, 0);
            ostack_push(ctx, fastcase ? R_TrueValue : R_FalseValue);

            NEXT();
        }

        INSTRUCTION(colon_cast_lhs_) {
            SEXP lhs = ostack_pop(ctx);
            SEXP newLhs = colonCastLhs(lhs);
            ostack_push(ctx, newLhs);
            NEXT();
        }

        INSTRUCTION(colon_cast_rhs_) {
            SEXP rhs = ostack_pop(ctx);
            SEXP newLhs = ostack_top(ctx);
            SEXP newRhs = colonCastRhs(newLhs, rhs);
            ostack_push(ctx, newRhs);
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
            Immediate type = readImmediate();
            advanceImmediate();
            bool res;
            switch (type) {
            case NILSXP:
            case LGLSXP:
            case REALSXP:
                res = TYPEOF(val) == type;
                break;

            case VECSXP:
                res = TYPEOF(val) == VECSXP || TYPEOF(val) == LISTSXP;
                break;

            case LISTSXP:
                res = TYPEOF(val) == LISTSXP || TYPEOF(val) == NILSXP;
                break;
            default:
                assert(false);
                res = false;
                break;
            }
            ostack_push(ctx, res ? R_TrueValue : R_FalseValue);
            NEXT();
        }

        INSTRUCTION(istype_) {
            SEXP val = ostack_pop(ctx);
            Immediate i = readImmediate();
            advanceImmediate();
            bool res = false;
            switch (static_cast<TypeChecks>(i)) {

            case TypeChecks::LogicalNonObject:
                res = TYPEOF(val) == LGLSXP && !isObject(val);
                break;
            case TypeChecks::LogicalNonObjectWrapped:
                if (TYPEOF(val) == PROMSXP)
                    val = PRVALUE(val);
                res = TYPEOF(val) == LGLSXP && !isObject(val);
                break;

            case TypeChecks::LogicalSimpleScalar:
                res = IS_SIMPLE_SCALAR(val, LGLSXP);
                break;
            case TypeChecks::LogicalSimpleScalarWrapped:
                if (TYPEOF(val) == PROMSXP)
                    val = PRVALUE(val);
                res = IS_SIMPLE_SCALAR(val, LGLSXP);
                break;

            case TypeChecks::IntegerNonObject:
                res = TYPEOF(val) == INTSXP && !isObject(val);
                break;
            case TypeChecks::IntegerNonObjectWrapped:
                if (TYPEOF(val) == PROMSXP)
                    val = PRVALUE(val);
                res = TYPEOF(val) == INTSXP && !isObject(val);
                break;

            case TypeChecks::IntegerSimpleScalar:
                res = IS_SIMPLE_SCALAR(val, INTSXP);
                break;
            case TypeChecks::IntegerSimpleScalarWrapped:
                if (TYPEOF(val) == PROMSXP)
                    val = PRVALUE(val);
                res = IS_SIMPLE_SCALAR(val, INTSXP);
                break;

            case TypeChecks::RealNonObject:
                res = TYPEOF(val) == REALSXP && !isObject(val);
                break;
            case TypeChecks::RealNonObjectWrapped:
                if (TYPEOF(val) == PROMSXP)
                    val = PRVALUE(val);
                res = TYPEOF(val) == REALSXP && !isObject(val);
                break;

            case TypeChecks::RealSimpleScalar:
                res = IS_SIMPLE_SCALAR(val, REALSXP);
                break;
            case TypeChecks::RealSimpleScalarWrapped:
                if (TYPEOF(val) == PROMSXP)
                    val = PRVALUE(val);
                res = IS_SIMPLE_SCALAR(val, REALSXP);
                break;

            case TypeChecks::NotObject:
                res = !isObject(val);
                break;
            case TypeChecks::NotObjectWrapped:
                if (TYPEOF(val) == PROMSXP)
                    val = PRVALUE(val);
                res = !isObject(val);
                break;

            case TypeChecks::NoAttribsExceptDim:
                res = fastVeceltOk(val);
                break;
            case TypeChecks::NoAttribsExceptDimWrapped:
                if (TYPEOF(val) == PROMSXP)
                    val = PRVALUE(val);
                res = fastVeceltOk(val);
                break;

            case TypeChecks::_START_:
            case TypeChecks::_END_:
                assert(false);
            }
            ostack_push(ctx, res ? R_TrueValue : R_FalseValue);

            NEXT();
        }

        INSTRUCTION(isstubenv_) {
            SEXP val = ostack_pop(ctx);
            auto le = LazyEnvironment::check(val);
            auto isStub = le && !le->materialized();
            ostack_push(ctx, isStub ? R_TrueValue : R_FalseValue);
            NEXT();
        }

        INSTRUCTION(missing_) {
            SEXP sym = readConst(ctx, readImmediate());
            advanceImmediate();
            SLOWASSERT(TYPEOF(sym) == SYMSXP);
            SLOWASSERT(!DDVAL(sym));
            assert(env);
            ostack_push(ctx, isMissing(sym, env, c, pc) ? R_TrueValue
                                                        : R_FalseValue);
            NEXT();
        }

        INSTRUCTION(check_missing_) {
            SEXP val = ostack_top(ctx);
            if (val == R_MissingArg)
                Rf_error("argument is missing, with no default");
            NEXT();
        }

        INSTRUCTION(brtrue_) {
            JumpOffset offset = readJumpOffset();
            advanceJump();
            if (ostack_pop(ctx) == R_TrueValue) {
                checkUserInterrupt();
                pc += offset;
            }
            PC_BOUNDSCHECK(pc, c);
            NEXT();
        }

        INSTRUCTION(brfalse_) {
            JumpOffset offset = readJumpOffset();
            advanceJump();
            if (ostack_pop(ctx) == R_FalseValue) {
                checkUserInterrupt();
                pc += offset;
            }
            PC_BOUNDSCHECK(pc, c);
            NEXT();
        }

        INSTRUCTION(br_) {
            JumpOffset offset = readJumpOffset();
            advanceJump();
            checkUserInterrupt();
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

        INSTRUCTION(extract1_3_) {
            SEXP val = ostack_at(ctx, 3);
            SEXP idx = ostack_at(ctx, 2);
            SEXP idx2 = ostack_at(ctx, 1);
            SEXP idx3 = ostack_at(ctx, 0);

            SEXP args = CONS_NR(
                val, CONS_NR(idx, CONS_NR(idx2, CONS_NR(idx3, R_NilValue))));
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

            ostack_popn(ctx, 5);

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
                ENSURE_NAMED(res);
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
                vec = Rf_shallow_duplicate(vec);
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
                mtx = Rf_shallow_duplicate(mtx);
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

        INSTRUCTION(subassign1_3_) {
            SEXP idx3 = ostack_at(ctx, 0);
            SEXP idx2 = ostack_at(ctx, 1);
            SEXP idx1 = ostack_at(ctx, 2);
            SEXP mtx = ostack_at(ctx, 3);
            SEXP val = ostack_at(ctx, 4);

            // Destructively modifies TOS, even if the refcount is 1. This is
            // intended, to avoid copying. Care need to be taken if `vec` is
            // used multiple times as a temporary.
            if (MAYBE_SHARED(mtx)) {
                mtx = Rf_shallow_duplicate(mtx);
                ostack_set(ctx, 2, mtx);
            }

            SEXP args = CONS_NR(
                mtx, CONS_NR(idx1,
                             CONS_NR(idx2,
                                     CONS_NR(idx3, CONS_NR(val, R_NilValue)))));
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
            ostack_popn(ctx, 5);
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
                        case REALSXP: {
                            double realRes = 0;
                            if (valT == REALSXP) {
                                realRes = *REAL(val);
                            } else {
                                if (*INTEGER(val) == NA_INTEGER) {
                                    realRes = R_NaN;
                                } else {
                                    realRes = (double)*INTEGER(val);
                                }
                            }
                            REAL(vec)[idx_] = realRes;
                            break;
                        }
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
                vec = Rf_shallow_duplicate(vec);
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

                    if (idx1_ >= 0 && idx1_ < Rf_nrows(mtx) && idx2_ >= 0 &&
                        idx2_ < Rf_ncols(mtx)) {
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
                mtx = Rf_shallow_duplicate(mtx);
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
            if (res != Rf_findFun(sym, env))
                Rf_error("Invalid Callee");
            NEXT();
        }

        INSTRUCTION(deopt_) {
            SEXP r = readConst(ctx, readImmediate());
            advanceImmediate();
            assert(TYPEOF(r) == RAWSXP);
            assert(XLENGTH(r) >= (int)sizeof(DeoptMetadata));
            auto m = (DeoptMetadata*)DATAPTR(r);

#if 0
            std::cout << "\n## ====================\n";
            size_t pos = 0;
            for (size_t i = 0; i < m->numFrames; ++i) {
                std::cout << "## Frame " << i << (m->frames[i].inPromise ? " prom" : "") << ":\n";
                std::cout << "Code " << m->frames[i].code << "\n";
                std::cout << "== env (" << pos << ")\n";
                Rf_PrintValue(ostack_at(ctx, pos++));
                for( size_t j = 0; j < m->frames[i].stackSize; ++j) {
                    std::cout << "== stack (" << pos << ") " << j << "\n";
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
                // TODO: report deoptimization reason.
                // For example if we deopt because of stubenv was materialized
                // we should prevent pir from stubbing the env in the future.
                dt->remove(c);
            }
            assert(m->numFrames >= 1);
            size_t stackHeight = 0;
            for (size_t i = 0; i < m->numFrames; ++i)
                stackHeight += m->frames[i].stackSize + 1;
            m->frames[m->numFrames - 1].code->registerDeopt();
            c->registerDeopt();
            deoptFramesWithContext(ctx, callCtxt, m, R_NilValue,
                                   m->numFrames - 1, stackHeight,
                                   (RCNTXT*)R_GlobalContext);
            assert(false);
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
            SEXP names = ostack_pop(ctx);
            Rf_setAttrib(ostack_top(ctx), R_NamesSymbol, names);
            NEXT();
        }

        INSTRUCTION(xlength_) {
            SEXP len = Rf_allocVector(INTSXP, 1);
            INTEGER(len)[0] = Rf_xlength(ostack_pop(ctx));
            ostack_push(ctx, len);
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
                seq = Rf_shallow_duplicate(seq);
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
            loopTrampoline(c, ctx, env, callCtxt, pc, localsBase, bindingCache);
            pc += offset;
            checkUserInterrupt();
            assert(*pc == Opcode::endloop_);
            advanceOpcode();
            NEXT();
        }

        INSTRUCTION(endloop_) { return loopTrampolineMarker; }

        INSTRUCTION(return_) {
            res = ostack_pop(ctx);
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

        INSTRUCTION(assert_type_) {
            assert(pir::Parameter::RIR_CHECK_PIR_TYPES);
            SEXP val = ostack_top(ctx);
            pir::PirType typ(pc);
            pc += sizeof(pir::PirType);
            int instrIdx = readSignedImmediate();
            const char* instr = NULL;
            if (instrIdx == -1) {
                instr = "not generated, set RIR_CHECK_PIR_TYPES=2";
            } else {
                instr = CHAR(Rf_asChar(Pool::get((unsigned)instrIdx)));
            }
            advanceImmediate();
            if (!typ.isInstance(val)) {
                std::cerr << "type assert failed in:\n" << instr << "\n";
                std::cerr << "got " << pir::PirType(val) << " but expexted a "
                          << typ << ":\n";
                Rf_PrintValue(val);
                std::cout << (PRVALUE(val) == R_UnboundValue) << " / "
                          << (PRVALUE(val) == R_MissingArg) << "\n";
                assert(false);
            }
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
    return evalRirCode(c, ctx, env, callCtxt, nullptr, nullptr, nullptr);
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
                     names.empty() ? nullptr : names.data(), rho, Context(),
                     ctx);
    call.arglist = arglist;
    call.safeForceArgs();

    auto res = rirCall(call, ctx);
    ostack_popn(ctx, call.passedArgs);
    return res;
}

SEXP rirEval(SEXP what, SEXP env) {
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
