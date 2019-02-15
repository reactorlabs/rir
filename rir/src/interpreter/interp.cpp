#include "interp.h"
#include "R/Funtab.h"
#include "R/RList.h"
#include "R/Symbols.h"
#include "R/r.h"
#include "compiler/translations/rir_2_pir/rir_2_pir_compiler.h"
#include "interp_context.h"
#include "ir/Deoptimization.h"
#include "ir/RuntimeFeedback_inl.h"
#include "runtime.h"
#include "utils/Pool.h"

#include <assert.h>
#include <deque>

#define NOT_IMPLEMENTED assert(false)

#undef eval

extern "C" {
extern SEXP Rf_NewEnvironment(SEXP, SEXP, SEXP);
extern Rboolean R_Visible;
}

// #define UNSOUND_OPTS

// #define DEBUG_DISPATCH

// helpers

using namespace rir;

struct CallContext {
    CallContext(Code* c, SEXP callee, size_t nargs, SEXP ast,
                R_bcstack_t* stackArgs, Immediate* implicitArgs,
                Immediate* names, SEXP callerEnv,
                const Assumptions& givenAssumptions, Context* ctx)
        : caller(c), suppliedArgs(nargs), passedArgs(nargs),
          stackArgs(stackArgs), implicitArgs(implicitArgs), names(names),
          callerEnv(callerEnv), ast(ast), callee(callee),
          givenAssumptions(givenAssumptions) {
        assert(callee &&
               (TYPEOF(callee) == CLOSXP || TYPEOF(callee) == SPECIALSXP ||
                TYPEOF(callee) == BUILTINSXP));
    }

    CallContext(Code* c, SEXP callee, size_t nargs, Immediate ast,
                Immediate* implicitArgs, Immediate* names, SEXP callerEnv,
                const Assumptions& givenAssumptions, Context* ctx)
        : CallContext(c, callee, nargs, cp_pool_at(ctx, ast), nullptr,
                      implicitArgs, names, callerEnv, givenAssumptions, ctx) {}

    CallContext(Code* c, SEXP callee, size_t nargs, Immediate ast,
                R_bcstack_t* stackArgs, Immediate* names, SEXP callerEnv,
                const Assumptions& givenAssumptions, Context* ctx)
        : CallContext(c, callee, nargs, cp_pool_at(ctx, ast), stackArgs,
                      nullptr, names, callerEnv, givenAssumptions, ctx) {}

    CallContext(Code* c, SEXP callee, size_t nargs, Immediate ast,
                Immediate* implicitArgs, SEXP callerEnv,
                const Assumptions& givenAssumptions, Context* ctx)
        : CallContext(c, callee, nargs, cp_pool_at(ctx, ast), nullptr,
                      implicitArgs, nullptr, callerEnv, givenAssumptions, ctx) {
    }

    CallContext(Code* c, SEXP callee, size_t nargs, Immediate ast,
                R_bcstack_t* stackArgs, SEXP callerEnv,
                const Assumptions& givenAssumptions, Context* ctx)
        : CallContext(c, callee, nargs, cp_pool_at(ctx, ast), stackArgs,
                      nullptr, nullptr, callerEnv, givenAssumptions, ctx) {}

    const Code* caller;
    const size_t suppliedArgs;
    size_t passedArgs;
    const R_bcstack_t* stackArgs;
    const Immediate* implicitArgs;
    const Immediate* names;
    const SEXP callerEnv;
    const SEXP ast;
    const SEXP callee;
    Assumptions givenAssumptions;
    SEXP arglist = nullptr;

    bool hasStackArgs() const { return stackArgs != nullptr; }
    bool hasEagerCallee() const { return TYPEOF(callee) == BUILTINSXP; }
    bool hasNames() const { return names; }

    Immediate implicitArgIdx(unsigned i) const {
        assert(implicitArgs && i < passedArgs);
        return implicitArgs[i];
    }

    bool missingArg(unsigned i) const {
        return implicitArgIdx(i) == MISSING_ARG_IDX;
    }

    Code* implicitArg(unsigned i) const {
        assert(caller);
        return caller->getPromise(implicitArgIdx(i));
    }

    R_bcstack_t stackArg(unsigned i) const {
        assert(stackArgs && i < passedArgs);
        return stackArgs[i];
    }

    SEXP name(unsigned i, Context* ctx) const {
        assert(hasNames() && i < suppliedArgs);
        return cp_pool_at(ctx, names[i]);
    }
};

RIR_INLINE SEXP getSrcAt(Code* c, Opcode* pc, Context* ctx) {
    unsigned sidx = c->getSrcIdxAt(pc, true);
    if (sidx == 0)
        return src_pool_at(ctx, c->src);
    return src_pool_at(ctx, sidx);
}

RIR_INLINE SEXP getSrcForCall(Code* c, Opcode* pc, Context* ctx) {
    unsigned sidx = c->getSrcIdxAt(pc, false);
    return src_pool_at(ctx, sidx);
}

#define PC_BOUNDSCHECK(pc, c)                                                  \
    SLOWASSERT((pc) >= (c)->code() && (pc) < (c)->endCode());

#ifdef THREADED_CODE
#define BEGIN_MACHINE NEXT();
#define INSTRUCTION(name)                                                      \
    op_##name: /* debug(c, pc, #name, ostack_length(ctx) - bp, ctx); */
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

void endClosureContext(RCNTXT* cntxt, SEXP result) {
    cntxt->returnValue = result;
    Rf_endcontext(cntxt);
}

RIR_INLINE SEXP createPromise(Code* code, SEXP env) {
    SEXP p = Rf_mkPROMISE(code->container(), env);
    return p;
}

RIR_INLINE SEXP promiseValue(SEXP promise, Context* ctx) {
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

static void jit(SEXP cls, SEXP name, Context* ctx) {
    assert(TYPEOF(cls) == CLOSXP);
    if (TYPEOF(BODY(cls)) == EXTERNALSXP)
        return;
    SEXP cmp = ctx->closureCompiler(cls, name);
    SET_BODY(cls, BODY(cmp));
}

void closureDebug(SEXP call, SEXP op, SEXP rho, SEXP newrho, RCNTXT* cntxt) {
    // TODO!!!
}

void endClosureDebug(SEXP call, SEXP op, SEXP rho) {
    // TODO!!!
}

/** Given argument code offsets, creates the argslist from their promises.
 */
// TODO unnamed only at this point
RIR_INLINE void __listAppend(SEXP* front, SEXP* last, SEXP value, SEXP name) {
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

SEXP createLegacyArgsListFromStackValues(const CallContext& call,
                                         bool eagerCallee, Context* ctx) {
    SEXP result = R_NilValue;
    SEXP pos = result;

    for (size_t i = 0; i < call.suppliedArgs; ++i) {

        SEXP name = call.hasNames() ? call.name(i, ctx) : R_NilValue;

        PROTECT(name);
        PROTECT(result);
        SEXP arg = stack_obj_to_sexp(call.stackArg(i));
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

SEXP createLegacyArgsList(const CallContext& call, bool eagerCallee,
                          Context* ctx) {
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
                // TODO
                SEXP env = call.callerEnv;
                SEXP arg = evalRirCodeExtCaller(call.implicitArg(i), ctx, &env);
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

SEXP argsLazyCreation(void* rirDataWrapper) {
    ArgsLazyData* argsLazy = ArgsLazyData::unpack(rirDataWrapper);
    return argsLazy->createArgsLists();
}

RIR_INLINE SEXP createLegacyLazyArgsList(const CallContext& call,
                                         Context* ctx) {
    if (call.hasStackArgs()) {
        return createLegacyArgsListFromStackValues(call, false, ctx);
    } else {
        return createLegacyArgsList(call, false, ctx);
    }
}

RIR_INLINE SEXP createLegacyArgsList(const CallContext& call, Context* ctx) {
    if (call.hasStackArgs()) {
        return createLegacyArgsListFromStackValues(call, call.hasEagerCallee(),
                                                   ctx);
    } else {
        return createLegacyArgsList(call, call.hasEagerCallee(), ctx);
    }
}

static SEXP rirCallTrampoline_(RCNTXT& cntxt, const CallContext& call,
                               Code* code, SEXP* env,
                               const R_bcstack_t* stackArgs, Context* ctx) {
    int trampIn = ostack_length(ctx);
    if ((SETJMP(cntxt.cjmpbuf))) {
        assert(trampIn == ostack_length(ctx));
        if (R_ReturnedValue == R_RestartToken) {
            cntxt.callflag = CTXT_RETURN; /* turn restart off */
            R_ReturnedValue = R_NilValue; /* remove restart token */
            return stack_obj_to_sexp(evalRirCode(code, ctx, env, &call));
        } else {
            return R_ReturnedValue;
        }
    }
    return stack_obj_to_sexp(evalRirCode(code, ctx, env, &call));
}

RIR_INLINE SEXP rirCallTrampoline(const CallContext& call, Function* fun,
                                  SEXP env, SEXP arglist,
                                  const R_bcstack_t* stackArgs, Context* ctx) {
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
    SEXP result =
        rirCallTrampoline_(cntxt, call, code, &cntxt.cloenv, stackArgs, ctx);
    PROTECT(result);

    endClosureDebug(call.ast, call.callee, env);
    endClosureContext(&cntxt, result);

    R_Srcref = cntxt.srcref;
    R_ReturnedValue = R_NilValue;

    UNPROTECT(2);
    return result;
}

RIR_INLINE SEXP rirCallTrampoline(const CallContext& call, Function* fun,
                                  SEXP arglist, Context* ctx) {
    return rirCallTrampoline(call, fun, R_NilValue, arglist, call.stackArgs,
                             ctx);
}

RIR_INLINE SEXP rirCallTrampoline(const CallContext& call, Function* fun,
                                  SEXP env, SEXP arglist, Context* ctx) {
    return rirCallTrampoline(call, fun, env, arglist, nullptr, ctx);
}

R_bcstack_t evalRirCode(Code*, Context*, SEXP*, const CallContext*, Opcode*,
                        R_bcstack_t*);
static void loopTrampoline(Code* c, Context* ctx, SEXP* env,
                           const CallContext* callCtxt, Opcode* pc,
                           R_bcstack_t* localsBase) {
    assert(*env);

    RCNTXT cntxt;
    Rf_begincontext(&cntxt, CTXT_LOOP, R_NilValue, *env, R_BaseEnv, R_NilValue,
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
        stack_obj_to_sexp(evalRirCode(c, ctx, env, callCtxt, pc, localsBase));
    assert(res == loopTrampolineMarker);
    Rf_endcontext(&cntxt);
}

void warnSpecial(SEXP callee, SEXP call) {
    return;

    // Enable this to find specials which are not implemented in RIR bytecodes
#if 0
    static bool isWarning = false;

    if (!isWarning) {
        isWarning = true;
        Rf_PrintValue(call);
        isWarning = false;
    }

    return;
    if (((sexprec_rjit*)callee)->u.i == 26) {
        Rprintf("warning: calling special: .Internal(%s\n",
                CHAR(PRINTNAME(CAR(CADR(call)))));
    } else {
        Rprintf("warning: calling special: %s\n",
                R_FunTab[((sexprec_rjit*)callee)->u.i].name);
    }
#endif
}

RIR_INLINE SEXP legacySpecialCall(const CallContext& call, Context* ctx) {
    assert(call.ast != R_NilValue);

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

RIR_INLINE SEXP legacyCallWithArgslist(const CallContext& call, SEXP argslist,
                                       Context* ctx) {
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

RIR_INLINE SEXP legacyCall(const CallContext& call, Context* ctx) {
    // create the argslist
    SEXP argslist = createLegacyArgsList(call, ctx);
    PROTECT(argslist);
    SEXP res = legacyCallWithArgslist(call, argslist, ctx);
    UNPROTECT(1);
    return res;
}

SEXP closureArgumentAdaptor(const CallContext& call, SEXP arglist,
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

void addDynamicAssumptionsFromContext(CallContext& call) {
    Assumptions& given = call.givenAssumptions;

    if (!call.hasNames())
        given.add(Assumption::CorrectOrderOfArguments);

    given.add(Assumption::NoExplicitlyMissingArgs);
    if (call.hasStackArgs()) {
        // Always true in this case, since we will pad missing args on the stack
        // later with R_MissingArg's
        given.add(Assumption::NotTooFewArguments);

        auto testArg = [&](size_t i) {
            R_bcstack_t arg = call.stackArg(i);
            bool notObj = true;
            bool isEager = true;
            if (stack_obj_sexp_type(arg) == PROMSXP) {
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

RIR_INLINE Assumptions addDynamicAssumptionsForOneTarget(
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

RIR_INLINE bool matches(const CallContext& call,
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
RIR_INLINE void supplyMissingArgs(CallContext& call, const Function* fun) {
    auto signature = fun->signature();
    assert(call.hasStackArgs());
    if (signature.expectedNargs() > call.suppliedArgs) {
        for (size_t i = 0; i < signature.expectedNargs() - call.suppliedArgs;
             ++i)
            ostack_push(ctx, sexp_to_stack_obj(R_MissingArg, false));
        call.passedArgs = signature.expectedNargs();
    }
}

Function* dispatch(const CallContext& call, DispatchTable* vt) {
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

static unsigned RIR_WARMUP =
    getenv("RIR_WARMUP") ? atoi(getenv("RIR_WARMUP")) : 3;

// Call a RIR function. Arguments are still untouched.
RIR_INLINE SEXP rirCall(CallContext& call, Context* ctx) {
    SEXP body = BODY(call.callee);
    assert(DispatchTable::check(body));

    auto table = DispatchTable::unpack(body);

    addDynamicAssumptionsFromContext(call);
    Function* fun = dispatch(call, table);
    fun->registerInvocation();

    if (!fun->unoptimizable && fun->invocationCount() % RIR_WARMUP == 0) {
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
    SEXP env = R_NilValue;
    SEXP result = nullptr;
    auto arglist = call.arglist;
    if (needsEnv) {
        if (!arglist)
            arglist = createLegacyLazyArgsList(call, ctx);
        PROTECT(arglist);
        env = closureArgumentAdaptor(call, arglist, R_NilValue);
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

SEXP doCall(CallContext& call, Context* ctx) {
    assert(call.callee);

    switch (TYPEOF(call.callee)) {
    case SPECIALSXP:
        return legacySpecialCall(call, ctx);
    case BUILTINSXP:
        return legacyCall(call, ctx);
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
                   SEXP callerEnv, Context* ctx) {
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
        SEXP lhs_sexp = stack_obj_to_sexp(lhs);                                \
        PROTECT(lhs_sexp);                                                     \
        SEXP rhs_sexp = stack_obj_to_sexp(rhs);                                \
        SEXP argslist = CONS_NR(lhs_sexp, CONS_NR(rhs_sexp, R_NilValue));      \
        UNPROTECT(2);                                                          \
        ostack_push(ctx, sexp_to_stack_obj(argslist, true));                   \
        if (flag < 2)                                                          \
            R_Visible = static_cast<Rboolean>(flag != 1);                      \
        SEXP res = blt(call, prim, argslist, *env);                            \
        if (flag < 2)                                                          \
            R_Visible = static_cast<Rboolean>(flag != 1);                      \
        ostack_pop(ctx);                                                       \
        STORE_BINOP(sexp_to_stack_obj(res, true));                             \
    } while (false)

#define STORE_BINOP(res)                                                       \
    do {                                                                       \
        ostack_pop(ctx);                                                       \
        ostack_set(ctx, 0, res);                                               \
    } while (false)

#define DO_BINOP(op, Op2)                                                      \
    do {                                                                       \
        if (lhs.tag == STACK_OBJ_REAL) {                                       \
            if (rhs.tag == STACK_OBJ_REAL) {                                   \
                double real_res =                                              \
                    (lhs.u.dval == NA_REAL || rhs.u.dval == NA_REAL)           \
                        ? NA_REAL                                              \
                        : lhs.u.dval op rhs.u.dval;                            \
                STORE_BINOP(real_stack_obj(real_res));                         \
            } else if (rhs.tag == STACK_OBJ_INT) {                             \
                double real_res =                                              \
                    (lhs.u.dval == NA_REAL || rhs.u.ival == NA_INTEGER)        \
                        ? NA_REAL                                              \
                        : lhs.u.dval op rhs.u.ival;                            \
                STORE_BINOP(real_stack_obj(real_res));                         \
            } else {                                                           \
                BINOP_FALLBACK(#op);                                           \
            }                                                                  \
        } else if (lhs.tag == STACK_OBJ_INT) {                                 \
            if (rhs.tag == STACK_OBJ_INT) {                                    \
                Rboolean naflag = FALSE;                                       \
                int int_res = Op2(lhs.u.ival, rhs.u.ival, &naflag);            \
                CHECK_INTEGER_OVERFLOW(naflag);                                \
                STORE_BINOP(int_stack_obj(int_res));                           \
            } else if (rhs.u.dval == STACK_OBJ_REAL) {                         \
                double real_res =                                              \
                    (lhs.u.ival == NA_INTEGER || rhs.u.dval == NA_REAL)        \
                        ? NA_REAL                                              \
                        : lhs.u.ival op rhs.u.dval;                            \
                STORE_BINOP(real_stack_obj(real_res));                         \
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

typedef struct {
    int ibeta, it, irnd, ngrd, machep, negep, iexp, minexp, maxexp;
    double eps, epsneg, xmin, xmax;
} AccuracyInfo;
LibExtern AccuracyInfo R_AccuracyInfo;
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

#define STORE_UNOP(res) ostack_set(ctx, 0, res)

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
        SEXP argslist = CONS_NR(stack_obj_to_sexp(val), R_NilValue);           \
        UNPROTECT(1);                                                          \
        ostack_push(ctx, sexp_to_stack_obj(argslist, true));                   \
        if (flag < 2)                                                          \
            R_Visible = static_cast<Rboolean>(flag != 1);                      \
        SEXP res = blt(call, prim, argslist, *env);                            \
        if (flag < 2)                                                          \
            R_Visible = static_cast<Rboolean>(flag != 1);                      \
        ostack_pop(ctx);                                                       \
        STORE_UNOP(sexp_to_stack_obj(res, true));                              \
    } while (false)

#define DO_UNOP(op, Op2)                                                       \
    do {                                                                       \
        R_bcstack_t res;                                                       \
        Rboolean naflag = FALSE;                                               \
        switch (val.tag) {                                                     \
        case STACK_OBJ_REAL:                                                   \
            res = real_stack_obj((val.u.dval == NA_REAL) ? NA_REAL             \
                                                         : op val.u.dval);     \
            STORE_UNOP(res);                                                   \
            break;                                                             \
        case STACK_OBJ_INT:                                                    \
            res = real_stack_obj(Op2(val.u.ival, &naflag));                    \
            CHECK_INTEGER_OVERFLOW(naflag);                                    \
            STORE_UNOP(res);                                                   \
            break;                                                             \
        default:                                                               \
            UNOP_FALLBACK(#op);                                                \
            break;                                                             \
        }                                                                      \
    } while (false)

#define DO_RELOP(op)                                                           \
    do {                                                                       \
        if (lhs.tag == STACK_OBJ_LOGICAL) {                                    \
            if (rhs.tag == STACK_OBJ_LOGICAL) {                                \
                if (lhs.u.ival == NA_LOGICAL || rhs.u.ival == NA_LOGICAL) {    \
                    STORE_BINOP(logical_stack_obj(NA_LOGICAL));                \
                } else {                                                       \
                    STORE_BINOP(logical_stack_obj(lhs.u.ival op rhs.u.ival));  \
                }                                                              \
                break;                                                         \
            }                                                                  \
        } else if (lhs.tag == STACK_OBJ_REAL) {                                \
            if (rhs.tag == STACK_OBJ_REAL) {                                   \
                if (lhs.u.dval == NA_REAL || rhs.u.dval == NA_REAL) {          \
                    STORE_BINOP(logical_stack_obj(NA_LOGICAL));                \
                } else {                                                       \
                    STORE_BINOP(logical_stack_obj(lhs.u.dval op rhs.u.dval));  \
                }                                                              \
                break;                                                         \
            } else if (rhs.tag == STACK_OBJ_INT) {                             \
                if (lhs.u.dval == NA_REAL || rhs.u.ival == NA_INTEGER) {       \
                    STORE_BINOP(logical_stack_obj(NA_LOGICAL));                \
                } else {                                                       \
                    STORE_BINOP(logical_stack_obj(lhs.u.dval op rhs.u.ival));  \
                }                                                              \
                break;                                                         \
            }                                                                  \
        } else if (lhs.tag == STACK_OBJ_INT) {                                 \
            if (rhs.tag == STACK_OBJ_INT) {                                    \
                if (lhs.u.ival == NA_INTEGER || rhs.u.ival == NA_INTEGER) {    \
                    STORE_BINOP(logical_stack_obj(NA_LOGICAL));                \
                } else {                                                       \
                    STORE_BINOP(logical_stack_obj(lhs.u.ival op rhs.u.ival));  \
                }                                                              \
                break;                                                         \
            } else if (rhs.tag == STACK_OBJ_REAL) {                            \
                if (lhs.u.ival == NA_INTEGER || rhs.u.dval == NA_REAL) {       \
                    STORE_BINOP(logical_stack_obj(NA_LOGICAL));                \
                } else {                                                       \
                    STORE_BINOP(logical_stack_obj(lhs.u.ival op rhs.u.dval));  \
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

void debug(Code* c, Opcode* pc, const char* name, unsigned depth,
           Context* ctx) {
    return;
    // Enable this to trace the execution of every BC
#if 0
    if (debugging == 0) {
        debugging = 1;
        printf("%p : %d, %s, s: %u\n", c, (int)*pc, name, depth);
        for (unsigned i = 0; i < depth; ++i) {
            printf("%3u: ", i);
            Rf_PrintValue(ostack_at(ctx, i));
        }
        printf("\n");
        debugging = 0;
    }
#endif
}

#define BINDING_CACHE_SIZE 5
typedef struct {
    SEXP loc;
    Immediate idx;
} BindingCache;

RIR_INLINE SEXP cachedGetBindingCell(SEXP env, Immediate idx, Context* ctx,
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

static SEXP cachedGetVar(SEXP env, Immediate idx, Context* ctx,
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
static void cachedSetVar(SEXP val, SEXP env, Immediate idx, Context* ctx,
                         BindingCache* bindingCache, bool keepMissing = false) {
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

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-align"

R_bcstack_t evalRirCode(Code* c, Context* ctx, SEXP* env,
                        const CallContext* callCtxt, Opcode* initialPC,
                        R_bcstack_t* localsBase = nullptr) {
    assert(*env || (callCtxt != nullptr));

    extern int R_PPStackTop;

#ifdef THREADED_CODE
    static void* opAddr[static_cast<uint8_t>(Opcode::num_of)] = {
#define DEF_INSTR(name, ...) (__extension__ && op_##name),
#include "ir/insns.h"
#undef DEF_INSTR
    };
#endif

    std::deque<FrameInfo*>* synthesizeFrames = nullptr;
    assert(c->info.magic == CODE_MAGIC);

    if (!localsBase) {
#ifdef TYPED_STACK
        // Zero the region of the locals to avoid keeping stuff alive and to
        // zero all the type tags. Note: this trick does not work with the stack
        // in general, since there intermediate callees might set the type tags
        // to something else.
        memset(R_BCNodeStackTop, 0, sizeof(*R_BCNodeStackTop) * c->localsCount);
#endif
        localsBase = R_BCNodeStackTop;
    }
    Locals locals(localsBase, c->localsCount);

    BindingCache bindingCache[BINDING_CACHE_SIZE];
    memset(&bindingCache, 0, sizeof(bindingCache));

    // make sure there is enough room on the stack
    // there is some slack of 5 to make sure the call instruction can store
    // some intermediate values on the stack
    ostack_ensureSize(ctx, c->stackLength + 5);

    Opcode* pc = initialPC ? initialPC : c->code();

    R_Visible = TRUE;

    // main loop
    BEGIN_MACHINE {

        INSTRUCTION(invalid_) assert(false && "wrong or unimplemented opcode");

        INSTRUCTION(nop_) NEXT();
      
        INSTRUCTION(mk_env_) {
            size_t n = readImmediate();
            advanceImmediate();
            R_bcstack_t parent = ostack_pop(ctx);
            assert(stack_obj_sexp_type(parent) == ENVSXP &&
                   "Non-environment used as environment parent.");
            SEXP arglist = R_NilValue;
            auto names = (Immediate*)pc;
            advanceImmediateN(n);
            PROTECT(parent.u.sxpval);
            for (long i = n - 1; i >= 0; --i) {
                PROTECT(arglist);
                SEXP val = stack_obj_to_sexp(ostack_pop(ctx));
                UNPROTECT(1);
                SEXP name = cp_pool_at(ctx, names[i]);
                arglist = CONS_NR(val, arglist);
                SET_TAG(arglist, name);
                SET_MISSING(arglist, val == R_MissingArg ? 2 : 0);
            }
            UNPROTECT(1);
            SEXP res = Rf_NewEnvironment(R_NilValue, arglist, parent.u.sxpval);
            ostack_push(ctx, sexp_to_stack_obj(res, true));
            NEXT();
        }

        INSTRUCTION(parent_env_) {
            // Can only be used for pir. In pir we always have a closure that
            // stores the lexical envrionment
            assert(callCtxt);
            ostack_push(ctx,
                        sexp_to_stack_obj(CLOENV(callCtxt->callee), false));
            NEXT();
        }

        INSTRUCTION(get_env_) {
            assert(env);
            ostack_push(ctx, sexp_to_stack_obj(*env, false));
            NEXT();
        }

        INSTRUCTION(set_env_) {
            // We need to clear the bindings cache, when we change the
            // environment
            memset(&bindingCache, 0, sizeof(bindingCache));
            R_bcstack_t e = ostack_pop(ctx);
            assert(stack_obj_sexp_type(e) == ENVSXP &&
                   "Expected an environment on TOS.");
            *env = e.u.sxpval;
            NEXT();
        }

        INSTRUCTION(ldfun_) {
            SEXP sym = readConst(ctx, readImmediate());
            advanceImmediate();
            SEXP res = Rf_findFun(sym, *env);

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
            ostack_push(ctx, sexp_to_stack_obj(res, true));
            NEXT();
        }

        INSTRUCTION(ldvar_) {
            Immediate id = readImmediate();
            advanceImmediate();
            SEXP res = cachedGetVar(*env, id, ctx, bindingCache);
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

            ostack_push(ctx, sexp_to_stack_obj(res, true));
            NEXT();
        }

        INSTRUCTION(ldvar_noforce_) {
            Immediate id = readImmediate();
            advanceImmediate();
            SEXP res = cachedGetVar(*env, id, ctx, bindingCache);
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

            ostack_push(ctx, sexp_to_stack_obj(res, true));
            NEXT();
        }

        INSTRUCTION(ldvar_super_) {
            SEXP sym = readConst(ctx, readImmediate());
            advanceImmediate();
            SEXP res = Rf_findVar(sym, ENCLOS(*env));
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

            ostack_push(ctx, sexp_to_stack_obj(res, true));
            NEXT();
        }

        INSTRUCTION(ldvar_noforce_super_) {
            SEXP sym = readConst(ctx, readImmediate());
            advanceImmediate();
            SEXP res = Rf_findVar(sym, ENCLOS(*env));
            R_Visible = TRUE;

            if (res == R_UnboundValue) {
                Rf_error("object \"%s\" not found", CHAR(PRINTNAME(sym)));
            } else if (res == R_MissingArg) {
                Rf_error("argument \"%s\" is missing, with no default",
                         CHAR(PRINTNAME(sym)));
            }

            if (res != R_NilValue)
                ENSURE_NAMED(res);

            ostack_push(ctx, sexp_to_stack_obj(res, true));
            NEXT();
        }

        INSTRUCTION(ldddvar_) {
            SEXP sym = readConst(ctx, readImmediate());
            advanceImmediate();
            SEXP res = Rf_ddfindVar(sym, *env);
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

            ostack_push(ctx, sexp_to_stack_obj(res, true));
            NEXT();
        }

        INSTRUCTION(ldlval_) {
            Immediate id = readImmediate();
            advanceImmediate();
            SEXP res = cachedGetBindingCell(*env, id, ctx, bindingCache);
            assert(res);
            res = CAR(res);
            assert(res != R_UnboundValue);

            R_Visible = TRUE;

            if (TYPEOF(res) == PROMSXP)
                res = PRVALUE(res);

            assert(res != R_UnboundValue);
            assert(res != R_MissingArg);

            if (res != R_NilValue)
                ENSURE_NAMED(res);

            ostack_push(ctx, sexp_to_stack_obj(res, true));
            NEXT();
        }

        INSTRUCTION(ldarg_) {
            Immediate idx = readImmediate();
            advanceImmediate();
            assert(callCtxt);

            if (callCtxt->hasStackArgs()) {
                ostack_push(ctx, callCtxt->stackArg(idx));
            } else {
                SEXP res;
                if (callCtxt->missingArg(idx)) {
                    res = R_MissingArg;
                } else {
                    Code* arg = callCtxt->implicitArg(idx);
                    res = createPromise(arg, callCtxt->callerEnv);
                }
                ostack_push(ctx, sexp_to_stack_obj(res, true));
            }
            NEXT();
        }

        INSTRUCTION(ldloc_) {
            Immediate offset = readImmediate();
            advanceImmediate();
            R_bcstack_t res = locals.load(offset);
            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(stvar_) {
            Immediate id = readImmediate();
            advanceImmediate();
            SEXP val = stack_obj_to_sexp(ostack_pop(ctx));

            cachedSetVar(val, *env, id, ctx, bindingCache);

            NEXT();
        }

        INSTRUCTION(starg_) {
            Immediate id = readImmediate();
            advanceImmediate();
            SEXP val = stack_obj_to_sexp(ostack_pop(ctx));

            cachedSetVar(val, *env, id, ctx, bindingCache, true);

            NEXT();
        }

        INSTRUCTION(stvar_super_) {
            SEXP sym = readConst(ctx, readImmediate());
            advanceImmediate();
            SLOWASSERT(TYPEOF(sym) == SYMSXP);
            PROTECT(sym);
            SEXP val = stack_obj_to_sexp(ostack_pop(ctx));
            UNPROTECT(1);
            INCREMENT_NAMED(val);
            Rf_setVar(sym, val, ENCLOS(*env));
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
            Assumptions given(readImmediate());
            advanceImmediate();
            auto arguments = (Immediate*)pc;
            advanceImmediateN(n);
            auto names = (Immediate*)pc;
            advanceImmediateN(n);
            CallContext call(c, stack_obj_to_sexp(ostack_top(ctx)), n, ast,
                             arguments, names, *env, given, ctx);
            SEXP res = doCall(call, ctx);
            ostack_pop(ctx); // callee
            ostack_push(ctx, sexp_to_stack_obj(res, true));

            SLOWASSERT(ttt == R_PPStackTop);
            SLOWASSERT(lll == ostack_length(ctx));
            NEXT();
        }

        INSTRUCTION(record_call_) {
            ObservedCallees* feedback = (ObservedCallees*)pc;
            SEXP callee = stack_obj_to_sexp(ostack_top(ctx));
            feedback->record(c, callee);
            pc += sizeof(ObservedCallees);
            NEXT();
        }

        INSTRUCTION(record_binop_) {
            ObservedValues* feedback = (ObservedValues*)pc;
            SEXP l = stack_obj_to_sexp(ostack_at(ctx, 1));
            SEXP r = stack_obj_to_sexp(ostack_top(ctx));
            feedback[0].record(l);
            feedback[1].record(r);
            pc += 2 * sizeof(ObservedValues);
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
            Assumptions given(readImmediate());
            advanceImmediate();
            auto arguments = (Immediate*)pc;
            advanceImmediateN(n);
            CallContext call(c, stack_obj_to_sexp(ostack_top(ctx)), n, ast,
                             arguments, *env, given, ctx);
            SEXP res = doCall(call, ctx);
            ostack_pop(ctx); // callee
            ostack_push(ctx, sexp_to_stack_obj(res, true));

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
            Assumptions given(readImmediate());
            advanceImmediate();
            SEXP fun = stack_obj_to_sexp(ostack_at(ctx, n));
            CallContext call(c, fun, n, ast, ostack_cell_at(ctx, n - 1), *env,
                             given, ctx);
            SEXP res = doCall(call, ctx);
            ostack_popn(ctx, call.passedArgs + 1);
            ostack_push(ctx, sexp_to_stack_obj(res, true));

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
            Assumptions given(readImmediate());
            advanceImmediate();
            auto names = (Immediate*)pc;
            advanceImmediateN(n);
            CallContext call(c, stack_obj_to_sexp(ostack_at(ctx, n)), n, ast,
                             ostack_cell_at(ctx, n - 1), names, *env, given,
                             ctx);
            SEXP res = doCall(call, ctx);
            ostack_popn(ctx, call.passedArgs + 1);
            ostack_push(ctx, sexp_to_stack_obj(res, true));

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
            CallContext call(c, callee, n, ast, ostack_cell_at(ctx, n - 1),
                             *env, Assumptions(), ctx);
            SEXP res = legacyCall(call, ctx);
            ostack_popn(ctx, call.passedArgs);
            ostack_push(ctx, sexp_to_stack_obj(res, true));

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
            Assumptions given(readImmediate());
            advanceImmediate();
            SEXP callee = cp_pool_at(ctx, readImmediate());
            advanceImmediate();
            SEXP version = cp_pool_at(ctx, readImmediate());
            CallContext call(c, callee, n, ast, ostack_cell_at(ctx, n - 1),
                             *env, given, ctx);
            auto fun = Function::unpack(version);
            SEXP res;
            addDynamicAssumptionsFromContext(call);
            bool dispatchFail = !matches(call, fun->signature());
            if (fun->invocationCount() % RIR_WARMUP == 0)
                if (addDynamicAssumptionsForOneTarget(call, fun->signature()) !=
                    fun->signature().assumptions)
                    // We have more assumptions available, let's recompile
                    dispatchFail = true;

            if (dispatchFail) {
                auto dt = DispatchTable::unpack(BODY(callee));
                fun = dispatch(call, dt);
                // Patch inline cache
                (*(Immediate*)pc) = Pool::insert(fun->container());
                assert(fun != dt->baseline());
            }
            advanceImmediate();

            ArgsLazyData lazyArgs(&call, ctx);
            fun->registerInvocation();
            supplyMissingArgs(call, fun);
            res = rirCallTrampoline(call, fun, *env, (SEXP)&lazyArgs,
                                    call.stackArgs, ctx);
            ostack_popn(ctx, call.passedArgs);
            ostack_push(ctx, sexp_to_stack_obj(res, true));

            SLOWASSERT(ttt == R_PPStackTop);
            SLOWASSERT(lll - call.suppliedArgs + 1 ==
                       (unsigned)ostack_length(ctx));
            NEXT();
        }

        INSTRUCTION(close_) {
            SEXP srcref = stack_obj_to_sexp(ostack_at(ctx, 0));
            SEXP body = stack_obj_to_sexp(ostack_at(ctx, 1));
            SEXP formals = stack_obj_to_sexp(ostack_at(ctx, 2));
            SEXP res = Rf_allocSExp(CLOSXP);
            assert(DispatchTable::check(body));
            SET_FORMALS(res, formals);
            SET_BODY(res, body);
            SET_CLOENV(res, *env);
            Rf_setAttrib(res, Rf_install("srcref"), srcref);
            ostack_popn(ctx, 3);
            ostack_push(ctx, sexp_to_stack_obj(res, true));
            NEXT();
        }

        INSTRUCTION(isfun_) {
            R_bcstack_t val = ostack_top(ctx);

            switch (stack_obj_sexp_type(val)) {
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
            SEXP prom = Rf_mkPROMISE(c->getPromise(id)->container(), *env);
            PROTECT(prom);
            SET_PRVALUE(prom, stack_obj_to_sexp(ostack_pop(ctx)));
            UNPROTECT(1);
            ostack_push(ctx, sexp_to_stack_obj(prom, true));
            NEXT();
        }

        INSTRUCTION(force_) {
            if (stack_obj_sexp_type(ostack_top(ctx)) == PROMSXP) {
                SEXP val = ostack_pop(ctx).u.sxpval;
                // If the promise is already evaluated then push the value
                // inside the promise onto the stack, otherwise push the value
                // from forcing the promise
                ostack_push(ctx,
                            sexp_to_stack_obj(promiseValue(val, ctx), true));
            }
            NEXT();
        }

        INSTRUCTION(push_) {
            SEXP res = readConst(ctx, readImmediate());
            advanceImmediate();
            R_Visible = TRUE;
            ostack_push(ctx, sexp_to_stack_obj(res, true));
            NEXT();
        }

        INSTRUCTION(push_code_) {
            Immediate n = readImmediate();
            advanceImmediate();
            ostack_push(ctx,
                        sexp_to_stack_obj(c->getPromise(n)->container(), true));
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

        INSTRUCTION(swap_) {
            R_bcstack_t lhs = ostack_pop(ctx);
            R_bcstack_t rhs = ostack_pop(ctx);
            ostack_push(ctx, lhs);
            ostack_push(ctx, rhs);
            NEXT();
        }

        INSTRUCTION(put_) {
            Immediate i = readImmediate();
            advanceImmediate();
            R_bcstack_t* pos = ostack_cell_at(ctx, 0);
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
            R_bcstack_t* pos = ostack_cell_at(ctx, i);
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
            R_bcstack_t val = ostack_at(ctx, i);
            ostack_push(ctx, val);
            NEXT();
        }

        INSTRUCTION(add_) {
            R_bcstack_t lhs = ostack_at(ctx, 1);
            R_bcstack_t rhs = ostack_at(ctx, 0);
            DO_BINOP(+, R_integer_plus);
            NEXT();
        }

        INSTRUCTION(uplus_) {
            R_bcstack_t val = ostack_at(ctx, 0);
            DO_UNOP(+, R_integer_uplus);
            NEXT();
        }

        INSTRUCTION(inc_) {
            R_bcstack_t val = ostack_pop(ctx);
#ifdef USE_TYPED_STACK
            assert(val.tag == STACK_OBJ_INT);
            int i = val.u.ival + 1;
#else
            assert(TYPEOF(val.u.sxpval) == INTSXP);
            int i = *INTEGER(val.u.sxpval) + 1;
#endif
            ostack_push(ctx, int_stack_obj(i));
            NEXT();
        }

        INSTRUCTION(sub_) {
            R_bcstack_t lhs = ostack_at(ctx, 1);
            R_bcstack_t rhs = ostack_at(ctx, 0);
            DO_BINOP(-, R_integer_minus);
            NEXT();
        }

        INSTRUCTION(uminus_) {
            R_bcstack_t val = ostack_at(ctx, 0);
            DO_UNOP(-, R_integer_uminus);
            NEXT();
        }

        INSTRUCTION(mul_) {
            R_bcstack_t lhs = ostack_at(ctx, 1);
            R_bcstack_t rhs = ostack_at(ctx, 0);
            DO_BINOP(*, R_integer_times);
            NEXT();
        }

        INSTRUCTION(div_) {
            R_bcstack_t lhs = ostack_at(ctx, 1);
            R_bcstack_t rhs = ostack_at(ctx, 0);

            if (lhs.tag == STACK_OBJ_REAL && rhs.tag == STACK_OBJ_REAL) {
                double real_res =
                    (lhs.u.dval == NA_REAL || rhs.u.dval == NA_REAL)
                        ? NA_REAL
                        : lhs.u.dval / rhs.u.dval;
                STORE_BINOP(real_stack_obj(real_res));
            } else if (lhs.tag == STACK_OBJ_INT && rhs.tag == STACK_OBJ_INT) {
                double real_res;
                int l = lhs.u.ival;
                int r = rhs.u.ival;
                if (l == NA_INTEGER || r == NA_INTEGER)
                    real_res = NA_REAL;
                else
                    real_res = (double)l / (double)r;
                STORE_BINOP(real_stack_obj(real_res));
            } else {
                BINOP_FALLBACK("/");
            }
            NEXT();
        }

        INSTRUCTION(idiv_) {
            R_bcstack_t lhs = ostack_at(ctx, 1);
            R_bcstack_t rhs = ostack_at(ctx, 0);

            if (lhs.tag == STACK_OBJ_REAL && rhs.tag == STACK_OBJ_REAL) {
                double real_res =
                    (lhs.u.dval == NA_REAL || rhs.u.dval == NA_REAL)
                        ? NA_REAL
                        : myfloor(lhs.u.dval, rhs.u.dval);
                STORE_BINOP(real_stack_obj(real_res));
            } else if (lhs.tag == STACK_OBJ_INT && rhs.tag == STACK_OBJ_INT) {
                int int_res;
                int l = lhs.u.ival;
                int r = rhs.u.ival;
                if (l == NA_INTEGER || r == NA_INTEGER)
                    int_res = NA_REAL;
                else
                    int_res = (int)floor((double)l / (double)r);
                STORE_BINOP(int_stack_obj(int_res));
            } else {
                BINOP_FALLBACK("%/%");
            }
            NEXT();
        }

        INSTRUCTION(mod_) {
            R_bcstack_t lhs = ostack_at(ctx, 1);
            R_bcstack_t rhs = ostack_at(ctx, 0);

            if (lhs.tag == STACK_OBJ_REAL && rhs.tag == STACK_OBJ_REAL) {
                double real_res = myfmod(lhs.u.dval, rhs.u.dval);
                STORE_BINOP(real_stack_obj(real_res));
            } else if (lhs.tag == STACK_OBJ_INT && rhs.tag == STACK_OBJ_INT) {
                int int_res;
                int l = lhs.u.ival;
                int r = rhs.u.ival;
                if (l == NA_INTEGER || r == NA_INTEGER || r == 0) {
                    int_res = NA_INTEGER;
                } else {
                    int_res = (l >= 0 && r > 0)
                                  ? l % r
                                  : (int)myfmod((double)l, (double)r);
                }
                STORE_BINOP(int_stack_obj(int_res));
            } else {
                BINOP_FALLBACK("%%");
            }
            NEXT();
        }

        INSTRUCTION(pow_) {
            R_bcstack_t lhs = ostack_at(ctx, 1);
            R_bcstack_t rhs = ostack_at(ctx, 0);
            BINOP_FALLBACK("^");
            NEXT();
        }

        INSTRUCTION(lt_) {
            R_bcstack_t lhs = ostack_at(ctx, 1);
            R_bcstack_t rhs = ostack_at(ctx, 0);
            DO_RELOP(<);
            NEXT();
        }

        INSTRUCTION(gt_) {
            R_bcstack_t lhs = ostack_at(ctx, 1);
            R_bcstack_t rhs = ostack_at(ctx, 0);
            DO_RELOP(>);
            NEXT();
        }

        INSTRUCTION(le_) {
            R_bcstack_t lhs = ostack_at(ctx, 1);
            R_bcstack_t rhs = ostack_at(ctx, 0);
            DO_RELOP(<=);
            NEXT();
        }

        INSTRUCTION(ge_) {
            R_bcstack_t lhs = ostack_at(ctx, 1);
            R_bcstack_t rhs = ostack_at(ctx, 0);
            DO_RELOP(>=);
            NEXT();
        }

        INSTRUCTION(eq_) {
            R_bcstack_t lhs = ostack_at(ctx, 1);
            R_bcstack_t rhs = ostack_at(ctx, 0);
            DO_RELOP(==);
            NEXT();
        }

        INSTRUCTION(identical_noforce_) {
            R_bcstack_t rhs = ostack_pop(ctx);
            R_bcstack_t lhs = ostack_pop(ctx);
            // This instruction does not force, but we should still compare
            // the actual promise value if it is already forced.
            // Especially important since all the inlined functions are probably
            // behind lazy loading stub promises.
            if (stack_obj_sexp_type(rhs) == PROMSXP &&
                PRVALUE(rhs.u.sxpval) != R_UnboundValue)
                rhs = sexp_to_stack_obj(PRVALUE(rhs.u.sxpval), true);
            if (stack_obj_sexp_type(lhs) == PROMSXP &&
                PRVALUE(lhs.u.sxpval) != R_UnboundValue)
                lhs = sexp_to_stack_obj(PRVALUE(lhs.u.sxpval), true);
            bool res = stack_objs_equal(lhs, rhs);
            ostack_push(ctx, logical_stack_obj(res));
            NEXT();
        }

        INSTRUCTION(ne_) {
            assert(R_PPStackTop >= 0);
            R_bcstack_t lhs = ostack_at(ctx, 1);
            R_bcstack_t rhs = ostack_at(ctx, 0);
            DO_RELOP(!=);
            NEXT();
        }

        INSTRUCTION(not_) {
            R_bcstack_t val = ostack_at(ctx, 0);

            switch (val.tag) {
            case STACK_OBJ_INT:
                int logical_res;
                if (val.u.ival == NA_INTEGER) {
                    logical_res = NA_LOGICAL;
                } else {
                    logical_res = (val.u.ival == 0);
                }
                STORE_UNOP(logical_stack_obj(logical_res));
                break;
            case STACK_OBJ_REAL:
                if (val.u.dval == NA_REAL) {
                    logical_res = NA_LOGICAL;
                } else {
                    logical_res = (val.u.dval == 0);
                }
                STORE_UNOP(logical_stack_obj(logical_res));
                break;
            case STACK_OBJ_LOGICAL:
                if (val.u.ival == NA_LOGICAL) {
                    logical_res = NA_LOGICAL;
                } else {
                    logical_res = (val.u.ival == 0);
                }
                STORE_UNOP(logical_stack_obj(logical_res));
                break;
            case STACK_OBJ_SEXP:
                UNOP_FALLBACK("!");
                break;
            default:
                assert(false);
            }

            NEXT();
        }

        INSTRUCTION(lgl_or_) {
            R_bcstack_t rhs = ostack_pop(ctx);
            R_bcstack_t lhs = ostack_pop(ctx);
#ifdef USE_TYPED_STACK
            assert(stack_obj_sexp_type(lhs) == LGLSXP &&
                   stack_obj_sexp_type(rhs) == LGLSXP);
            int x1 = try_stack_obj_to_logical_na(lhs);
            int x2 = try_stack_obj_to_logical_na(rhs);
#else
            int x1 = *LOGICAL(lhs.u.sxpval);
            int x2 = *LOGICAL(rhs.u.sxpval);
#endif
            assert(x1 == 1 || x1 == 0 || x1 == NA_LOGICAL);
            assert(x2 == 1 || x2 == 0 || x2 == NA_LOGICAL);
            int logical_res;
            if (x1 == 1 || x2 == 1) {
                logical_res = 1;
            } else if (x1 == 0 && x2 == 0) {
                logical_res = 0;
            } else {
                logical_res = NA_LOGICAL;
            }
            ostack_push(ctx, logical_stack_obj(logical_res));
            NEXT();
        }

        INSTRUCTION(lgl_and_) {
            R_bcstack_t rhs = ostack_pop(ctx);
            R_bcstack_t lhs = ostack_pop(ctx);
#ifdef USE_TYPED_STACK
            assert(stack_obj_sexp_type(lhs) == LGLSXP &&
                   stack_obj_sexp_type(rhs) == LGLSXP);
            int x1 = try_stack_obj_to_logical_na(lhs);
            int x2 = try_stack_obj_to_logical_na(rhs);
#else
            int x1 = *LOGICAL(lhs.u.sxpval);
            int x2 = *LOGICAL(rhs.u.sxpval);
#endif
            assert(x1 == 1 || x1 == 0 || x1 == NA_LOGICAL);
            assert(x2 == 1 || x2 == 0 || x2 == NA_LOGICAL);
            int logical_res;
            if (x1 == 1 && x2 == 1) {
                logical_res = 1;
            } else if (x1 == 0 || x2 == 0) {
                logical_res = 0;
            } else {
                logical_res = NA_LOGICAL;
            }
            ostack_push(ctx, logical_stack_obj(logical_res));
            NEXT();
        }

        INSTRUCTION(aslogical_) {
            R_bcstack_t val = ostack_top(ctx);
            int logical_res;
            switch (val.tag) {
            case STACK_OBJ_REAL:
                // TODO: Is this right?
                logical_res = (val.u.dval != 0.0);
                break;
            case STACK_OBJ_INT:
                // TODO: Is this right?
                logical_res = (val.u.ival != 0);
                break;
            case STACK_OBJ_LOGICAL:
                logical_res = val.u.ival;
                break;
            case STACK_OBJ_SEXP:
                logical_res = Rf_asLogical(val.u.sxpval);
                break;
            default:
                assert(false);
            }
            STORE_UNOP(logical_stack_obj(logical_res));
            NEXT();
        }

        INSTRUCTION(asbool_) {
            R_bcstack_t val = ostack_top(ctx);

            int logical_res;
            switch (val.tag) {
            case STACK_OBJ_REAL:
                // TODO: Is this right?
                logical_res = (val.u.dval != 0.0);
                break;
            case STACK_OBJ_INT:
                // TODO: Is this right?
                logical_res = (val.u.ival != 0);
                break;
            case STACK_OBJ_LOGICAL:
                logical_res = val.u.ival;
                break;
            case STACK_OBJ_SEXP:
                if (XLENGTH(val.u.sxpval) > 1)
                    Rf_warningcall(
                        getSrcAt(c, pc - 1, ctx),
                        "the condition has length > 1 and only the first "
                        "element will be used");

                if (!isLogical(val.u.sxpval)) {
                    Rf_errorcall(getSrcAt(c, pc - 1, ctx),
                                 "argument is not interpretable as logical");
                } else if (XLENGTH(val.u.sxpval) == 0) {
                    Rf_errorcall(getSrcAt(c, pc - 1, ctx),
                                 "argument is of length zero");
                } else {
                    logical_res = Rf_asLogical(val.u.sxpval);
                }
                break;
            default:
                assert(false);
            }
            if (logical_res == NA_LOGICAL) {
                Rf_errorcall(getSrcAt(c, pc - 1, ctx),
                             "missing value where TRUE/FALSE needed");
            }
            STORE_UNOP(logical_stack_obj(logical_res));
            NEXT();
        }

        INSTRUCTION(asast_) {
            R_bcstack_t val = ostack_pop(ctx);
            assert(stack_obj_sexp_type(val) == PROMSXP);
            SEXP res = PRCODE(val.u.sxpval);
            // if the code is EXTERNALSXP then it is rir Code object, get its
            // ast
            if (TYPEOF(res) == EXTERNALSXP)
                res = cp_pool_at(ctx, Code::unpack(res)->src);
            // otherwise return whatever we had, make sure we do not see
            // bytecode
            assert(TYPEOF(res) != BCODESXP);
            ostack_push(ctx, sexp_to_stack_obj(res, false));
            NEXT();
        }

        INSTRUCTION(is_) {
            R_bcstack_t val = ostack_pop(ctx);
            SEXPTYPE type = stack_obj_sexp_type(val);
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
            ostack_push(ctx, logical_stack_obj(logical_res));
            NEXT();
        }

        INSTRUCTION(isobj_) {
            R_bcstack_t val = ostack_pop(ctx);
            int logical_res =
                val.tag == STACK_OBJ_SEXP && isObject(val.u.sxpval);
            ostack_push(ctx, logical_stack_obj(logical_res));
            NEXT();
        }

        INSTRUCTION(missing_) {
            SEXP sym = readConst(ctx, readImmediate());
            advanceImmediate();
            SLOWASSERT(TYPEOF(sym) == SYMSXP);
            SLOWASSERT(!DDVAL(sym));
            assert(env);
            SEXP val = R_findVarLocInFrame(*env, sym).cell;
            if (val == NULL)
                Rf_errorcall(getSrcAt(c, pc - 1, ctx),
                             "'missing' can only be used for arguments");

            if (MISSING(val) || CAR(val) == R_MissingArg) {
                ostack_push(ctx, logical_stack_obj(true));
                NEXT();
            }

            val = CAR(val);

            if (TYPEOF(val) != PROMSXP) {
                ostack_push(ctx, logical_stack_obj(false));
                NEXT();
            }

            val = findRootPromise(val);
            if (!isSymbol(PREXPR(val)))
                ostack_push(ctx, logical_stack_obj(false));
            else {
                ostack_push(ctx, logical_stack_obj(
                                     R_isMissing(PREXPR(val), PRENV(val))));
            }
            NEXT();
        }

        INSTRUCTION(check_missing_) {
            R_bcstack_t val = ostack_top(ctx);
            if (val.tag == STACK_OBJ_SEXP && val.u.sxpval == R_MissingArg)
                Rf_error("argument is missing, with no default");
            NEXT();
        }

        INSTRUCTION(brobj_) {
            R_bcstack_t val = ostack_top(ctx);
            JumpOffset offset = readJumpOffset();
            advanceJump();
            if (val.tag == STACK_OBJ_SEXP && isObject(val.u.sxpval))
                pc += offset;
            PC_BOUNDSCHECK(pc, c);
            NEXT();
        }

        INSTRUCTION(brtrue_) {
            R_bcstack_t val = ostack_pop(ctx);
            JumpOffset offset = readJumpOffset();
            advanceJump();
            if (try_stack_obj_to_logical(val) == 1) {
                pc += offset;
            }
            PC_BOUNDSCHECK(pc, c);
            NEXT();
        }

        INSTRUCTION(brfalse_) {
            R_bcstack_t val = ostack_pop(ctx);
            JumpOffset offset = readJumpOffset();
            advanceJump();
            if (try_stack_obj_to_logical(val) == 0) {
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
            SEXP val = stack_obj_to_sexp(ostack_at(ctx, 1));
            SEXP idx = stack_obj_to_sexp(ostack_at(ctx, 0));
            SEXP args = CONS_NR(val, CONS_NR(idx, R_NilValue));
            ostack_push(ctx, sexp_to_stack_obj(args, true));

            SEXP res;
            if (isObject(val)) {
                SEXP call = getSrcForCall(c, pc - 1, ctx);
                res =
                    dispatchApply(call, val, args, symbol::Bracket, *env, ctx);
                if (!res)
                    res =
                        do_subset_dflt(R_NilValue, symbol::Bracket, args, *env);
            } else {
                res = do_subset_dflt(R_NilValue, symbol::Bracket, args, *env);
            }

            ostack_popn(ctx, 3);

            R_Visible = TRUE;
            ostack_push(ctx, sexp_to_stack_obj(res, true));
            NEXT();
        }

        INSTRUCTION(extract1_2_) {
            SEXP val = stack_obj_to_sexp(ostack_at(ctx, 2));
            SEXP idx = stack_obj_to_sexp(ostack_at(ctx, 1));
            SEXP idx2 = stack_obj_to_sexp(ostack_at(ctx, 0));

            SEXP args = CONS_NR(val, CONS_NR(idx, CONS_NR(idx2, R_NilValue)));
            ostack_push(ctx, sexp_to_stack_obj(args, true));

            SEXP res;
            if (isObject(val)) {
                SEXP call = getSrcForCall(c, pc - 1, ctx);
                res =
                    dispatchApply(call, val, args, symbol::Bracket, *env, ctx);
                if (!res)
                    res =
                        do_subset_dflt(R_NilValue, symbol::Bracket, args, *env);
            } else {
                res = do_subset_dflt(R_NilValue, symbol::Bracket, args, *env);
            }

            ostack_popn(ctx, 4);

            R_Visible = TRUE;
            ostack_push(ctx, sexp_to_stack_obj(res, true));
            NEXT();
        }

        INSTRUCTION(extract2_1_) {
            SEXP val = stack_obj_to_sexp(ostack_at(ctx, 1));
            R_bcstack_t idx = ostack_at(ctx, 0);
            int i = -1;

            if (ATTRIB(val) != R_NilValue)
                goto fallback;

            switch (idx.tag) {
            case STACK_OBJ_INT:
                if (idx.u.ival == NA_INTEGER)
                    goto fallback;
                i = idx.u.ival - 1;
                break;
            case STACK_OBJ_REAL:
                if (idx.u.dval == NA_REAL)
                    goto fallback;
                i = idx.u.dval - 1;
                break;
            case LGLSXP:
                if (idx.u.ival == NA_LOGICAL)
                    goto fallback;
                i = idx.u.ival - 1;
                break;
            default:
                goto fallback;
            }

            if (i >= XLENGTH(val) || i < 0)
                goto fallback;

            SEXP res;
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
                break;
            }

            default:
                goto fallback;
            }

            R_Visible = TRUE;
            ostack_popn(ctx, 2);
            ostack_push(ctx, sexp_to_stack_obj(res, true));
            NEXT();

        // ---------
        fallback : {
            PROTECT(val);
            SEXP args =
                CONS_NR(val, CONS_NR(stack_obj_to_sexp(idx), R_NilValue));
            UNPROTECT(1);
            ostack_push(ctx, sexp_to_stack_obj(args, true));
            if (isObject(val)) {
                SEXP call = getSrcAt(c, pc - 1, ctx);
                res = dispatchApply(call, val, args, symbol::DoubleBracket,
                                    *env, ctx);
                if (!res)
                    res = do_subset2_dflt(call, symbol::DoubleBracket, args,
                                          *env);
            } else {
                res = do_subset2_dflt(R_NilValue, symbol::DoubleBracket, args,
                                      *env);
            }
            ostack_popn(ctx, 3);

            R_Visible = TRUE;
            ostack_push(ctx, sexp_to_stack_obj(res, true));
            NEXT();
        }
        }

        // TODO: Fast case
        INSTRUCTION(extract2_2_) {
            SEXP val = stack_obj_to_sexp(ostack_at(ctx, 2));
            SEXP idx = stack_obj_to_sexp(ostack_at(ctx, 1));
            SEXP idx2 = stack_obj_to_sexp(ostack_at(ctx, 0));

            SEXP args = CONS_NR(val, CONS_NR(idx, CONS_NR(idx2, R_NilValue)));
            ostack_push(ctx, sexp_to_stack_obj(args, true));

            SEXP res;
            if (isObject(val)) {
                SEXP call = getSrcForCall(c, pc - 1, ctx);
                res = dispatchApply(call, val, args, symbol::DoubleBracket,
                                    *env, ctx);
                if (!res)
                    res = do_subset2_dflt(call, symbol::DoubleBracket, args,
                                          *env);
            } else {
                res = do_subset2_dflt(R_NilValue, symbol::DoubleBracket, args,
                                      *env);
            }

            ostack_popn(ctx, 4);
            R_Visible = TRUE;
            ostack_push(ctx, sexp_to_stack_obj(res, true));
            NEXT();
        }

        INSTRUCTION(subassign1_1_) {
            SEXP idx = stack_obj_to_sexp(ostack_at(ctx, 0));
            SEXP vec = stack_obj_to_sexp(ostack_at(ctx, 1));
            SEXP val = stack_obj_to_sexp(ostack_at(ctx, 2));

            if (MAYBE_SHARED(vec)) {
                vec = Rf_duplicate(vec);
                ostack_set(ctx, 1, sexp_to_stack_obj(vec, true));
            }

            SEXP args = CONS_NR(vec, CONS_NR(idx, CONS_NR(val, R_NilValue)));
            SET_TAG(CDDR(args), symbol::value);
            PROTECT(args);

            SEXP res = nullptr;
            SEXP call = getSrcForCall(c, pc - 1, ctx);
            RCNTXT assignContext;
            Rf_begincontext(&assignContext, CTXT_RETURN, call, *env,
                            ENCLOS(*env), args, symbol::AssignBracket);
            if (isObject(vec)) {
                res = dispatchApply(call, vec, args, symbol::AssignBracket,
                                    *env, ctx);
            }
            if (!res) {
                res =
                    do_subassign_dflt(call, symbol::AssignBracket, args, *env);
                // We duplicated the vector above, and there is a stvar
                // following
                SET_NAMED(res, 0);
            }
            Rf_endcontext(&assignContext);
            ostack_popn(ctx, 3);
            UNPROTECT(1);

            ostack_push(ctx, sexp_to_stack_obj(res, true));
            NEXT();
        }

        INSTRUCTION(subassign1_2_) {
            SEXP idx2 = stack_obj_to_sexp(ostack_at(ctx, 0));
            SEXP idx1 = stack_obj_to_sexp(ostack_at(ctx, 1));
            SEXP mtx = stack_obj_to_sexp(ostack_at(ctx, 2));
            SEXP val = stack_obj_to_sexp(ostack_at(ctx, 3));

            if (MAYBE_SHARED(mtx)) {
                mtx = Rf_duplicate(mtx);
                ostack_set(ctx, 2, sexp_to_stack_obj(mtx, true));
            }

            SEXP args = CONS_NR(
                mtx, CONS_NR(idx1, CONS_NR(idx2, CONS_NR(val, R_NilValue))));
            SET_TAG(CDDDR(args), symbol::value);
            PROTECT(args);

            SEXP res = nullptr;
            SEXP call = getSrcForCall(c, pc - 1, ctx);
            RCNTXT assignContext;
            Rf_begincontext(&assignContext, CTXT_RETURN, call, *env,
                            ENCLOS(*env), args, symbol::AssignBracket);
            if (isObject(mtx)) {
                res = dispatchApply(call, mtx, args, symbol::AssignBracket,
                                    *env, ctx);
            }

            if (!res) {
                res =
                    do_subassign_dflt(call, symbol::AssignBracket, args, *env);
                // We duplicated the matrix above, and there is a stvar
                // following
                SET_NAMED(res, 0);
            }
            Rf_endcontext(&assignContext);
            ostack_popn(ctx, 4);
            UNPROTECT(1);

            ostack_push(ctx, sexp_to_stack_obj(res, true));
            NEXT();
        }

        INSTRUCTION(subassign2_1_) {
            R_bcstack_t idx = ostack_at(ctx, 0);
            SEXP vec = stack_obj_to_sexp(ostack_at(ctx, 1));
            R_bcstack_t val = ostack_at(ctx, 2);

            // Fast case, only if:
            // 1. vector isn't shared or an object
            // 2. vector is real and shape of value fits into real
            //      or vector is int and shape of value is int
            //      or vector is generic
            // 3. index is numerical and scalar, and in the vector's range
            if (NOT_SHARED(vec) && !isObject(vec)) { // 1
                SEXPTYPE vectorT = TYPEOF(vec);

                if ((vectorT == REALSXP &&
                     (val.tag == STACK_OBJ_INT || val.tag == STACK_OBJ_REAL)) ||
                    (vectorT == INTSXP && val.tag == STACK_OBJ_INT) ||
                    vectorT == VECSXP) { // 2
                    int idx_ = try_stack_obj_to_idx(idx);

                    if (idx_ >= 0 && idx_ < XLENGTH(vec)) {
                        switch (vectorT) {
                        case REALSXP:
                            REAL(vec)
                            [idx_] =
                                val.tag == STACK_OBJ_REAL ? val.u.dval
                                                          : (double)val.u.ival;
                            break;
                        case INTSXP:
                            INTEGER(vec)[idx_] = val.u.ival;
                            break;
                        case VECSXP:
                            PROTECT(vec);
                            SET_VECTOR_ELT(vec, idx_, stack_obj_to_sexp(val));
                            UNPROTECT(1);
                            break;
                        }
                        ostack_popn(ctx, 3);

                        ostack_push(ctx, sexp_to_stack_obj(vec, true));
                        NEXT();
                    }
                }
            }

            if (MAYBE_SHARED(vec)) {
                vec = Rf_duplicate(vec);
                ostack_set(ctx, 1, sexp_to_stack_obj(vec, false));
            }

            PROTECT(vec);
            SEXP idx_sexp = stack_obj_to_sexp(idx);
            PROTECT(idx_sexp);
            SEXP val_sexp = stack_obj_to_sexp(val);
            SEXP args =
                CONS_NR(vec, CONS_NR(idx_sexp, CONS_NR(val_sexp, R_NilValue)));
            SET_TAG(CDDR(args), symbol::value);
            UNPROTECT(2);
            PROTECT(args);

            SEXP res = nullptr;
            SEXP call = getSrcForCall(c, pc - 1, ctx);

            RCNTXT assignContext;
            Rf_begincontext(&assignContext, CTXT_RETURN, call, *env,
                            ENCLOS(*env), args, symbol::AssignDoubleBracket);
            if (isObject(vec)) {
                res = dispatchApply(call, vec, args,
                                    symbol::AssignDoubleBracket, *env, ctx);
            }

            if (!res) {
                res = do_subassign2_dflt(call, symbol::AssignDoubleBracket,
                                         args, *env);
                // We duplicated the vector above, and there is a stvar
                // following
                SET_NAMED(res, 0);
            }
            Rf_endcontext(&assignContext);
            ostack_popn(ctx, 3);
            UNPROTECT(1);

            ostack_push(ctx, sexp_to_stack_obj(res, true));
            NEXT();
        }

        INSTRUCTION(subassign2_2_) {
            R_bcstack_t idx2 = ostack_at(ctx, 0);
            R_bcstack_t idx1 = ostack_at(ctx, 1);
            SEXP mtx = stack_obj_to_sexp(ostack_at(ctx, 2));
            R_bcstack_t val = ostack_at(ctx, 3);

            // Fast case, only if:
            // 1. matrix isn't shared or an object
            // 2. matrix is real and shape of value fits into real
            //      or matrix is int and shape of value is int
            //      or matrix is generic
            // 3. index is numerical and scalar, and in the matrix's 2D range
            if (NOT_SHARED(mtx) && !isObject(mtx)) { // 1
                SEXPTYPE matrixT = TYPEOF(mtx);

                if ((matrixT == REALSXP &&
                     (val.tag == STACK_OBJ_INT || val.tag == STACK_OBJ_REAL)) ||
                    (matrixT == INTSXP && val.tag == STACK_OBJ_INT) ||
                    matrixT == VECSXP) { // 2
                    int idx1_ = try_stack_obj_to_idx(idx1);
                    int idx2_ = try_stack_obj_to_idx(idx2);

                    if (idx1_ >= 0 && idx1_ < Rf_ncols(mtx) && idx2_ >= 0 &&
                        idx2_ < Rf_nrows(mtx)) {
                        int idx_ = idx1_ + (idx2_ * Rf_nrows(mtx));
                        switch (matrixT) {
                        case REALSXP:
                            REAL(mtx)
                            [idx_] =
                                val.tag == STACK_OBJ_REAL ? val.u.dval
                                                          : (double)val.u.ival;
                            break;
                        case INTSXP:
                            INTEGER(mtx)[idx_] = val.u.ival;
                            break;
                        case VECSXP:
                            PROTECT(mtx);
                            SET_VECTOR_ELT(mtx, idx_, stack_obj_to_sexp(val));
                            UNPROTECT(1);
                            break;
                        }
                        ostack_popn(ctx, 4);

                        ostack_push(ctx, sexp_to_stack_obj(mtx, true));
                        NEXT();
                    }
                }
            }

            if (MAYBE_SHARED(mtx)) {
                mtx = Rf_duplicate(mtx);
                ostack_set(ctx, 2, sexp_to_stack_obj(mtx, true));
            }

            PROTECT(mtx);
            SEXP idx1_sexp = stack_obj_to_sexp(idx1);
            PROTECT(idx1_sexp);
            SEXP idx2_sexp = stack_obj_to_sexp(idx2);
            PROTECT(idx2_sexp);
            SEXP val_sexp = stack_obj_to_sexp(val);
            SEXP args = CONS_NR(
                mtx,
                CONS_NR(idx1_sexp,
                        CONS_NR(idx2_sexp, CONS_NR(val_sexp, R_NilValue))));
            SET_TAG(CDDDR(args), symbol::value);
            UNPROTECT(3);
            PROTECT(args);

            SEXP res = nullptr;
            SEXP call = getSrcForCall(c, pc - 1, ctx);
            RCNTXT assignContext;
            Rf_begincontext(&assignContext, CTXT_RETURN, call, *env,
                            ENCLOS(*env), args, symbol::AssignDoubleBracket);
            if (isObject(mtx)) {
                res = dispatchApply(call, mtx, args,
                                    symbol::AssignDoubleBracket, *env, ctx);
            }

            if (!res) {
                res = do_subassign2_dflt(call, symbol::AssignDoubleBracket,
                                         args, *env);
                // We duplicated the matrix above, and there is a stvar
                // following
                SET_NAMED(res, 0);
            }
            Rf_endcontext(&assignContext);
            ostack_popn(ctx, 4);
            UNPROTECT(1);

            ostack_push(ctx, sexp_to_stack_obj(res, true));
            NEXT();
        }

        INSTRUCTION(guard_fun_) {
            SEXP sym = readConst(ctx, readImmediate());
            advanceImmediate();
            SEXP res = readConst(ctx, readImmediate());
            advanceImmediate();
            advanceImmediate();
#ifndef UNSOUND_OPTS
            assert(res == Rf_findFun(sym, *env) && "guard_fun_ fail");
#endif
            NEXT();
        }

        INSTRUCTION(deopt_) {
            SEXP r = readConst(ctx, readImmediate());
            advanceImmediate();
            assert(TYPEOF(r) == RAWSXP);
            assert(XLENGTH(r) >= (int)sizeof(DeoptMetadata));
            auto m = (DeoptMetadata*)DATAPTR(r);
            assert(m->numFrames >= 1);

#if 0
            size_t pos = 0;
            for (size_t i = 0; i < m->numFrames; ++i) {
                std::cout << "Code " << m->frames[i].code << "\n";
                std::cout << "Frame " << i << ":\n";
                std::cout << "  - env\n";
                Rf_PrintValue(ostack_at(ctx, pos++));
                for( size_t j = 0; j < m->frames[i].stackSize; ++j) {
                    std::cout << "  - stack " << j <<"\n";
                    Rf_PrintValue(ostack_at(ctx, pos++));
                }
            }
#endif

            for (size_t i = 1; i < m->numFrames; ++i) {
                if (!synthesizeFrames)
                    synthesizeFrames = new std::deque<FrameInfo*>;
                synthesizeFrames->push_back(&m->frames[i]);
            }

            FrameInfo& f = m->frames[0];
            pc = f.pc;
            c = f.code;
            c->registerInvocation();
            assert(c->code() <= pc && pc < c->endCode());
            R_bcstack_t e = ostack_pop(ctx);
            assert(stack_obj_sexp_type(e) == ENVSXP);
            *env = e.u.sxpval;
            // We need to clear the bindings cache, when we change the
            // environment
            memset(&bindingCache, 0, sizeof(bindingCache));
            NEXT();
        }

        INSTRUCTION(seq_) {
            static SEXP prim = NULL;
            if (!prim) {
                // TODO: we could call seq.default here, but it messes up the
                // error call :(
                prim = Rf_findFun(Rf_install("seq"), R_GlobalEnv);
            }

            // TODO: add a real guard here...
            assert(prim == Rf_findFun(Rf_install("seq"), *env));

            R_bcstack_t from = ostack_at(ctx, 2);
            R_bcstack_t to = ostack_at(ctx, 1);
            R_bcstack_t by = ostack_at(ctx, 0);
            bool has_res = false;
            R_bcstack_t res;

            if (from.tag == STACK_OBJ_INT && to.tag == STACK_OBJ_INT &&
                by.tag == STACK_OBJ_INT) {
                int f = from.u.ival;
                int t = to.u.ival;
                int b = by.u.ival;
                if (f != NA_INTEGER && t != NA_INTEGER && b != NA_INTEGER) {
                    if ((f < t && b > 0) || (t < f && b < 0)) {
                        int size = 1 + (t - f) / b;
                        SEXP res_sexp = Rf_allocVector(INTSXP, size);
                        int v = f;
                        for (int i = 0; i < size; ++i) {
                            INTEGER(res_sexp)[i] = v;
                            v += b;
                        }
                        has_res = true;
                        res = sexp_to_stack_obj(res_sexp, true);
                    } else if (f == t) {
                        has_res = true;
                        res = int_stack_obj(f);
                    }
                }
            }

            if (!has_res) {
                SLOWASSERT(from.tag != STACK_OBJ_SEXP ||
                           !isObject(from.u.sxpval));
                SEXP call = getSrcForCall(c, pc - 1, ctx);
                PROTECT(call);
                SEXP from_sexp = stack_obj_to_sexp(from);
                PROTECT(from_sexp);
                SEXP to_sexp = stack_obj_to_sexp(to);
                PROTECT(to_sexp);
                SEXP by_sexp = stack_obj_to_sexp(by);
                SEXP argslist = CONS_NR(
                    from_sexp, CONS_NR(to_sexp, CONS_NR(by_sexp, R_NilValue)));
                UNPROTECT(3);
                ostack_push(ctx, sexp_to_stack_obj(argslist, true));
                res = sexp_to_stack_obj(
                    Rf_applyClosure(call, prim, argslist, *env, R_NilValue),
                    true);
                ostack_pop(ctx);
            }

            ostack_popn(ctx, 3);
            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(colon_) {
            R_bcstack_t lhs = ostack_at(ctx, 1);
            R_bcstack_t rhs = ostack_at(ctx, 0);
            SEXP res = nullptr;

            if (lhs.tag == STACK_OBJ_INT) {
                int from = lhs.u.ival;
                if (rhs.tag == STACK_OBJ_INT) {
                    int to = rhs.u.ival;
                    if (from != NA_INTEGER && to != NA_INTEGER) {
                        res = seq_int(from, to);
                    }
                } else if (rhs.tag == STACK_OBJ_REAL) {
                    double to = rhs.u.dval;
                    if (from != NA_INTEGER && to != NA_REAL && R_FINITE(to) &&
                        INT_MIN <= to && INT_MAX >= to && to == (int)to) {
                        res = seq_int(from, (int)to);
                    }
                }
            } else if (lhs.tag == STACK_OBJ_REAL) {
                double from = lhs.u.dval;
                if (rhs.tag == STACK_OBJ_INT) {
                    int to = rhs.u.ival;
                    if (from != NA_REAL && to != NA_INTEGER && R_FINITE(from) &&
                        INT_MIN <= from && INT_MAX >= from &&
                        from == (int)from) {
                        res = seq_int((int)from, to);
                    }
                } else if (rhs.tag == STACK_OBJ_REAL) {
                    double to = rhs.u.dval;
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
                STORE_BINOP(sexp_to_stack_obj(res, true));
            }
            NEXT();
        }

        INSTRUCTION(names_) {
            SEXP val = stack_obj_to_sexp(ostack_pop(ctx));
            ostack_push(
                ctx, sexp_to_stack_obj(Rf_getAttrib(val, R_NamesSymbol), true));
            NEXT();
        }

        INSTRUCTION(set_names_) {
            SEXP name = stack_obj_to_sexp(ostack_pop(ctx));
            if (!isNull(name)) {
                PROTECT(name);
                SEXP val = stack_obj_to_sexp(ostack_pop(ctx));
                UNPROTECT(1);
                Rf_setAttrib(val, R_NamesSymbol, name);
                ostack_push(ctx, sexp_to_stack_obj(val, true));
            }
            NEXT();
        }

        INSTRUCTION(alloc_) {
            R_bcstack_t val = ostack_pop(ctx);
            assert(val.tag == STACK_OBJ_INT);
            int type = readSignedImmediate();
            advanceImmediate();
            SEXP res = Rf_allocVector(type, val.u.ival);
            ostack_push(ctx, sexp_to_stack_obj(res, true));
            NEXT();
        }

        INSTRUCTION(length_) {
            R_bcstack_t val = ostack_pop(ctx);
            R_xlen_t len = stack_obj_length(val);
            ostack_push(ctx, int_stack_obj(len));
            NEXT();
        }

        INSTRUCTION(for_seq_size_) {
            R_bcstack_t seq = ostack_at(ctx, 0);
            // TODO: we should extract the length just once at the begining of
            // the loop and generally have somthing more clever here...
            int value;
            if (stack_obj_is_vector(seq)) {
                // TODO: This uses XLENGTH instead of LENGTH. Is that OK?
                value = stack_obj_length(seq);
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
                SEXP seq_sexp = Rf_duplicate(seq.u.sxpval);
                SET_OBJECT(seq.u.sxpval, 0);
                seq = sexp_to_stack_obj(seq_sexp, true);
                ostack_set(ctx, 0, seq);
            }
            ostack_push(ctx, int_stack_obj(value));
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
            R_bcstack_t val = ostack_top(ctx);
            if (val.tag == STACK_OBJ_SEXP) {
                ENSURE_NAMED(val.u.sxpval);
            }
            NEXT();
        }

        INSTRUCTION(set_shared_) {
            R_bcstack_t val = ostack_top(ctx);
            if (val.tag == STACK_OBJ_SEXP) {
                INCREMENT_NAMED(val.u.sxpval);
            }
            NEXT();
        }

        INSTRUCTION(make_unique_) {
            R_bcstack_t val = ostack_top(ctx);
            if (val.tag == STACK_OBJ_SEXP && MAYBE_SHARED(val.u.sxpval)) {
                val =
                    sexp_to_stack_obj(Rf_shallow_duplicate(val.u.sxpval), true);
                ostack_set(ctx, 0, val);
                SET_NAMED(val.u.sxpval, 1);
            }
            NEXT();
        }

        INSTRUCTION(beginloop_) {
            SLOWASSERT(*env);
            int offset = readJumpOffset();
            advanceJump();
            loopTrampoline(c, ctx, env, callCtxt, pc, localsBase);
            pc += offset;
            assert(*pc == Opcode::endloop_);
            advanceOpcode();
            NEXT();
        }

        INSTRUCTION(endloop_) {
            return sexp_to_stack_obj(loopTrampolineMarker, false);
        }

        INSTRUCTION(return_) {
            SEXP res = stack_obj_to_sexp(ostack_top(ctx));
            // this restores stack pointer to the value from the target context
            Rf_findcontext(CTXT_BROWSER | CTXT_FUNCTION, *env, res);
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
    if (synthesizeFrames) {
        while (!synthesizeFrames->empty()) {
            FrameInfo* f = synthesizeFrames->front();
            synthesizeFrames->pop_front();
            R_bcstack_t res = ostack_pop(ctx);
            R_bcstack_t e = ostack_pop(ctx);
            assert(stack_obj_sexp_type(e) == ENVSXP);
            *env = e.u.sxpval;
            ostack_push(ctx, res);
            f->code->registerInvocation();
            res = evalRirCode(f->code, ctx, env, callCtxt, f->pc);
            ostack_push(ctx, res);
        }
        delete synthesizeFrames;
    }
    return ostack_pop(ctx);
}

#pragma GCC diagnostic pop

SEXP evalRirCodeExtCaller(Code* c, Context* ctx, SEXP* env) {
    return stack_obj_to_sexp(evalRirCode(c, ctx, env, nullptr));
}

R_bcstack_t evalRirCode(Code* c, Context* ctx, SEXP* env,
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
        ostack_push(ctx, sexp_to_stack_obj(*arg, true));
        if (arg.hasTag()) {
            names.resize(nargs + 1);
            names[nargs] = Pool::insert(arg.tag());
        }
        nargs++;
    }
    if (!names.empty()) {
        names.resize(nargs);
    }

    CallContext call(nullptr, op, nargs, ast, ostack_cell_at(ctx, nargs - 1),
                     nullptr, names.empty() ? nullptr : names.data(), rho,
                     Assumptions(), ctx);
    call.arglist = arglist;

    auto res = rirCall(call, ctx);
    ostack_popn(ctx, call.passedArgs);
    return res;
}

SEXP rirEval_f(SEXP what, SEXP env) {
    assert(TYPEOF(what) == EXTERNALSXP);

    SEXP lenv = env;
    // TODO: do we not need an RCNTXT here?

    if (auto code = Code::check(what)) {
        return evalRirCodeExtCaller(code, globalContext(), &lenv);
    }

    if (auto table = DispatchTable::check(what)) {
        // TODO: add an adapter frame to be able to call something else than
        // the baseline version!
        Function* fun = table->baseline();
        fun->registerInvocation();

        return evalRirCodeExtCaller(fun->body(), globalContext(), &lenv);
    }

    if (auto fun = Function::check(what)) {
        fun->registerInvocation();
        return evalRirCodeExtCaller(fun->body(), globalContext(), &lenv);
    }

    assert(false && "Expected a code object or a dispatch table");
}
