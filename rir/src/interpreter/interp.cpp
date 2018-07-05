#include <alloca.h>
#include <assert.h>

#include "R/Funtab.h"
#include "interp.h"
#include "interp_context.h"
#include "interpreter/deoptimizer.h"
#include "runtime.h"

#define NOT_IMPLEMENTED assert(false)

#undef eval

extern "C" {
extern SEXP Rf_NewEnvironment(SEXP, SEXP, SEXP);
extern Rboolean R_Visible;
}

// #define UNSOUND_OPTS

// helpers

using namespace rir;

struct CallContext {
    CallContext(Code* c, unsigned callsiteId, SEXP callee, bool hasEagerArgs,
                R_bcstack_t* stackArgs, SEXP callerEnv, Context* ctx)
        : hasEagerArgs(hasEagerArgs), stackArgs(stackArgs),
          caller(c->function()), callerEnv(callerEnv),
          callSite(c->callSite(callsiteId)),
          ast(cp_pool_at(ctx, callSite->call)), callee_(callee) {
        assert(!callee || TYPEOF(callee) == CLOSXP ||
               TYPEOF(callee) == SPECIALSXP || TYPEOF(callee) == BUILTINSXP);
    }

    CallContext(Code* c, unsigned id, SEXP callee, SEXP callerEnv, Context* ctx)
        : CallContext(c, id, callee, false, nullptr, callerEnv, ctx) {}

    CallContext(Code* c, unsigned id, bool hasEagerArgs, R_bcstack_t* stackArgs,
                SEXP callerEnv, Context* ctx)
        : CallContext(c, id, nullptr, hasEagerArgs, stackArgs, callerEnv, ctx) {
    }

    CallContext(Code* c, unsigned id, SEXP callerEnv, Context* ctx)
        : CallContext(c, id, nullptr, false, nullptr, callerEnv, ctx) {}

    bool hasEagerArgs;
    R_bcstack_t* stackArgs;
    Function* caller;
    SEXP callerEnv;
    CallSite* callSite;
    SEXP ast;

    unsigned nargs() const { return callSite->nargs; }

    bool hasStackArgs() const { return stackArgs != nullptr; }
    bool hasEagerCallee() const { return TYPEOF(callee()) == BUILTINSXP; }

    SEXP callee() const {
        assert(callee_);
        return callee_;
    }

    CallContext resolveTarget(SEXP newCallee) const {
        assert(TYPEOF(newCallee) == CLOSXP || TYPEOF(newCallee) == SPECIALSXP ||
               TYPEOF(newCallee) == BUILTINSXP);
        auto c = *this;
        c.callee_ = newCallee;
        return c;
    }

    Code* arg(unsigned i) const {
        assert(i < nargs());
        return caller->codeAt(callSite->args()[i]);
    }

    SEXP stackArg(unsigned i) const {
        assert(stackArgs && i < nargs());
        return ostack_at_cell(stackArgs + i);
    }

    SEXP getFixedupAst() const;

    CallContext fixupAst() const {
        auto copy = *this;
        copy.ast = getFixedupAst();
        return copy;
    }

  protected:
    SEXP callee_;
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

#define advanceOpcode() (*pc++)
#define readImmediate() (*(Immediate*)pc)
#define readSignedImmediate() (*(SignedImmediate*)pc)
#define readJumpOffset() (*(JumpOffset*)(pc))
#define advanceImmediate() pc += sizeof(Immediate)
#define advanceJump() pc += sizeof(JumpOffset)

#define readConst(ctx, idx) (cp_pool_at(ctx, idx))

void initClosureContext(const CallContext& call, RCNTXT* cntxt, SEXP rho,
                        SEXP sysparent, SEXP arglist, SEXP op) {
    /*  If we have a generic function we need to use the sysparent of
       the generic as the sysparent of the method because the method
       is a straight substitution of the generic.  */

    if (R_GlobalContext->callflag == CTXT_GENERIC)
        Rf_begincontext(cntxt, CTXT_RETURN, call.ast, rho,
                        R_GlobalContext->sysparent, arglist, op);
    else
        Rf_begincontext(cntxt, CTXT_RETURN, call.ast, rho, sysparent, arglist,
                        op);
}

void endClosureContext(RCNTXT* cntxt, SEXP result) {
    cntxt->returnValue = result;
    Rf_endcontext(cntxt);
}

RIR_INLINE SEXP createPromise(Code* code, SEXP env) {
    SEXP p = Rf_mkPROMISE((SEXP)code, env);
    return p;
}

RIR_INLINE SEXP promiseValue(SEXP promise, Context* ctx) {
    // if already evaluated, return the value
    if (PRVALUE(promise) && PRVALUE(promise) != R_UnboundValue) {
        promise = PRVALUE(promise);
        assert(TYPEOF(promise) != PROMSXP);
        SET_NAMED(promise, 2);
        return promise;
    } else {
        SEXP res = forcePromise(promise);
        assert(TYPEOF(res) != PROMSXP && "promise returned promise");
        return res;
    }
}

static void jit(SEXP cls, Context* ctx) {
    assert(TYPEOF(cls) == CLOSXP);
    if (TYPEOF(BODY(cls)) == EXTERNALSXP)
        return;
    SEXP cmp = ctx->compiler(cls, NULL);
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

    auto cs = call.callSite;
    bool hasNames = cs->hasNames;

    for (size_t i = 0; i < cs->nargs; ++i) {

        SEXP name = hasNames ? cp_pool_at(ctx, cs->names()[i]) : R_NilValue;

        SEXP arg = call.stackArg(i);

        if (call.hasEagerArgs && !eagerCallee &&
            (arg == R_MissingArg || arg == R_DotsSymbol)) {
            // We have to wrap them in a promise, otherwise they are treated
            // as expressions to be evaluated, when in fact they are meant to be
            // asts as values
            SEXP promise = Rf_mkPROMISE(arg, call.callerEnv);
            SET_PRVALUE(promise, arg);
            __listAppend(&result, &pos, promise, R_NilValue);
        } else {
            if (eagerCallee && TYPEOF(arg) == PROMSXP) {
                arg = Rf_eval(arg, call.callerEnv);
            }
            __listAppend(&result, &pos, arg, name);
        }
    }

    if (result != R_NilValue)
        UNPROTECT(1);

    return result;
}

SEXP createLegacyArgsList(const CallContext& call, bool eagerCallee,
                          Context* ctx) {
    SEXP result = R_NilValue;
    SEXP pos = result;
    auto cs = call.callSite;

    // loop through the arguments and create a promise, unless it is a missing
    // argument
    bool hasNames = cs->hasNames;

    for (size_t i = 0; i < cs->nargs; ++i) {
        unsigned argi = cs->args()[i];
        SEXP name = hasNames ? cp_pool_at(ctx, cs->names()[i]) : R_NilValue;

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
                SEXP arg = evalRirCodeExtCaller(call.arg(i), ctx, &env);
                assert(TYPEOF(arg) != PROMSXP);
                __listAppend(&result, &pos, arg, name);
            } else {
                Code* arg = call.arg(i);
                SEXP promise = createPromise(arg, call.callerEnv);
                __listAppend(&result, &pos, promise, name);
            }
        }
    }

    if (result != R_NilValue)
        UNPROTECT(1);
    return result;
}

SEXP createLegacyLazyArgsList(const CallContext& call, Context* ctx) {
    if (call.hasStackArgs()) {
        return createLegacyArgsListFromStackValues(call, false, ctx);
    } else {
        return createLegacyArgsList(call, false, ctx);
    }
}

SEXP createLegacyArgsList(const CallContext& call, Context* ctx) {
    if (call.hasStackArgs()) {
        return createLegacyArgsListFromStackValues(call, call.hasEagerCallee(),
                                                   ctx);
    } else {
        return createLegacyArgsList(call, call.hasEagerCallee(), ctx);
    }
}

SEXP rirCallTrampoline(const CallContext& call, Function* fun, SEXP env,
                       SEXP arglist, R_bcstack_t* stackArgs, Context* ctx) {
    auto trampoline = [](RCNTXT* cntxt, const CallContext* call, Code* code,
                         SEXP* env, R_bcstack_t* stackArgs, Context* ctx) {
        int trampIn = ostack_length(ctx);
        if ((SETJMP(cntxt->cjmpbuf))) {
            assert(trampIn == ostack_length(ctx));
            if (R_ReturnedValue == R_RestartToken) {
                cntxt->callflag = CTXT_RETURN; /* turn restart off */
                R_ReturnedValue = R_NilValue;  /* remove restart token */
                return evalRirCode(code, ctx, env, call);
            } else {
                return R_ReturnedValue;
            }
        }
        return evalRirCode(code, ctx, env, call);
    };

    RCNTXT cntxt;

    initClosureContext(call, &cntxt, env, call.callerEnv, arglist,
                       call.callee());
    closureDebug(call.ast, call.callee(), env, R_NilValue, &cntxt);

    // Warning: call.popArgs() between initClosureContext and trampoline will
    // result in broken stack on non-local returns.

    Code* code = fun->body();
    // Pass &cntxt.cloenv, to let evalRirCode update the env of the current
    // context
    SEXP result =
        trampoline(&cntxt, &call, code, &cntxt.cloenv, stackArgs, ctx);
    PROTECT(result);

    endClosureDebug(call.ast, call.callee(), env);
    endClosureContext(&cntxt, result);

    UNPROTECT(1);
    return result;
}

SEXP rirCallTrampoline(const CallContext& call, Function* fun, Context* ctx) {
    return rirCallTrampoline(call, fun, R_NilValue, R_NilValue, call.stackArgs,
                             ctx);
}

SEXP rirCallTrampoline(const CallContext& call, Function* fun, SEXP env,
                       SEXP arglist, Context* ctx) {
    return rirCallTrampoline(call, fun, env, arglist, nullptr, ctx);
}

void warnSpecial(SEXP callee, SEXP call) {
    // TODO: turn this on?
    return;

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
}

/** Performs the call.

  TODO this is currently super simple.

 */

void doProfileCall(CallSite*, SEXP);
RIR_INLINE void profileCall(const CallContext& call, SEXP callee) {
    if (!call.callSite->hasProfile)
        return;
    doProfileCall(call.callSite, callee);
}
RIR_INLINE void profileCall(const CallContext& call) {
    return profileCall(call, call.callee());
}

void doProfileCall(CallSite* cs, SEXP callee) {
    CallSiteProfile* p = cs->profile();
    if (!p->takenOverflow) {
        if (p->taken + 1 == CallSiteProfile_maxTaken)
            p->takenOverflow = true;
        else
            p->taken++;
    }
    if (!p->targetsOverflow) {
        if (p->numTargets + 1 == CallSiteProfile_maxTargets) {
            p->targetsOverflow = true;
        } else {
            int i = 0;
            for (; i < p->numTargets; ++i)
                if (p->targets[i] == callee)
                    break;
            if (i == p->numTargets)
                p->targets[p->numTargets++] = callee;
        }
    }
}

SEXP CallContext::getFixedupAst() const {
    SEXP ast = this->ast;
    assert(hasEagerArgs);
    // This is a hack to support complex assignment's rewritten asts for
    // getters and setters.
    // The rewritten ast has target (and value for setters) marked as
    // placeholders, which we need to fill in here.
    if ((CADR(ast) == getterPlaceholderSym ||
         CADR(ast) == setterPlaceholderSym)) {
        int setter = CADR(ast) == setterPlaceholderSym;
        ast = Rf_shallow_duplicate(ast);
        PROTECT(ast);

        SEXP a = CDR(ast);

        SEXP target = ostack_at(ctx, nargs() - 1);

        if (target == R_MissingArg) {
            assert(!setter);
            SETCDR(ast, R_NilValue);
            UNPROTECT(1);
            return ast;
        }

        SEXP p = target;
        // It might be tempting to put the values as consts into the ast, but
        // then they are converted to consts (named = 2) which is bad.
        // therefore we wrap them in fake promises.
        if (TYPEOF(p) != PROMSXP) {
            p = Rf_mkPROMISE(getterPlaceholderSym, R_NilValue);
            SET_PRVALUE(p, target);
        }

        SETCAR(a, p);

        if (setter) {
            SEXP prev = ast;
            while (CDR(a) != R_NilValue) {
                prev = a;
                a = CDR(a);
            }

            assert(CAR(a) == setterPlaceholderSym);
            SEXP val = ostack_top(ctx);

            SEXP p = val;
            if (TYPEOF(p) != PROMSXP) {
                p = Rf_mkPROMISE(setterPlaceholderSym, R_NilValue);
                SET_PRVALUE(p, val);
            }

            SEXP v = CONS_NR(p, R_NilValue);
            SET_TAG(v, R_valueSym);
            SETCDR(prev, v);
        }
        UNPROTECT(1);
    }
    return ast;
}

SEXP legacySpecialCall(const CallContext& call, Context* ctx) {
    assert(call.ast != R_NilValue);

    // get the ccode
    CCODE f = getBuiltin(call.callee());
    int flag = getFlag(call.callee());
    R_Visible = static_cast<Rboolean>(flag != 1);
    // call it with the AST only
    SEXP result = f(call.ast, call.callee(), CDR(call.ast), call.callerEnv);
    if (flag < 2)
        R_Visible = static_cast<Rboolean>(flag != 1);
    return result;
}

SEXP legacyCallWithArgslist(const CallContext& call, SEXP argslist,
                            Context* ctx) {
    if (TYPEOF(call.callee()) == BUILTINSXP) {
        // get the ccode
        CCODE f = getBuiltin(call.callee());
        int flag = getFlag(call.callee());
        if (flag < 2)
            R_Visible = static_cast<Rboolean>(flag != 1);
        // call it
        SEXP result = f(call.ast, call.callee(), argslist, call.callerEnv);
        if (flag < 2)
            R_Visible = static_cast<Rboolean>(flag != 1);
        return result;
    }

    assert(TYPEOF(call.callee()) == CLOSXP &&
           TYPEOF(BODY(call.callee())) != EXTERNALSXP);
    return Rf_applyClosure(call.ast, call.callee(), argslist, call.callerEnv,
                           R_NilValue);
}

SEXP legacyCall(const CallContext& call, Context* ctx) {
    // create the argslist
    SEXP argslist = createLegacyArgsList(call, ctx);
    PROTECT(argslist);
    SEXP res = legacyCallWithArgslist(call, argslist, ctx);
    UNPROTECT(1);
    return res;
}

SEXP closureArgumentAdaptor(const CallContext& call, SEXP arglist,
                            SEXP suppliedvars) {
    SEXP op = call.callee();
    if (FORMALS(op) == R_NilValue && arglist == R_NilValue)
        return Rf_NewEnvironment(R_NilValue, R_NilValue, CLOENV(op));

    /*  Set up a context with the call in it so error has access to it */
    RCNTXT cntxt;
    initClosureContext(call, &cntxt, CLOENV(op), call.callerEnv, arglist, op);

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
    // (or end() if no such exist)
    Function* fun = DispatchTable::unpack(BODY(op))->first();
    Code* c = findDefaultArgument(fun->first());
    Code* e = fun->codeEnd();
    while (f != R_NilValue) {
        if (CAR(f) != R_MissingArg) {
            if (CAR(a) == R_MissingArg) {
                assert(c != e && "No more compiled formals available.");
                SETCAR(a, createPromise(c, newrho));
                SET_MISSING(a, 2);
            }
            // Either just used the compiled formal or it was not needed.
            // Skip to next Code (at least the body Code is always there),
            // then find the following compiled formal
            c = findDefaultArgument(c->next());
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

unsigned dispatch(const CallContext& call, DispatchTable* vt) {
    assert(vt->capacity() > 0);
    if (vt->capacity() == 1 || !vt->available(1))
        return 0;

    // Try to dispatch to slot 1
    if (call.callSite->hasNames ||
        /* TODO: length is waay slow. Will be fixed by signatures */
        call.nargs() != (size_t)Rf_length(FORMALS(call.callee()))) {
        return 0;
    }

    return 1;
};

// Call a RIR function, when we already have created the list of actuals (this
// is for example the case, if we tried to dispatch).
SEXP rirCall(const CallContext& call, SEXP actuals, Context* ctx) {
    assert(actuals);
    SEXP body = BODY(call.callee());
    assert(isValidDispatchTableSEXP(body));

    auto table = DispatchTable::unpack(body);

    unsigned slot = dispatch(call, table);
    bool needsEnv = slot == 0;

    Function* fun = table->at(slot);
    fun->registerInvocation();

    SEXP env = R_NilValue;

    SEXP result = nullptr;
    if (needsEnv) {
        env = closureArgumentAdaptor(call, actuals, R_NilValue);
        result = rirCallTrampoline(call, fun, env, actuals, ctx);
    } else {
        result = rirCallTrampoline(call, fun, ctx);
    }

    assert(result);

    if (env) {
        if (!fun->envLeaked && FRAME_LEAKED(env))
            fun->envLeaked = true;
        if (!fun->envChanged && FRAME_CHANGED(env))
            fun->envChanged = true;
    }

    assert(!fun->deopt);
    return result;
}

// Call a RIR function. Arguments are still untouched.
SEXP rirCall(const CallContext& call, Context* ctx) {
    SEXP body = BODY(call.callee());
    assert(isValidDispatchTableSEXP(body));

    auto table = DispatchTable::unpack(body);

    unsigned slot = dispatch(call, table);
    bool needsEnv = slot == 0;

    Function* fun = table->at(slot);
    fun->registerInvocation();

    SEXP env = R_NilValue;

    SEXP result = nullptr;
    if (needsEnv) {
        auto arglist = createLegacyLazyArgsList(call, ctx);
        PROTECT(arglist);
        env = closureArgumentAdaptor(call, arglist, R_NilValue);
        result = rirCallTrampoline(call, fun, env, arglist, ctx);
        UNPROTECT(1);
    } else {
        result = rirCallTrampoline(call, fun, ctx);
    }

    assert(result);

    if (env) {
        if (!fun->envLeaked && FRAME_LEAKED(env))
            fun->envLeaked = true;
        if (!fun->envChanged && FRAME_CHANGED(env))
            fun->envChanged = true;
    }

    assert(!fun->deopt);
    return result;
}

SEXP doCall(const CallContext& call, Context* ctx) {
    assert(call.callee());

    profileCall(call);

    auto apply = [&](const CallContext& call) {
        switch (TYPEOF(call.callee())) {
        case SPECIALSXP:
            return legacySpecialCall(call, ctx);
        case BUILTINSXP:
            return legacyCall(call, ctx);
        case CLOSXP: {
            if (TYPEOF(BODY(call.callee())) != EXTERNALSXP)
                return legacyCall(call, ctx);
            return rirCall(call, ctx);
        }
        default:
            Rf_error("Invalid Callee");
        };
        return R_NilValue;
    };

    if (call.hasEagerArgs) {
        CallContext fcall = call.fixupAst();
        PROTECT(fcall.ast);
        auto res = apply(fcall);
        UNPROTECT(1);
        return res;
    }
    return apply(call);
}

SEXP dispatchApply(const CallContext& call, SEXP obj, SEXP actuals,
                   Context* ctx);

SEXP doDispatch(const CallContext& call, Context* ctx) {
    SEXP obj = call.hasStackArgs() ? ostack_at(ctx, call.nargs() - 1)
                                   : ostack_top(ctx);
    assert(isObject(obj));

    profileCall(call, Rf_install("*dispatch*"));

    if (call.hasStackArgs()) {
        auto fcall = call.hasEagerArgs ? call.fixupAst() : call;
        PROTECT(fcall.ast);
        SEXP actuals = createLegacyLazyArgsList(call, ctx);
        PROTECT(actuals);
        SEXP result = dispatchApply(fcall, obj, actuals, ctx);
        UNPROTECT(2);
        return result;
    }

    SEXP actuals = createLegacyLazyArgsList(call, ctx);
    PROTECT(actuals);

    // Patch the already evaluated object into the first entry of
    // the promise args list
    SET_PRVALUE(CAR(actuals), obj);
    SEXP result = dispatchApply(call, obj, actuals, ctx);
    UNPROTECT(1);
    return result;
}

SEXP dispatchApply(const CallContext& call, SEXP obj, SEXP actuals,
                   Context* ctx) {
    CallSite* cs = call.callSite;
    SEXP ast = call.ast;
    SEXP selector = cp_pool_at(ctx, *cs->selector());
    SEXP op = SYMVALUE(selector);

    // ===============================================
    // First try S4
    if (IS_S4_OBJECT(obj) && R_has_methods(op)) {
        SEXP result =
            R_possible_dispatch(ast, op, actuals, call.callerEnv, TRUE);
        if (result)
            return result;
    }

    // ===============================================
    // Then try S3
    const char* generic = CHAR(PRINTNAME(selector));
    SEXP rho1 = Rf_NewEnvironment(R_NilValue, R_NilValue, call.callerEnv);
    PROTECT(rho1);
    RCNTXT cntxt;
    initClosureContext(call, &cntxt, rho1, call.callerEnv, actuals, op);
    SEXP result;
    bool success = Rf_usemethod(generic, obj, ast, actuals, rho1,
                                call.callerEnv, R_BaseEnv, &result);
    UNPROTECT(1);
    endClosureContext(&cntxt, success ? result : R_NilValue);
    if (success)
        return result;

    // ===============================================
    // Now normal dispatch
    SEXP callee = Rf_findFun(selector, call.callerEnv);

    // TODO something should happen here
    if (callee == R_UnboundValue)
        assert(false && "Unbound var");
    if (callee == R_MissingArg)
        assert(false && "Missing argument");

    auto fcall = call.resolveTarget(callee);

    switch (TYPEOF(fcall.callee())) {
    case SPECIALSXP:
        return legacySpecialCall(fcall, ctx);
    case BUILTINSXP:
        // force all promises in the args list. We created a lazy legacy args
        // list, since we did not know the callee yet. So now we know and need
        // to make it eager. We use Rf_eval here, since in some circumstances
        // non-rir promises might be in the list.
        for (SEXP a = actuals; a != R_NilValue; a = CDR(a))
            SETCAR(a, Rf_eval(CAR(a), call.callerEnv));
        return legacyCallWithArgslist(fcall, actuals, ctx);
    case CLOSXP:
        if (TYPEOF(BODY(fcall.callee())) != EXTERNALSXP)
            return legacyCallWithArgslist(fcall, actuals, ctx);
        return rirCall(fcall, actuals, ctx);
    }

    assert(false && "Don't know how to run other stuff");
    return R_NilValue;
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
        SEXP call = getSrcForCall(c, pc - 1, ctx);                             \
        SEXP argslist = CONS_NR(lhs, CONS_NR(rhs, R_NilValue));                \
        ostack_push(ctx, argslist);                                            \
        if (flag < 2)                                                          \
            R_Visible = static_cast<Rboolean>(flag != 1);                      \
        res = blt(call, prim, argslist, getenv());                             \
        if (flag < 2)                                                          \
            R_Visible = static_cast<Rboolean>(flag != 1);                      \
        ostack_pop(ctx);                                                       \
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
        res = ostack_at(ctx, 1);                                               \
        if (TYPEOF(res) != res_type || !NO_REFERENCES(res)) {                  \
            res = allocVector(res_type, 1);                                    \
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
        } else {                                                               \
            BINOP_FALLBACK(#op);                                               \
        }                                                                      \
        ostack_pop(ctx);                                                       \
        ostack_set(ctx, 0, res);                                               \
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
        warning("probable complete loss of accuracy in modulus");
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
        res = blt(call, prim, argslist, getenv());                             \
        if (flag < 2)                                                          \
            R_Visible = static_cast<Rboolean>(flag != 1);                      \
        ostack_pop(ctx);                                                       \
    } while (false)

#define DO_UNOP(op, op2)                                                       \
    do {                                                                       \
        if (IS_SIMPLE_SCALAR(val, REALSXP)) {                                  \
            res = Rf_allocVector(REALSXP, 1);                                  \
            *REAL(res) = (*REAL(val) == NA_REAL) ? NA_REAL : op * REAL(val);   \
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

RIR_INLINE SEXP findRootPromise(SEXP p) {
    if (TYPEOF(p) == PROMSXP) {
        while (TYPEOF(PREXPR(p)) == PROMSXP) {
            p = PREXPR(p);
        }
    }
    return p;
}

extern void printCode(Code* c);
extern void printFunction(Function* f);

extern SEXP Rf_deparse1(SEXP call, Rboolean abbrev, int opts);

RIR_INLINE void incPerfCount(Code* c) {
    if (c->perfCounter < UINT_MAX) {
        c->perfCounter++;
        // if (c->perfCounter == 200000)
        //     printCode(c);
    }
}

static int debugging = 0;
void debug(Code* c, Opcode* pc, const char* name, unsigned depth,
           Context* ctx) {
    return;
    if (debugging == 0) {
        debugging = 1;
        printf("%p : %d, %s, s: %d\n", c, (int)*pc, name, depth);
        for (unsigned i = 0; i < depth; ++i) {
            printf("%3d: ", i);
            Rf_PrintValue(ostack_at(ctx, i));
        }
        printf("\n");
        debugging = 0;
    }
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
                         BindingCache* bindingCache) {
    SEXP loc = cachedGetBindingCell(env, idx, ctx, bindingCache);
    if (loc && !BINDING_IS_LOCKED(loc) && !IS_ACTIVE_BINDING(loc)) {
        SEXP cur = CAR(loc);
        if (cur == val)
            return;
        INCREMENT_NAMED(val);
        SETCAR(loc, val);
        if (MISSING(loc))
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

SEXP evalRirCodeExtCaller(Code* c, Context* ctx, SEXP* env) {
    return evalRirCode(c, ctx, env, nullptr);
}

SEXP evalRirCode(Code* c, Context* ctx, SEXP* env,
                 const CallContext* callCtxt) {
    assert(*env);
    assert((*env != R_NilValue) || (callCtxt != nullptr));

    extern int R_PPStackTop;

#ifdef THREADED_CODE
    static void* opAddr[static_cast<uint8_t>(Opcode::num_of)] = {
#define DEF_INSTR(name, ...) (__extension__ && op_##name),
#include "ir/insns.h"
#undef DEF_INSTR
    };
#endif

    assert(c->magic == CODE_MAGIC);

    Locals locals(c->localsCount);

    BindingCache bindingCache[BINDING_CACHE_SIZE];
    memset(&bindingCache, 0, sizeof(bindingCache));

    // make sure there is enough room on the stack
    // there is some slack of 5 to make sure the call instruction can store
    // some intermediate values on the stack
    ostack_ensureSize(ctx, c->stackLength + 5);

    Opcode* pc = c->code();
    SEXP res;

    R_Visible = TRUE;

    auto getenv = [&env]() -> SEXP {
        assert(*env && *env != R_NilValue);
        return *env;
    };

    // main loop
    BEGIN_MACHINE {

        INSTRUCTION(invalid_) assert(false && "wrong or unimplemented opcode");

        INSTRUCTION(nop_) NEXT();

        INSTRUCTION(make_env_) {
            SEXP parent = ostack_pop(ctx);
            assert(TYPEOF(parent) == ENVSXP &&
                   "Non-environment used as environment parent.");
            res = Rf_NewEnvironment(R_NilValue, R_NilValue, parent);
            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(caller_env_) {
            ostack_push(ctx, callCtxt->callerEnv);
            NEXT();
        }

        INSTRUCTION(get_env_) {
            assert(env);
            ostack_push(ctx, getenv());
            NEXT();
        }

        INSTRUCTION(set_env_) {
            SEXP e = ostack_pop(ctx);
            assert(TYPEOF(e) == ENVSXP && "Expected an environment on TOS.");
            *env = e;
            NEXT();
        }

        INSTRUCTION(ldfun_) {
            SEXP sym = readConst(ctx, readImmediate());
            advanceImmediate();
            res = Rf_findFun(sym, getenv());

            // TODO something should happen here
            if (res == R_UnboundValue)
                assert(false && "Unbound var");
            if (res == R_MissingArg)
                assert(false && "Missing argument");

            switch (TYPEOF(res)) {
            case CLOSXP:
                jit(res, ctx);
                break;
            case SPECIALSXP:
            case BUILTINSXP:
                // special and builtin functions are ok
                break;
            default:
                error("attempt to apply non-function");
            }
            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(ldvar_) {
            Immediate id = readImmediate();
            advanceImmediate();
            res = cachedGetVar(getenv(), id, ctx, bindingCache);
            R_Visible = TRUE;

            if (res == R_UnboundValue) {
                Rf_error("object not found");
            } else if (res == R_MissingArg) {
                SEXP sym = cp_pool_at(ctx, id);
                Rf_error("argument \"%s\" is missing, with no default",
                         CHAR(PRINTNAME(sym)));
            }

            // if promise, evaluate & return
            if (TYPEOF(res) == PROMSXP)
                res = promiseValue(res, ctx);

            if (NAMED(res) == 0 && res != R_NilValue)
                SET_NAMED(res, 1);

            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(ldvar_noforce_) {
            Immediate id = readImmediate();
            advanceImmediate();
            res = cachedGetVar(getenv(), id, ctx, bindingCache);
            R_Visible = TRUE;

            if (res == R_UnboundValue) {
                Rf_error("object not found");
            }

            if (NAMED(res) == 0 && res != R_NilValue)
                SET_NAMED(res, 1);

            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(ldvar_super_) {
            SEXP sym = readConst(ctx, readImmediate());
            advanceImmediate();
            res = Rf_findVar(sym, ENCLOS(getenv()));
            R_Visible = TRUE;

            if (res == R_UnboundValue) {
                Rf_error("object not found");
            } else if (res == R_MissingArg) {
                Rf_error("argument \"%s\" is missing, with no default",
                         CHAR(PRINTNAME(res)));
            }

            // if promise, evaluate & return
            if (TYPEOF(res) == PROMSXP)
                res = promiseValue(res, ctx);

            if (NAMED(res) == 0 && res != R_NilValue)
                SET_NAMED(res, 1);

            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(ldvar_noforce_super_) {
            SEXP sym = readConst(ctx, readImmediate());
            advanceImmediate();
            res = Rf_findVar(sym, ENCLOS(getenv()));
            R_Visible = TRUE;

            if (res == R_UnboundValue) {
                Rf_error("object not found");
            }

            if (NAMED(res) == 0 && res != R_NilValue)
                SET_NAMED(res, 1);

            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(ldddvar_) {
            SEXP sym = readConst(ctx, readImmediate());
            advanceImmediate();
            res = Rf_ddfindVar(sym, getenv());
            R_Visible = TRUE;

            // TODO better errors
            if (res == R_UnboundValue) {
                Rf_error("object not found");
            } else if (res == R_MissingArg) {
                error("argument is missing, with no default");
            }

            // if promise, evaluate & return
            if (TYPEOF(res) == PROMSXP)
                res = promiseValue(res, ctx);

            if (NAMED(res) == 0 && res != R_NilValue)
                SET_NAMED(res, 1);

            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(ldlval_) {
            Immediate id = readImmediate();
            advanceImmediate();
            res = cachedGetBindingCell(getenv(), id, ctx, bindingCache);
            assert(res);
            res = CAR(res);
            assert(res != R_UnboundValue);

            R_Visible = TRUE;

            if (TYPEOF(res) == PROMSXP)
                res = PRVALUE(res);

            assert(res != R_UnboundValue);
            assert(res != R_MissingArg);

            if (NAMED(res) == 0 && res != R_NilValue)
                SET_NAMED(res, 1);

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
                Code* arg = callCtxt->arg(idx);
                res = createPromise(arg, callCtxt->callerEnv);
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
            int wasChanged = FRAME_CHANGED(getenv());
            SEXP val = ostack_pop(ctx);

            cachedSetVar(val, getenv(), id, ctx, bindingCache);

            if (!wasChanged)
                CLEAR_FRAME_CHANGED(getenv());
            NEXT();
        }

        INSTRUCTION(stvar_super_) {
            SEXP sym = readConst(ctx, readImmediate());
            advanceImmediate();
            SLOWASSERT(TYPEOF(sym) == SYMSXP);
            SEXP val = ostack_pop(ctx);
            INCREMENT_NAMED(val);
            Rf_setVar(sym, val, ENCLOS(getenv()));
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

        INSTRUCTION(call_) {
            auto lll = ostack_length(ctx);
            int ttt = R_PPStackTop;

            // Callee is TOS
            // Arguments are immediate (in the CallSite struct), given as
            // promise code indices.
            Immediate id = readImmediate();
            advanceImmediate();
            advanceImmediate(); // nargs, TODO: remove
            CallContext call(c, id, ostack_top(ctx), getenv(), ctx);
            res = doCall(call, ctx);
            ostack_pop(ctx); // callee
            ostack_push(ctx, res);

            assert(ttt == R_PPStackTop);
            assert(lll == ostack_length(ctx));
            NEXT();
        }

        INSTRUCTION(call_stack_eager_) {
            auto lll = ostack_length(ctx);
            int ttt = R_PPStackTop;

            // Stack contains [callee, arg1, ..., argn]
            Immediate id = readImmediate();
            advanceImmediate();
            Immediate n = readImmediate();
            advanceImmediate();
            CallContext call(c, id, ostack_at(ctx, n), true,
                             ostack_cell_at(ctx, n - 1), getenv(), ctx);
            res = doCall(call, ctx);
            ostack_popn(ctx, n + 1);
            ostack_push(ctx, res);

            assert(ttt == R_PPStackTop);
            assert(lll - call.nargs() == ostack_length(ctx));
            NEXT();
        }

        INSTRUCTION(static_call_stack_eager_) {
            auto lll = ostack_length(ctx);
            int ttt = R_PPStackTop;

            // Stack contains [arg1, ..., argn], callee is immediate
            Immediate id = readImmediate();
            advanceImmediate();
            Immediate n = readImmediate();
            advanceImmediate();
            SEXP callee = cp_pool_at(ctx, *c->callSite(id)->target());
            CallContext call(c, id, callee, true, ostack_cell_at(ctx, n - 1),
                             getenv(), ctx);
            res = doCall(call, ctx);
            ostack_popn(ctx, n);
            ostack_push(ctx, res);

            assert(ttt == R_PPStackTop);
            assert(lll - call.nargs() + 1 == ostack_length(ctx));
            NEXT();
        }

        INSTRUCTION(call_stack_promised_) {
            auto lll = ostack_length(ctx);
            int ttt = R_PPStackTop;

            // Stack contains [callee, arg1, ..., argn]
            Immediate id = readImmediate();
            advanceImmediate();
            Immediate n = readImmediate();
            advanceImmediate();
            CallContext call(c, id, ostack_at(ctx, n), false,
                             ostack_cell_at(ctx, n - 1), getenv(), ctx);
            res = doCall(call, ctx);
            ostack_popn(ctx, n + 1);
            ostack_push(ctx, res);

            assert(ttt == R_PPStackTop);
            assert(lll - call.nargs() == ostack_length(ctx));
            NEXT();
        }

        INSTRUCTION(static_call_stack_promised_) {
            auto lll = ostack_length(ctx);
            int ttt = R_PPStackTop;

            // Stack contains [arg1, ..., argn], callee is immediate
            Immediate id = readImmediate();
            advanceImmediate();
            Immediate n = readImmediate();
            advanceImmediate();
            SEXP callee = cp_pool_at(ctx, *c->callSite(id)->target());
            CallContext call(c, id, callee, false, ostack_cell_at(ctx, n - 1),
                             getenv(), ctx);
            res = doCall(call, ctx);
            ostack_popn(ctx, n);
            ostack_push(ctx, res);

            assert(ttt == R_PPStackTop);
            assert(lll - call.nargs() + 1 == ostack_length(ctx));
            NEXT();
        }

        INSTRUCTION(dispatch_) {
            auto lll = ostack_length(ctx);
            int ttt = R_PPStackTop;

            // TOS is receiver object
            // Arguments are immediate (in the CallSite struct), given as
            // promise code indices.
            Immediate id = readImmediate();
            advanceImmediate();
            advanceImmediate(); // nargs, TODO: remove
            CallContext call(c, id, getenv(), ctx);
            res = doDispatch(call, ctx);
            ostack_pop(ctx); // receiver obj
            ostack_push(ctx, res);

            assert(ttt == R_PPStackTop);
            assert(lll == ostack_length(ctx));
            NEXT();
        }

        INSTRUCTION(dispatch_stack_eager_) {
            auto lll = ostack_length(ctx);
            int ttt = R_PPStackTop;

            // Stack contains [receiver obj (aka arg1), arg2, ..., argn]
            Immediate id = readImmediate();
            advanceImmediate();
            Immediate n = readImmediate();
            advanceImmediate();
            CallContext call(c, id, true, ostack_cell_at(ctx, n - 1), getenv(),
                             ctx);
            res = doDispatch(call, ctx);
            ostack_popn(ctx, n);
            ostack_push(ctx, res);

            assert(ttt == R_PPStackTop);
            assert(lll - call.nargs() + 1 == ostack_length(ctx));
            NEXT();
        }

        INSTRUCTION(close_) {
            SEXP srcref = ostack_at(ctx, 0);
            SEXP body = ostack_at(ctx, 1);
            SEXP formals = ostack_at(ctx, 2);
            res = allocSExp(CLOSXP);
            assert(isValidDispatchTableObject(body));
            SET_FORMALS(res, formals);
            SET_BODY(res, body);
            SET_CLOENV(res, getenv());
            Rf_setAttrib(res, Rf_install("srcref"), srcref);
            ostack_popn(ctx, 3);
            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(isfun_) {
            SEXP val = ostack_top(ctx);

            switch (TYPEOF(val)) {
            case CLOSXP:
                jit(val, ctx);
                break;
            case SPECIALSXP:
            case BUILTINSXP:
                // builtins and specials are fine
                // TODO for now - we might be fancier here later
                break;
            default:
                error("attempt to apply non-function");
            }
            NEXT();
        }

        INSTRUCTION(promise_) {
            // get the Code * pointer we need
            Immediate id = readImmediate();
            advanceImmediate();
            Code* promiseCode = c->function()->codeAt(id);
            // create the promise and push it on stack
            SEXP prom = createPromise(promiseCode, getenv());
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
            R_Visible = TRUE;
            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(push_code_) {
            // get the Code * pointer we need
            Immediate n = readImmediate();
            advanceImmediate();
            Code* promiseCode = c->function()->codeAt(n);
            // create the promise and push it on stack
            ostack_push(ctx, (SEXP)promiseCode);
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
            assert(TYPEOF(val) == INTSXP);
            int i = INTEGER(val)[0];
            if (MAYBE_SHARED(val)) {
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
            }

            ostack_popn(ctx, 2);
            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(idiv_) {
            SEXP lhs = ostack_at(ctx, 1);
            SEXP rhs = ostack_at(ctx, 0);

            if (IS_SIMPLE_SCALAR(lhs, REALSXP) &&
                IS_SIMPLE_SCALAR(rhs, REALSXP)) {
                double real_res = myfloor(*REAL(lhs), *REAL(rhs));
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
            }

            ostack_popn(ctx, 2);
            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(mod_) {
            SEXP lhs = ostack_at(ctx, 1);
            SEXP rhs = ostack_at(ctx, 0);

            if (IS_SIMPLE_SCALAR(lhs, REALSXP) &&
                IS_SIMPLE_SCALAR(rhs, REALSXP)) {
                double real_res = myfmod(*REAL(lhs), *REAL(rhs));
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
            }

            ostack_popn(ctx, 2);
            ostack_push(ctx, res);
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

        INSTRUCTION(identical_) {
            SEXP rhs = ostack_pop(ctx);
            SEXP lhs = ostack_pop(ctx);
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
            int x2 = LOGICAL(ostack_pop(ctx))[0];
            int x1 = LOGICAL(ostack_pop(ctx))[0];
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
            int x2 = LOGICAL(ostack_pop(ctx))[0];
            int x1 = LOGICAL(ostack_pop(ctx))[0];
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
            res = Rf_ScalarLogical(x1);
            ostack_pop(ctx);
            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(asbool_) {
            SEXP val = ostack_top(ctx);
            int cond = NA_LOGICAL;
            if (XLENGTH(val) > 1)
                warningcall(getSrcAt(c, pc - 1, ctx),
                            ("the condition has length > 1 and only the first "
                             "element will be used"));

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
                errorcall(getSrcAt(c, pc - 1, ctx), msg);
            }

            ostack_pop(ctx);
            ostack_push(ctx, cond ? R_TrueValue : R_FalseValue);
            NEXT();
        }

        INSTRUCTION(asast_) {
            SEXP val = ostack_pop(ctx);
            assert(TYPEOF(val) == PROMSXP);
            res = PRCODE(val);
            // if the code is NILSXP then it is rir Code object, get its ast
            if (TYPEOF(res) == NILSXP)
                res = cp_pool_at(ctx, ((Code*)res)->src);
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

            default:
                assert(false);
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

        INSTRUCTION(missing_) {
            SEXP sym = readConst(ctx, readImmediate());
            advanceImmediate();
            SLOWASSERT(TYPEOF(sym) == SYMSXP);
            SLOWASSERT(!DDVAL(sym));
            assert(env);
            SEXP val = R_findVarLocInFrame(getenv(), sym).cell;
            if (val == NULL)
                errorcall(getSrcAt(c, pc - 1, ctx),
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
            if (OBJECT(ostack_top(ctx)))
                pc = pc + offset;
            PC_BOUNDSCHECK(pc, c);
            NEXT();
        }

        INSTRUCTION(brtrue_) {
            JumpOffset offset = readJumpOffset();
            advanceJump();
            if (ostack_pop(ctx) == R_TrueValue) {
                pc = pc + offset;
                if (offset < 0)
                    incPerfCount(c);
            }
            PC_BOUNDSCHECK(pc, c);
            NEXT();
        }

        INSTRUCTION(brfalse_) {
            JumpOffset offset = readJumpOffset();
            advanceJump();
            if (ostack_pop(ctx) == R_FalseValue) {
                pc = pc + offset;
                if (offset < 0)
                    incPerfCount(c);
            }
            PC_BOUNDSCHECK(pc, c);
            NEXT();
        }

        INSTRUCTION(br_) {
            JumpOffset offset = readJumpOffset();
            advanceJump();
            if (offset < 0)
                incPerfCount(c);
            pc = pc + offset;
            PC_BOUNDSCHECK(pc, c);
            NEXT();
        }

        INSTRUCTION(extract1_1_) {
            SEXP val = ostack_at(ctx, 1);
            SEXP idx = ostack_at(ctx, 0);

            SEXP args = CONS_NR(idx, R_NilValue);
            args = CONS_NR(val, args);
            ostack_push(ctx, args);
            res = do_subset_dflt(R_NilValue, R_SubsetSym, args, getenv());
            ostack_popn(ctx, 3);

            R_Visible = TRUE;
            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(extract1_2_) {
            SEXP val = ostack_at(ctx, 2);
            SEXP idx = ostack_at(ctx, 1);
            SEXP idx2 = ostack_at(ctx, 0);

            SEXP args = CONS_NR(idx2, R_NilValue);
            args = CONS_NR(idx, args);
            args = CONS_NR(val, args);
            ostack_push(ctx, args);
            res = do_subset_dflt(R_NilValue, R_SubsetSym, args, getenv());
            ostack_popn(ctx, 4);

            R_Visible = TRUE;
            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(subassign1_) {
            SEXP vec = ostack_at(ctx, 2);
            SEXP idx = ostack_at(ctx, 1);
            SEXP val = ostack_at(ctx, 0);

            INCREMENT_NAMED(vec);
            SEXP args = CONS_NR(val, R_NilValue);
            args = CONS_NR(idx, args);
            args = CONS_NR(vec, args);
            PROTECT(args);
            res = do_subassign_dflt(R_NilValue, R_SubassignSym, args, getenv());
            ostack_popn(ctx, 3);
            UNPROTECT(1);

            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(extract2_1_) {
            SEXP val = ostack_at(ctx, 1);
            SEXP idx = ostack_at(ctx, 0);
            int i = -1;

            if (getAttrib(val, R_NamesSymbol) != R_NilValue ||
                ATTRIB(idx) != R_NilValue)
                goto fallback;

            switch (TYPEOF(idx)) {
            case REALSXP:
                if (SHORT_VEC_LENGTH(idx) != 1 || *REAL(idx) == NA_REAL)
                    goto fallback;
                i = (int)*REAL(idx) - 1;
                break;
            case INTSXP:
                if (SHORT_VEC_LENGTH(idx) != 1 || *INTEGER(idx) == NA_INTEGER)
                    goto fallback;
                i = *INTEGER(idx) - 1;
                break;
            case LGLSXP:
                if (SHORT_VEC_LENGTH(idx) != 1 || *LOGICAL(idx) == NA_LOGICAL)
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
            res = allocVector(vectype, 1);                                     \
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
            ostack_push(ctx, res);
            NEXT();

        // ---------
        fallback : {
            SEXP args = CONS_NR(idx, R_NilValue);
            args = CONS_NR(val, args);
            ostack_push(ctx, args);
            res = do_subset2_dflt(R_NilValue, R_Subset2Sym, args, getenv());
            ostack_popn(ctx, 3);

            R_Visible = TRUE;
            ostack_push(ctx, res);
            NEXT();
        }
        }

        INSTRUCTION(extract2_2_) {
            SEXP val = ostack_at(ctx, 2);
            SEXP idx = ostack_at(ctx, 1);
            SEXP idx2 = ostack_at(ctx, 0);

            SEXP args = CONS_NR(idx2, R_NilValue);
            args = CONS_NR(idx, args);
            args = CONS_NR(val, args);
            ostack_push(ctx, args);
            res = do_subset_dflt(R_NilValue, R_Subset2Sym, args, getenv());
            ostack_popn(ctx, 4);

            R_Visible = TRUE;
            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(subassign2_) {
            SEXP vec = ostack_at(ctx, 2);
            SEXP idx = ostack_at(ctx, 1);
            SEXP val = ostack_at(ctx, 0);

            unsigned targetI = readImmediate();
            advanceImmediate();

            // Fast case
            if (!MAYBE_SHARED(vec)) {
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

                    // if the target == R_NilValue that means this is a stack
                    // allocated
                    // vector
                    SEXP target = cp_pool_at(ctx, targetI);
                    bool localBinding = (target == R_NilValue) ||
                                        !R_VARLOC_IS_NULL(R_findVarLocInFrame(
                                            getenv(), target));

                    if (localBinding) {
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
                                REAL(vec)[idx_] = valT == REALSXP
                                                       ? *REAL(val)
                                                       : (double)*INTEGER(val);
                                break;
                            case INTSXP:
                                INTEGER(vec)[idx_] = *INTEGER(val);
                                break;
                            case VECSXP:
                                SET_VECTOR_ELT(vec, idx_, val);
                                break;
                            }
                            ostack_popn(ctx, 3);

                            // this is a very nice and dirty hack...
                            // if the next instruction is a matching stvar
                            // (which is highly probably) then we do not
                            // have to execute it, since we changed the value
                            // inline
                            if (target != R_NilValue && *pc == Opcode::stvar_ &&
                                *(int*)(pc - sizeof(int)) == *(int*)(pc + 1)) {
                                pc = pc + sizeof(int) + 1;
                                if (NAMED(vec) == 0)
                                    SET_NAMED(vec, 1);
                            } else {
                                ostack_push(ctx, vec);
                            }
                            NEXT();
                        }
                    }
                }
            }

            INCREMENT_NAMED(vec);
            SEXP args = CONS_NR(val, R_NilValue);
            args = CONS_NR(idx, args);
            args = CONS_NR(vec, args);
            PROTECT(args);
            res =
                do_subassign2_dflt(R_NilValue, R_Subassign2Sym, args, getenv());
            ostack_popn(ctx, 3);
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
            assert(res == Rf_findFun(sym, getenv()) && "guard_fun_ fail");
#endif
            NEXT();
        }

        INSTRUCTION(guard_env_) {
            uint32_t deoptId = readImmediate();
            advanceImmediate();
            if (FRAME_CHANGED(getenv()) || FRAME_LEAKED(getenv())) {
                Function* fun = c->function();
                assert(fun->body() == c && "Cannot deopt from promise");
                fun->deopt = true;
                SEXP val = fun->origin();
                Function* deoptFun = Function::unpack(val);
                Code* deoptCode = deoptFun->body();
                c = deoptCode;
                pc = Deoptimizer_pc(deoptId);
                PC_BOUNDSCHECK(pc, c);
            }
            NEXT();
        }

        INSTRUCTION(seq_) {
            static SEXP prim = NULL;
            if (!prim) {
                // TODO: we could call seq.default here, but it messes up the
                // error
                // call :(
                prim = Rf_findFun(Rf_install("seq"), R_GlobalEnv);
            }

            // TODO: add a real guard here...
            assert(prim == Rf_findFun(Rf_install("seq"), getenv()));

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
                res = applyClosure(call, prim, argslist, getenv(), R_NilValue);
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

            if (res == NULL) {
                BINOP_FALLBACK(":");
            }

            ostack_popn(ctx, 2);
            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(names_) {
            ostack_push(ctx, getAttrib(ostack_pop(ctx), R_NamesSymbol));
            NEXT();
        }

        INSTRUCTION(set_names_) {
            SEXP val = ostack_pop(ctx);
            if (!isNull(val))
                setAttrib(ostack_top(ctx), R_NamesSymbol, val);
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
            SEXP value = allocVector(INTSXP, 1);
            if (isVector(seq)) {
                INTEGER(value)[0] = LENGTH(seq);
            } else if (isList(seq) || isNull(seq)) {
                INTEGER(value)[0] = Rf_length(seq);
            } else {
                errorcall(R_NilValue, "invalid for() loop sequence");
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

        INSTRUCTION(set_shared_) {
            SEXP val = ostack_top(ctx);
            if (NAMED(val) < 2) {
                SET_NAMED(val, 2);
            }
            NEXT();
        }

        INSTRUCTION(make_unique_) {
            SEXP val = ostack_top(ctx);
            if (NAMED(val) == 2) {
                val = shallow_duplicate(val);
                ostack_set(ctx, 0, val);
                SET_NAMED(val, 1);
            }
            NEXT();
        }

        INSTRUCTION(beginloop_) {
            // Allocate a RCNTXT on the stack
            SEXP val = Rf_allocVector(RAWSXP, sizeof(RCNTXT) + sizeof(pc));
            ostack_push(ctx, val);

            RCNTXT* cntxt = (RCNTXT*)RAW(val);

            // (ab)use the same buffe to store the current pc
            Opcode** oldPc = (Opcode**)(cntxt + 1);
            *oldPc = pc;

            Rf_begincontext(cntxt, CTXT_LOOP, R_NilValue, getenv(), R_BaseEnv,
                            R_NilValue, R_NilValue);
            // (ab)use the unused cenddata field to store sp
            cntxt->cenddata = (void*)ostack_length(ctx);

            advanceJump();

            int s;
            if ((s = SETJMP(cntxt->cjmpbuf))) {
                // incoming non-local break/continue:
                // restore our stack state

                // get the RCNTXT from the stack
                val = ostack_top(ctx);
                assert(TYPEOF(val) == RAWSXP && "stack botched");
                RCNTXT* cntxt = (RCNTXT*)RAW(val);
                assert(cntxt == R_GlobalContext && "stack botched");
                Opcode** oldPc = (Opcode**)(cntxt + 1);
                pc = *oldPc;

                int offset = readJumpOffset();
                advanceJump();

                if (s == CTXT_BREAK)
                    pc = pc + offset;
                PC_BOUNDSCHECK(pc, c);
            }
            NEXT();
        }

        INSTRUCTION(endcontext_) {
            SEXP val = ostack_top(ctx);
            assert(TYPEOF(val) == RAWSXP);
            RCNTXT* cntxt = (RCNTXT*)RAW(val);
            Rf_endcontext(cntxt);
            ostack_pop(ctx); // Context
            NEXT();
        }

        INSTRUCTION(return_) {
            res = ostack_top(ctx);
            // this restores stack pointer to the value from the target context
            Rf_findcontext(CTXT_BROWSER | CTXT_FUNCTION, getenv(), res);
            // not reached
            NEXT();
        }

        INSTRUCTION(ret_) { goto eval_done; }

        INSTRUCTION(int3_) {
            asm("int3");
            NEXT();
        }

        LASTOP;
    }

eval_done:
    return ostack_pop(ctx);
}

SEXP rirExpr(SEXP f) {
    if (isValidCodeObject(f)) {
        Code* c = (Code*)f;
        return src_pool_at(globalContext(), c->src);
    }
    Function* ff;
    if ((ff = Function::check(f))) {
        return src_pool_at(globalContext(), ff->body()->src);
    }
    DispatchTable* t;
    if ((t = DispatchTable::check(f))) {
        // Default is the source of the first function in the dispatch table
        Function* ff = t->first();
        return src_pool_at(globalContext(), ff->body()->src);
    }
    return f;
}

SEXP rirEval_f(SEXP what, SEXP env) {
    assert(TYPEOF(what) == EXTERNALSXP);

    SEXP lenv = env;
    // TODO: do we not need an RCNTXT here?

    if (isValidCodeObject(what))
        return evalRirCodeExtCaller((Code*)what, globalContext(), &lenv);

    if (DispatchTable::check(what)) {
        auto table = DispatchTable::unpack(what);
        size_t offset = 0; // Default target is the first version

        Function* fun = table->at(offset);
        fun->registerInvocation();

        return evalRirCodeExtCaller(fun->body(), globalContext(), &lenv);
    }

    assert(false && "Expected a code object or a dispatch table");
}
