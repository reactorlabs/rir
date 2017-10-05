#include <assert.h>
#include <alloca.h>

#include "interp.h"
#include "interp_context.h"
#include "runtime.h"
#include "R/Funtab.h"
#include "interpreter/deoptimizer.h"

#define NOT_IMPLEMENTED assert(false)

#undef eval

extern SEXP R_TrueValue;
extern SEXP R_FalseValue;
extern SEXP Rf_NewEnvironment(SEXP, SEXP, SEXP);
extern Rboolean R_Visible;

// #define UNSOUND_OPTS

// helpers

INLINE SEXP getSrcAt(Code* c, OpcodeT* pc, Context* ctx) {
    unsigned sidx = getSrcIdxAt(c, pc, true);
    if (sidx == 0)
        return src_pool_at(ctx, c->src);
    return src_pool_at(ctx, sidx);
}

INLINE SEXP getSrcForCall(Code* c, OpcodeT* pc, Context* ctx) {
    unsigned sidx = getSrcIdxAt(c, pc, false);
    return src_pool_at(ctx, sidx);
}

#define PC_BOUNDSCHECK(pc, c)                                                  \
    SLOWASSERT((pc) >= code(c) && (pc) < code(c) + (c)->codeSize);

#ifdef THREADED_CODE

#define BEGIN_MACHINE NEXT();
#define INSTRUCTION(name) \
    op_##name:  // debug(c, pc, #name, ostack_length(ctx) - bp, ctx);
#define NEXT() (__extension__ ({goto *opAddr[advanceOpcode()];}))
#define LASTOP {}

#else

#define BEGIN_MACHINE  loop: switch(advanceOpcode())
#define INSTRUCTION(name)  case name:  // debug(c, pc, #name, ostack_length(ctx) - bp, ctx);
#define NEXT()  goto loop
#define LASTOP  default: assert(false && "wrong or unimplemented opcode") /* error(_("bad opcode")) */

#endif

// bytecode accesses

#define advanceOpcode() (*pc++)
#define readImmediate() (*(Immediate*)pc)
#define readSignedImmediate() (*(SignedImmediate*)pc)
#define readJumpOffset() (*(JumpOffset*)(pc))
#define advanceImmediate() pc += sizeof(Immediate)
#define advanceJump() pc += sizeof(JumpOffset)

#define readConst(ctx, idx) (cp_pool_at(ctx, idx))
//INLINE SEXP readConst(Context* ctx, unsigned idx) {
//    return cp_pool_at(ctx, idx);
//}

void initClosureContext(RCNTXT* cntxt, SEXP call, SEXP rho, SEXP sysparent,
                        SEXP arglist, SEXP op) {

    /*  If we have a generic function we need to use the sysparent of
       the generic as the sysparent of the method because the method
       is a straight substitution of the generic.  */

    if (R_GlobalContext->callflag == CTXT_GENERIC)
        begincontext(cntxt, CTXT_RETURN, call, rho, R_GlobalContext->sysparent,
                     arglist, op);
    else
        begincontext(cntxt, CTXT_RETURN, call, rho, sysparent, arglist, op);
}

void endClosureContext(RCNTXT* cntxt, SEXP result) {
    cntxt->returnValue = result;
    endcontext(cntxt);
}

INLINE SEXP createPromise(Code* code, SEXP env) {
    SEXP p = mkPROMISE((SEXP)code, env);
    return p;
}

INLINE SEXP promiseValue(SEXP promise, Context * ctx) {
    // if already evaluated, return the value
    if (PRVALUE(promise) && PRVALUE(promise) != R_UnboundValue) {
        promise = PRVALUE(promise);
        assert(TYPEOF(promise) != PROMSXP);
        SET_NAMED(promise, 2);
        return promise;
    } else {
        return forcePromise(promise);
    }
}

static void jit(SEXP cls, Context* ctx) {
    assert(TYPEOF(cls) == CLOSXP);
    if (TYPEOF(BODY(cls)) == EXTERNALSXP)
        return;
    SEXP cmp = ctx->compiler(cls, NULL);
    SET_BODY(cls, BODY(cmp));
    SET_FORMALS(cls, FORMALS(cmp));
}

void closureDebug(SEXP call, SEXP op, SEXP rho, SEXP newrho, RCNTXT* cntxt) {
    // TODO!!!
}

void endClosureDebug(SEXP op, SEXP call, SEXP rho) {
    // TODO!!!
}

/** Given argument code offsets, creates the argslist from their promises.
 */
// TODO unnamed only at this point
INLINE void __listAppend(SEXP* front, SEXP* last, SEXP value, SEXP name) {
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

SEXP createArgsListStack(Code* c, size_t nargs, CallSiteStruct* cs, SEXP env,
                         Context* ctx, bool eager) {
    SEXP result = R_NilValue;
    SEXP pos = result;

    bool hasNames = cs->hasNames;

    for (size_t i = 0; i < nargs; ++i) {

        SEXP name =
            hasNames ? cp_pool_at(ctx, CallSite_names(cs)[i]) : R_NilValue;

        SEXP arg = ostack_at(ctx, nargs - i - 1);

        if (!eager && (arg == R_MissingArg || arg == R_DotsSymbol)) {
            // We have to wrap them in a promise, otherwise they are threated
            // as extression to be evaluated, when in fact they are meant to be
            // asts as values
            SEXP promise = mkPROMISE(arg, env);
            SET_PRVALUE(promise, arg);
            __listAppend(&result, &pos, promise, R_NilValue);
        } else {
            if (eager && TYPEOF(arg) == PROMSXP) {
                arg = Rf_eval(arg, env);
            }
            __listAppend(&result, &pos, arg, name);
        }
    }

    if (result != R_NilValue)
        UNPROTECT(1);
    return result;
}

SEXP createArgsList(Code* c, SEXP call, size_t nargs, CallSiteStruct* cs,
                    SEXP env, Context* ctx, bool eager) {
    SEXP result = R_NilValue;
    SEXP pos = result;

    // loop through the arguments and create a promise, unless it is a missing
    // argument
    bool hasNames = cs->hasNames;

    for (size_t i = 0; i < nargs; ++i) {
        unsigned argi = CallSite_args(cs)[i];
        SEXP name =
            hasNames ? cp_pool_at(ctx, CallSite_names(cs)[i]) : R_NilValue;

        // if the argument is an ellipsis, then retrieve it from the environment
        // and
        // flatten the ellipsis
        if (argi == DOTS_ARG_IDX) {
            SEXP ellipsis = findVar(R_DotsSymbol, env);
            if (TYPEOF(ellipsis) == DOTSXP) {
                while (ellipsis != R_NilValue) {
                    name = TAG(ellipsis);
                    if (eager) {
                        SEXP arg = CAR(ellipsis);
                        if (arg != R_MissingArg)
                            arg = Rf_eval(CAR(ellipsis), env);
                        assert(TYPEOF(arg) != PROMSXP);
                        __listAppend(&result, &pos, arg, name);
                    } else {
                        SEXP promise = mkPROMISE(CAR(ellipsis), env);
                        __listAppend(&result, &pos, promise, name);
                    }
                    ellipsis = CDR(ellipsis);
                }
            }
        } else if (argi == MISSING_ARG_IDX) {
            if (eager)
                Rf_errorcall(call, "argument %d is empty", i + 1);
            __listAppend(&result, &pos, R_MissingArg, R_NilValue);
        } else {
            if (eager) {
                SEXP arg = evalRirCode(codeAt(code2function(c), argi), ctx, env, 0);
                assert(TYPEOF(arg) != PROMSXP);
                __listAppend(&result, &pos, arg, name);
            } else {
                Code* arg = codeAt(code2function(c), argi);
                SEXP promise = createPromise(arg, env);
                __listAppend(&result, &pos, promise, name);
            }
        }
    }

    if (result != R_NilValue)
        UNPROTECT(1);
    return result;
}

static SEXP closureArgumentAdaptor(SEXP call, SEXP op, SEXP arglist, SEXP rho,
                                   SEXP suppliedvars) {
    if (FORMALS(op) == R_NilValue && arglist == R_NilValue)
        return Rf_NewEnvironment(R_NilValue, R_NilValue, CLOENV(op));

    /*  Set up a context with the call in it so error has access to it */
    RCNTXT cntxt;
    initClosureContext(&cntxt, call, CLOENV(op), rho, arglist, op);

    /*  Build a list which matches the actual (unevaluated) arguments
        to the formal paramters.  Build a new environment which
        contains the matched pairs.  Ideally this environment sould be
        hashed.  */
    SEXP newrho, a, f;

    SEXP actuals = matchArgs(FORMALS(op), arglist, call);
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
    while (f != R_NilValue) {
        if (CAR(a) == R_MissingArg && CAR(f) != R_MissingArg) {
            SETCAR(a, mkPROMISE(CAR(f), newrho));
            SET_MISSING(a, 2);
        }
        assert(CAR(f) != R_DotsSymbol || TYPEOF(CAR(a)) == DOTSXP);
        f = CDR(f);
        a = CDR(a);
    }

    /*  Fix up any extras that were supplied by usemethod. */

    if (suppliedvars != R_NilValue)
        addMissingVarsToNewEnv(newrho, suppliedvars);

    if (R_envHasNoSpecialSymbols(newrho))
        SET_NO_SPECIAL_SYMBOLS(newrho);

    endClosureContext(&cntxt, R_NilValue);

    UNPROTECT(1);

    return newrho;
}

SEXP rirCallTrampoline(RCNTXT* cntxt, Code* code, SEXP env, unsigned nargs,
                       Context* ctx) {
    if ((SETJMP(cntxt->cjmpbuf))) {
        if (R_ReturnedValue == R_RestartToken) {
            cntxt->callflag = CTXT_RETURN; /* turn restart off */
            R_ReturnedValue = R_NilValue;  /* remove restart token */
            return evalRirCode(code, ctx, env, nargs);
        } else {
            return R_ReturnedValue;
        }
    }
    return evalRirCode(code, ctx, env, nargs);
}

static SEXP rirCallClosure(SEXP call, SEXP env, SEXP callee, SEXP actuals,
                           unsigned nargs, Context* ctx) {

    SEXP body = BODY(callee);
    Function* fun = sexp2function(body);

    static bool optimizing = false;

    if (!optimizing &&
        !fun->next
         /* currently there is a bug if we reoptimize a function twice:
          *  Deopt ids from the first optimization and second optimizations
          *  will be mixed and the deoptimizer always only goes back one
          *  optimization level!
          *  To avoid this bug we currently only optimize once
          */
        &&
        !fun->origin) {
        Code* code = bodyCode(fun);
        if (fun->markOpt ||
            (fun->invocationCount == 1 && code->perfCounter > 100) ||
            (fun->invocationCount == 10 && code->perfCounter > 20) ||
            fun->invocationCount == 100) {
            optimizing = true;

            Function* oldFun = fun;
            SEXP oldBody = body;
            cp_pool_add(ctx, oldBody);
            body = globalContext()->optimizer(callee);

            // TODO: there might be promises with references to the old code!
            // Therefore we keep it around.
            // TODO: first I tried to use R_PreserveObject, but it did not work
            // for large vectors, not sure why, need to investigate
            cp_pool_add(ctx, body);

            fun->next = body;

            SET_BODY(callee, body);
            fun = sexp2function(body);
            fun->origin = oldBody;

            fun->invocationCount = oldFun->invocationCount + 1;
            fun->envLeaked = oldFun->envLeaked;
            fun->envChanged = oldFun->envChanged;

            optimizing = false;
        } else if (fun->invocationCount < UINT_MAX)
            fun->invocationCount++;
    }

    // match formal arguments and create the env of this new activation record
    SEXP newEnv =
        closureArgumentAdaptor(call, callee, actuals, env, R_NilValue);

    ostack_push(ctx, newEnv);

    RCNTXT cntxt;
    if (R_GlobalContext->callflag == CTXT_GENERIC)
        Rf_begincontext(&cntxt, CTXT_RETURN, call, newEnv,
                        R_GlobalContext->sysparent, actuals, callee);
    else
        Rf_begincontext(&cntxt, CTXT_RETURN, call, newEnv, env, actuals, callee);

    // Exec the closure
    closureDebug(call, callee, env, newEnv, &cntxt);
    Code* code = bodyCode(fun);

    SEXP result = rirCallTrampoline(&cntxt, code, newEnv, nargs, ctx);

    endClosureDebug(callee, call, env);

    endClosureContext(&cntxt, result);

    if (!fun->envLeaked && FRAME_LEAKED(newEnv))
        fun->envLeaked = true;
    if (!fun->envChanged && FRAME_CHANGED(newEnv))
        fun->envChanged = true;

    if (fun->deopt)
        SET_BODY(callee, fun->origin);

    ostack_pop(ctx); // newEnv
    return result;
}

void warnSpecial(SEXP callee, SEXP call) {
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

void doProfileCall(CallSiteStruct*, SEXP);
INLINE void profileCall(CallSiteStruct* cs, SEXP callee) {
    if (!cs->hasProfile)
        return;
    doProfileCall(cs, callee);
}

void doProfileCall(CallSiteStruct* cs, SEXP callee) {
    CallSiteProfile* p = CallSite_profile(cs);
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

SEXP doCall(Code* caller, SEXP callee, unsigned nargs, unsigned id, SEXP env,
            Context* ctx) {

    CallSiteStruct* cs = CallSite_get(caller, id);
    profileCall(cs, callee);
    SEXP call = cp_pool_at(ctx, cs->call);

    SEXP result = R_NilValue;
    switch (TYPEOF(callee)) {
    case SPECIALSXP: {
        // get the ccode
        CCODE f = getBuiltin(callee);
        int flag = getFlag(callee);
        R_Visible = flag != 1;
        warnSpecial(callee, call);

        // Store and restore stack status in case we get back here through
        // non-local return
        // call it with the AST only
        result = f(call, callee, CDR(call), env);
        if (flag < 2) R_Visible = flag != 1;
        break;
    }
    case BUILTINSXP: {
        // get the ccode
        CCODE f = getBuiltin(callee);
        int flag = getFlag(callee);
        // create the argslist
        SEXP argslist = createArgsList(caller, call, nargs, cs, env, ctx, true);
        // callit
        PROTECT(argslist);
        if (flag < 2) R_Visible = flag != 1;

        // Store and restore stack status in case we get back here through
        // non-local return
        result = f(call, callee, argslist, env);
        if (flag < 2) R_Visible = flag != 1;
        UNPROTECT(1);
        break;
    }
    case CLOSXP: {
        SEXP argslist =
            createArgsList(caller, call, nargs, cs, env, ctx, false);
        PROTECT(argslist);

        // if body is INTSXP, it is rir serialized code, execute it directly
        SEXP body = BODY(callee);
        if (TYPEOF(body) == EXTERNALSXP) {
            assert(isValidFunctionSEXP(body));
            result =
                rirCallClosure(call, env, callee, argslist, nargs, ctx);
            UNPROTECT(1);
            break;
        }

        Function * f = isValidClosureSEXP(callee);

        // Store and restore stack status in case we get back here through
        // non-local return
        result = applyClosure(call, callee, argslist, env, R_NilValue);
        UNPROTECT(1); // argslist
        break;
    }
    default:
        assert(false && "Don't know how to run other stuff");
    }
    return result;
}

INLINE SEXP fixupAST(SEXP call, Context* ctx, size_t nargs) {
    // This is a hack to support complex assignment's rewritten asts for
    // getters and setters.
    // The rewritten ast has target (and value for setters) marked as
    // placeholders, which we need to fill in here.
    if ((CADR(call) == getterPlaceholderSym ||
         CADR(call) == setterPlaceholderSym)) {
        int setter = CADR(call) == setterPlaceholderSym;
        call = Rf_shallow_duplicate(call);
        PROTECT(call);

        SEXP a = CDR(call);

        SEXP target = ostack_at(ctx, nargs - 1);

        if (target == R_MissingArg) {
            assert(!setter);
            SETCDR(call, R_NilValue);
            UNPROTECT(1);
            return call;
        }

        SEXP p = target;
        // It might be tempting to put the values as consts into the ast, but
        // then they are converted to consts (named = 2) which is bad.
        // therefore we wrap them in fake promises.
        if (TYPEOF(p) != PROMSXP) {
            p = mkPROMISE(getterPlaceholderSym, R_NilValue);
            SET_PRVALUE(p, target);
        }

        SETCAR(a, p);

        if (setter) {
            SEXP prev = call;
            while (CDR(a) != R_NilValue) {
                prev = a;
                a = CDR(a);
            }

            assert(CAR(a) == setterPlaceholderSym);
            SEXP val = ostack_top(ctx);

            SEXP p = val;
            if (TYPEOF(p) != PROMSXP) {
                p = mkPROMISE(setterPlaceholderSym, R_NilValue);
                SET_PRVALUE(p, val);
            }

            SEXP v = CONS_NR(p, R_NilValue);
            SET_TAG(v, R_valueSym);
            SETCDR(prev, v);
        }
        UNPROTECT(1);
    }
    return call;
}

// TODO: unify with the above doCall
SEXP doCallStack(Code* caller, SEXP callee, size_t nargs, unsigned id, SEXP env,
                 Context* ctx) {

    CallSiteStruct* cs = CallSite_get(caller, id);
    SEXP call = cp_pool_at(ctx, cs->call);

    SEXP res = R_NilValue;

    // TODO: in the case of closures we should not do it eagerly
    profileCall(cs, callee);
    if (TYPEOF(callee) == SPECIALSXP || TYPEOF(callee) == CLOSXP)
        call = fixupAST(call, ctx, nargs);
    PROTECT(call);

    switch (TYPEOF(callee)) {
    case SPECIALSXP: {
        assert(call != R_NilValue);
        ostack_popn(ctx, nargs);
        // get the ccode
        CCODE f = getBuiltin(callee);
        int flag = getFlag(callee);
        R_Visible = flag != 1;
        warnSpecial(callee, call);

        // Store and restore stack status in case we get back here through
        // non-local return
        // call it with the AST only
        res = f(call, callee, CDR(call), env);
        if (flag < 2)
            R_Visible = flag != 1;
        break;
    }
    case BUILTINSXP: {
        SEXP argslist = createArgsListStack(caller, nargs, cs, env, ctx, true);
        PROTECT(argslist);
        ostack_popn(ctx, nargs);
        // get the ccode
        CCODE f = getBuiltin(callee);
        int flag = getFlag(callee);
        // create the argslist
        // callit
        if (flag < 2)
            R_Visible = flag != 1;

        // Store and restore stack status in case we get back here through
        // non-local return
        res = f(call, callee, argslist, env);
        if (flag < 2)
            R_Visible = flag != 1;
        UNPROTECT(1);
        break;
    }
    case CLOSXP: {
        SEXP argslist = createArgsListStack(caller, nargs, cs, env, ctx, false);
        PROTECT(argslist);
        ostack_popn(ctx, nargs);

        // if body is INTSXP, it is rir serialized code, execute it directly
        SEXP body = BODY(callee);
        if (TYPEOF(body) == EXTERNALSXP) {
            assert(isValidFunctionSEXP(body));
            res = rirCallClosure(call, env, callee, argslist, nargs, ctx);
            UNPROTECT(1);
            break;
        }

        Function* f = isValidClosureSEXP(callee);

        // Store and restore stack status in case we get back here through
        // non-local return
        res = applyClosure(call, callee, argslist, env, R_NilValue);
        UNPROTECT(1);
        break;
    }
    default:
        assert(false && "Don't know how to run other stuff");
    }
    UNPROTECT(1);
    return res;
}

SEXP doDispatchStack(Code* caller, size_t nargs, uint32_t id, SEXP env,
                     Context* ctx) {

    CallSiteStruct* cs = CallSite_get(caller, id);
    profileCall(cs, Rf_install("*dispatch*"));
    SEXP call = cp_pool_at(ctx, cs->call);
    SEXP selector = cp_pool_at(ctx, *CallSite_selector(cs));
    SEXP op = SYMVALUE(selector);

    SEXP obj = ostack_at(ctx, nargs - 1);
    assert(isObject(obj));

    call = fixupAST(call, ctx, nargs);
    PROTECT(call);

    SEXP actuals = createArgsListStack(caller, nargs, cs, env, ctx, true);

    ostack_popn(ctx, nargs);

    ostack_push(ctx, actuals);
    ostack_push(ctx, call);
    UNPROTECT(1);

    SEXP res;

    do {
        // ===============================================
        // First try S4
        if (IS_S4_OBJECT(obj) && R_has_methods(op)) {
            res = R_possible_dispatch(call, op, actuals, env, TRUE);
            if (res) {
                break;
            }
        }

        // ===============================================
        // Then try S3
        const char* generic = CHAR(PRINTNAME(selector));
        RCNTXT cntxt;
        SEXP rho1 = Rf_NewEnvironment(R_NilValue, R_NilValue, env);
        ostack_push(ctx, rho1);
        initClosureContext(&cntxt, call, rho1, env, actuals, op);
        bool success =
            usemethod(generic, obj, call, actuals, rho1, env, R_BaseEnv, &res);
        ostack_pop(ctx);
        endClosureContext(&cntxt, success ? res : R_NilValue);
        if (success) {
            break;
        }

        // ===============================================
        // Now normal dispatch (mostly a copy from doCall)
        SEXP callee = findFun(selector, env);

        // TODO something should happen here
        if (callee == R_UnboundValue)
            assert(false && "Unbound var");
        else if (callee == R_MissingArg)
            assert(false && "Missing argument");

        switch (TYPEOF(callee)) {
        case SPECIALSXP: {
            // get the ccode
            CCODE f = getBuiltin(callee);
            int flag = getFlag(callee);
            R_Visible = flag != 1;
            warnSpecial(callee, call);

            // call it with the AST only
            res = f(call, callee, CDR(call), env);

            if (flag < 2)
                R_Visible = flag != 1;
            break;
        }
        case BUILTINSXP: {
            // get the ccode
            CCODE f = getBuiltin(callee);
            int flag = getFlag(callee);
            // force all promises in the args list
            for (SEXP a = actuals; a != R_NilValue; a = CDR(a))
                SETCAR(a, Rf_eval(CAR(a), env));
            if (flag < 2)
                R_Visible = flag != 1;

            // Store and restore stack status in case we get back here through
            // non-local return
            res = f(call, callee, actuals, env);

            if (flag < 2)
                R_Visible = flag != 1;
            break;
        }
        case CLOSXP: {
            // if body is INTSXP, it is rir serialized code, execute it directly
            SEXP body = BODY(callee);
            if (TYPEOF(body) == EXTERNALSXP) {
                assert(isValidFunctionSEXP(body));
                res =
                    rirCallClosure(call, env, callee, actuals, nargs, ctx);
                break;
            }
            // Store and restore stack status in case we get back here through
            // non-local return
            res = applyClosure(call, callee, actuals, env, R_NilValue);
            break;
        }
        default:
            assert(false && "Don't know how to run other stuff");
        }
    } while (false);

    ostack_popn(ctx, 2);
    // This line resets the sp after possible non-local return
    assert(res);
    return res;
}

SEXP doDispatch(Code* caller, uint32_t nargs, uint32_t id, SEXP env,
                Context* ctx) {

    SEXP obj = ostack_top(ctx);
    assert(isObject(obj));

    CallSiteStruct* cs = CallSite_get(caller, id);
    profileCall(cs, Rf_install("*dispatch*"));
    SEXP call = cp_pool_at(ctx, cs->call);
    SEXP selector = cp_pool_at(ctx, *CallSite_selector(cs));
    SEXP op = SYMVALUE(selector);

    SEXP actuals = createArgsList(caller, call, nargs, cs, env, ctx, false);
    ostack_push(ctx, actuals);
    SEXP res = NULL;

    // Patch the already evaluated object into the first entry of the promise
    // args list
    SET_PRVALUE(CAR(actuals), obj);

    do {
        // ===============================================
        // First try S4
        if (IS_S4_OBJECT(obj) && R_has_methods(op)) {
            res = R_possible_dispatch(call, op, actuals, env, TRUE);
            if (res) {
                break;
            }
        }

        // ===============================================
        // Then try S3
        const char* generic = CHAR(PRINTNAME(selector));
        RCNTXT cntxt;
        SEXP rho1 = Rf_NewEnvironment(R_NilValue, R_NilValue, env);
        ostack_push(ctx, rho1);
        initClosureContext(&cntxt, call, rho1, env, actuals, op);
        bool success = Rf_usemethod(generic, obj, call, actuals, rho1, env,
                                    R_BaseEnv, &res);
        ostack_pop(ctx);
        endClosureContext(&cntxt, success ? res : R_NilValue);
        if (success) {
            break;
        }

        // ===============================================
        // Now normal dispatch (mostly a copy from doCall)
        SEXP callee = findFun(selector, env);

        // TODO something should happen here
        if (callee == R_UnboundValue)
            assert(false && "Unbound var");
        else if (callee == R_MissingArg)
            assert(false && "Missing argument");

        switch (TYPEOF(callee)) {
        case SPECIALSXP: {
            // get the ccode
            CCODE f = getBuiltin(callee);
            int flag = getFlag(callee);
            R_Visible = flag != 1;
            warnSpecial(callee, call);

            // call it with the AST only
            res = f(call, callee, CDR(call), env);

            if (flag < 2)
                R_Visible = flag != 1;
            break;
        }
        case BUILTINSXP: {
            // get the ccode
            CCODE f = getBuiltin(callee);
            int flag = getFlag(callee);
            // force all promises in the args list
            for (SEXP a = actuals; a != R_NilValue; a = CDR(a))
                SETCAR(a, Rf_eval(CAR(a), env));
            if (flag < 2)
                R_Visible = flag != 1;

            // Store and restore stack status in case we get back here through
            // non-local return
            res = f(call, callee, actuals, env);

            if (flag < 2)
                R_Visible = flag != 1;
            break;
        }
        case CLOSXP: {
            // if body is INTSXP, it is rir serialized code, execute it directly
            SEXP body = BODY(callee);
            if (TYPEOF(body) == EXTERNALSXP) {
                assert(isValidFunctionSEXP(body));
                res = rirCallClosure(call, env, callee, actuals, nargs, ctx);
                break;
            }

            // Store and restore stack status in case we get back here through
            // non-local return
            res = applyClosure(call, callee, actuals, env, R_NilValue);
            break;
        }
        default:
            assert(false && "Don't know how to run other stuff");
        }
    } while (false);

    ostack_popn(ctx, 2);
    // This line resets the sp after possible non-local return
    assert(res);
    return res;
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
            prim = findFun(Rf_install(op), R_GlobalEnv);                       \
            blt = getBuiltin(prim);                                            \
            flag = getFlag(prim);                                              \
        }                                                                      \
        SEXP call = getSrcForCall(c, pc - 1, ctx);                             \
        SEXP argslist = CONS_NR(lhs, CONS_NR(rhs, R_NilValue));                \
        ostack_push(ctx, argslist);                                            \
        if (flag < 2)                                                          \
            R_Visible = flag != 1;                                             \
        res = blt(call, prim, argslist, env);                                  \
        if (flag < 2)                                                          \
            R_Visible = flag != 1;                                             \
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
        int int_res;                                                           \
        double real_res;                                                       \
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
            prim = findFun(Rf_install(op), R_GlobalEnv);                       \
            blt = getBuiltin(prim);                                            \
            flag = getFlag(prim);                                              \
        }                                                                      \
        SEXP call = getSrcForCall(c, pc - 1, ctx);                             \
        SEXP argslist = CONS_NR(val, R_NilValue);                              \
        ostack_push(ctx, argslist);                                            \
        if (flag < 2)                                                          \
            R_Visible = flag != 1;                                             \
        res = blt(call, prim, argslist, env);                                  \
        if (flag < 2)                                                          \
            R_Visible = flag != 1;                                             \
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
    int *data = INTEGER(ans);
    if (n1 <= n2) {
        while (n1 <= n2) *data++ = n1++;
    } else {
        while (n1 >= n2) *data++ = n1--;
    }
    return ans;
}

INLINE SEXP findRootPromise(SEXP p) {
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

INLINE void incPerfCount(Code* c) {
    if (c->perfCounter < UINT_MAX) {
        c->perfCounter++;
        // if (c->perfCounter == 200000)
        //     printCode(c);
    }
}

static int debugging = 0;
void debug(Code* c, OpcodeT* pc, const char* name, unsigned depth, Context* ctx) {
    return;
    if (debugging == 0) {
        debugging = 1;
        printf("%p : %d, %s, s: %d\n", c, *pc, name, depth);
        for (int i = 0; i < depth; ++i) {
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

INLINE SEXP cachedGetBindingCell(SEXP env, Immediate idx, Context* ctx,
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
    return findVar(sym, env);
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
    defineVar(sym, val, env);
    UNPROTECT(1);
}

SEXP evalRirCode(Code* c, Context* ctx, SEXP env, unsigned numArgs) {

#ifdef THREADED_CODE
    static void* opAddr[numInsns_] = {
#define DEF_INSTR(name, ...) (__extension__ && op_##name),
#include "ir/insns.h"
#undef DEF_INSTR
    };
#endif

    assert(c->magic == CODE_MAGIC);

    BindingCache bindingCache[BINDING_CACHE_SIZE];
    memset(&bindingCache, 0, sizeof(bindingCache));

    if (!env) {
        error("'rho' cannot be C NULL: detected in C-level eval");
    }
    if (!isEnvironment(env)) {
        error("'rho' must be an environment not %s: detected in C-level eval",
              type2char(TYPEOF(env)));
    }

    // make sure there is enough room on the stack
    // there is some slack of 5 to make sure the call instruction can store
    // some intermediate values on the stack
    ostack_ensureSize(ctx, c->stackLength + 5);
    unsigned bp = ostack_length(ctx);

    OpcodeT* pc = code(c);
    SEXP res;

    R_Visible = TRUE;

    // main loop
    BEGIN_MACHINE {

        INSTRUCTION(invalid_) assert(false && "wrong or unimplemented opcode");

        INSTRUCTION(nop_) NEXT();

        INSTRUCTION(ldfun_) {
            SEXP sym = readConst(ctx, readImmediate());
            advanceImmediate();
            res = findFun(sym, env);

            // TODO something should happen here
            if (res == R_UnboundValue)
                assert(false && "Unbound var");
            else if (res == R_MissingArg)
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
            res = cachedGetVar(env, id, ctx, bindingCache);
            R_Visible = TRUE;

            if (res == R_UnboundValue) {
                Rf_error("object not found");
            } else if (res == R_MissingArg) {
                SEXP sym = cp_pool_at(ctx, id);
                Rf_error("argument \"%s\" is missing, with no default", CHAR(PRINTNAME(sym)));
            }

            // if promise, evaluate & return
            if (TYPEOF(res) == PROMSXP)
                res = promiseValue(res, ctx);

            if (NAMED(res) == 0 && res != R_NilValue)
                SET_NAMED(res, 1);

            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(ldvar2_) {
            SEXP sym = readConst(ctx, readImmediate());
            advanceImmediate();
            res = findVar(sym, ENCLOS(env));
            R_Visible = TRUE;

            if (res == R_UnboundValue) {
                Rf_error("object not found");
            } else if (res == R_MissingArg) {
                Rf_error("argument \"%s\" is missing, with no default", CHAR(PRINTNAME(res)));
            }

            // if promise, evaluate & return
            if (TYPEOF(res) == PROMSXP)
                res = promiseValue(res, ctx);

            if (NAMED(res) == 0 && res != R_NilValue)
                SET_NAMED(res, 1);

            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(ldddvar_) {
            SEXP sym = readConst(ctx, readImmediate());
            advanceImmediate();
            res = Rf_ddfindVar(sym, env);
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
            res = cachedGetBindingCell(env, id, ctx, bindingCache);
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
            Immediate id = readImmediate();
            advanceImmediate();
            res = cachedGetBindingCell(env, id, ctx, bindingCache);
            assert(res);
            res = CAR(res);
            assert(res != R_UnboundValue);

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

        INSTRUCTION(call_) {
            Immediate id = readImmediate();
            advanceImmediate();
            Immediate n = readImmediate();
            advanceImmediate();
            // get the closure itself
            res = ostack_at(ctx, 0);
            res = doCall(c, res, n, id, env, ctx);
            ostack_pop(ctx);
            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(call_stack_) {
            Immediate id = readImmediate();
            advanceImmediate();
            Immediate n = readImmediate();
            advanceImmediate();
            res = ostack_at(ctx, n);
            res = doCallStack(c, res, n, id, env, ctx);
            ostack_pop(ctx); // callee
            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(static_call_stack_) {
            Immediate id = readImmediate();
            advanceImmediate();
            Immediate n = readImmediate();
            advanceImmediate();
            res = cp_pool_at(ctx, *CallSite_target(CallSite_get(c, id)));
            res = doCallStack(c, res, n, id, env, ctx);
            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(dispatch_stack_) {
            Immediate id = readImmediate();
            advanceImmediate();
            Immediate n = readImmediate();
            advanceImmediate();
            ostack_push(ctx, doDispatchStack(c, n, id, env, ctx));
            NEXT();
        }

        INSTRUCTION(dispatch_) {
            Immediate id = readImmediate();
            advanceImmediate();
            Immediate n = readImmediate();
            advanceImmediate();
            ostack_push(ctx, doDispatch(c, n, id, env, ctx));
            NEXT();
        }

        INSTRUCTION(close_) {
            SEXP srcref = ostack_at(ctx, 0);
            SEXP body = ostack_at(ctx, 1);
            SEXP formals = ostack_at(ctx, 2);
            res = allocSExp(CLOSXP);

            assert(isValidFunctionSEXP(body));
            // Make sure to use the most optimized version of this function
            while ((sexp2function(body))->next)
                body = ((sexp2function(body))->next);
            assert(isValidFunctionSEXP(body));

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
            Code* promiseCode = codeAt(code2function(c), id);
            // create the promise and push it on stack
            ostack_push(ctx, createPromise(promiseCode, env));
            NEXT();
        }

        INSTRUCTION(force_) {
            SEXP val = ostack_pop(ctx);
            assert(TYPEOF(val) == PROMSXP);
            // If the promise is already evaluated then push the value inside the promise
            // onto the stack, otherwise push the value from forcing the promise
            ostack_push(ctx, promiseValue(val, ctx));
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
            Code* promiseCode = codeAt(code2function(c), n);
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

        INSTRUCTION(stvar_) {
            Immediate id = readImmediate();
            advanceImmediate();
            int wasChanged = FRAME_CHANGED(env);
            SEXP val = ostack_pop(ctx);

            cachedSetVar(val, env, id, ctx, bindingCache);

            if (!wasChanged)
                CLEAR_FRAME_CHANGED(env);
            NEXT();
        }

        INSTRUCTION(stvar2_) {
            SEXP sym = readConst(ctx, readImmediate());
            advanceImmediate();
            SLOWASSERT(TYPEOF(sym) == SYMSXP);
            SEXP val = ostack_pop(ctx);
            INCREMENT_NAMED(val);
            setVar(sym, val, ENCLOS(env));
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

            if (IS_SIMPLE_SCALAR(lhs, REALSXP) && IS_SIMPLE_SCALAR(rhs, REALSXP)) {
                double real_res = (*REAL(lhs) == NA_REAL || *REAL(rhs) == NA_REAL)
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

            if (IS_SIMPLE_SCALAR(lhs, REALSXP) && IS_SIMPLE_SCALAR(rhs, REALSXP)) {
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

            if (IS_SIMPLE_SCALAR(lhs, REALSXP) && IS_SIMPLE_SCALAR(rhs, REALSXP)) {
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
                    int_res = (l >= 0 && r > 0) ? l % r
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

        INSTRUCTION(ne_) {
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
            int x1 = asLogical(val);
            res = ScalarLogical(x1);
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
                    cond = INTEGER(val)[0]; // relies on NA_INTEGER == NA_LOGICAL
                    break;
                default:
                    cond = asLogical(val);
                }
            }

            if (cond == NA_LOGICAL) {
                const char* msg =
                    XLENGTH(val)
                        ? (isLogical(val) ? ("missing value where TRUE/FALSE needed")
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
            // otherwise return whatever we had, make sure we do not see bytecode
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

        INSTRUCTION(missing_) {
            SEXP sym = readConst(ctx, readImmediate());
            advanceImmediate();
            SLOWASSERT(TYPEOF(sym) == SYMSXP);
            SLOWASSERT(!DDVAL(sym));
            SEXP val = R_findVarLocInFrame(env, sym).cell;
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
                ostack_push(ctx, R_isMissing(PREXPR(val), PRENV(val)) ? R_TrueValue
                                                                      : R_FalseValue);
            }
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

        INSTRUCTION(subset1_) {
            SEXP idx = ostack_at(ctx, 0);
            SEXP val = ostack_at(ctx, 1);

            SEXP args = CONS_NR(idx, R_NilValue);
            args = CONS_NR(val, args);
            ostack_push(ctx, args);
            res = do_subset_dflt(R_NilValue, R_SubsetSym, args, env);
            ostack_popn(ctx, 3);

            R_Visible = 1;
            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(subset2_) {
            SEXP idx2 = ostack_at(ctx, 0);
            SEXP idx = ostack_at(ctx, 1);
            SEXP val = ostack_at(ctx, 2);

            SEXP args = CONS_NR(idx2, R_NilValue);
            args = CONS_NR(idx, args);
            args = CONS_NR(val, args);
            ostack_push(ctx, args);
            res = do_subset_dflt(R_NilValue, R_SubsetSym, args, env);
            ostack_popn(ctx, 4);

            R_Visible = 1;
            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(subassign_) {
            SEXP val = ostack_at(ctx, 2);
            SEXP idx = ostack_at(ctx, 1);
            SEXP orig = ostack_at(ctx, 0);

            INCREMENT_NAMED(orig);
            SEXP args = CONS_NR(val, R_NilValue);
            args = CONS_NR(idx, args);
            args = CONS_NR(orig, args);
            PROTECT(args);
            res = do_subassign_dflt(R_NilValue, R_SubassignSym, args, env);
            ostack_popn(ctx, 3);
            UNPROTECT(1);

            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(subassign2_) {
            SEXP val = ostack_at(ctx, 2);
            SEXP idx = ostack_at(ctx, 1);
            SEXP orig = ostack_at(ctx, 0);

            unsigned targetI = readImmediate();
            advanceImmediate();

            // Fast case
            if (!MAYBE_SHARED(orig)) {
                SEXPTYPE vectorT = TYPEOF(orig);
                SEXPTYPE valT = TYPEOF(val);
                SEXPTYPE idxT = TYPEOF(idx);

                // Fast case only if
                // 1. index is numerical and scalar
                // 2. vector is real and shape of value fits into real
                //      or vector is int and shape of value is int
                //      or vector is generic
                // 3. value fits into one cell of the vector
                if ((idxT == INTSXP || idxT == REALSXP) && (XLENGTH(idx) == 1) &&   // 1
                    ((vectorT == REALSXP && (valT == REALSXP || valT == INTSXP)) || // 2
                     (vectorT == INTSXP && (valT == INTSXP)) || (vectorT == VECSXP)) &&
                    (XLENGTH(val) == 1 || vectorT == VECSXP)) { // 3

                    // if the target == R_NilValue that means this is a stack allocated
                    // vector
                    SEXP target = cp_pool_at(ctx, targetI);
                    bool localBinding =
                        (target == R_NilValue) ||
                        !R_VARLOC_IS_NULL(R_findVarLocInFrame(env, target));

                    if (localBinding) {
                        int idx_ = -1;

                        if (idxT == REALSXP) {
                            if (*REAL(idx) != NA_REAL)
                                idx_ = (int)*REAL(idx) - 1;
                        } else {
                            if (*INTEGER(idx) != NA_INTEGER)
                                idx_ = *INTEGER(idx) - 1;
                        }

                        if (idx_ >= 0 && idx_ < XLENGTH(orig)) {
                            switch (vectorT) {
                            case REALSXP:
                                REAL(orig)[idx_] = valT == REALSXP
                                                       ? *REAL(val)
                                                       : (double)*INTEGER(val);
                                break;
                            case INTSXP:
                                INTEGER(orig)[idx_] = *INTEGER(val);
                                break;
                            case VECSXP:
                                SET_VECTOR_ELT(orig, idx_, val);
                                break;
                            }
                            ostack_popn(ctx, 3);

                            // this is a very nice and dirty hack...
                            // if the next instruction is a matching stvar
                            // (which is highly probably) then we do not
                            // have to execute it, since we changed the value inline
                            if (target != R_NilValue && *pc == stvar_ &&
                                *(int*)(pc - sizeof(int)) == *(int*)(pc + 1)) {
                                pc = pc + sizeof(int) + 1;
                                if (NAMED(orig) == 0)
                                    SET_NAMED(orig, 1);
                            } else {
                                ostack_push(ctx, orig);
                            }
                            NEXT();
                        }
                    }
                }
            }

            INCREMENT_NAMED(orig);
            SEXP args = CONS_NR(val, R_NilValue);
            args = CONS_NR(idx, args);
            args = CONS_NR(orig, args);
            PROTECT(args);
            res = do_subassign2_dflt(R_NilValue, R_Subassign2Sym, args, env);
            ostack_popn(ctx, 3);
            UNPROTECT(1);

            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(extract1_) {
            SEXP idx = ostack_at(ctx, 0);
            SEXP val = ostack_at(ctx, 1);

            if (getAttrib(val, R_NamesSymbol) != R_NilValue || ATTRIB(idx) != R_NilValue)
                goto fallback;

            int i = -1;
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

            R_Visible = 1;
            ostack_popn(ctx, 2);
            ostack_push(ctx, res);
            NEXT();

        // ---------
            fallback : {
                SEXP args = CONS_NR(idx, R_NilValue);
                args = CONS_NR(val, args);
                ostack_push(ctx, args);
                res = do_subset2_dflt(R_NilValue, R_Subset2Sym, args, env);
                ostack_popn(ctx, 3);

                R_Visible = 1;
                ostack_push(ctx, res);
                NEXT();
            }
        }

        INSTRUCTION(extract2_) {
            SEXP idx2 = ostack_at(ctx, 0);
            SEXP idx = ostack_at(ctx, 1);
            SEXP val = ostack_at(ctx, 2);

            SEXP args = CONS_NR(idx2, R_NilValue);
            args = CONS_NR(idx, args);
            args = CONS_NR(val, args);
            ostack_push(ctx, args);
            res = do_subset_dflt(R_NilValue, R_Subset2Sym, args, env);
            ostack_popn(ctx, 4);

            R_Visible = 1;
            ostack_push(ctx, res);
            NEXT();
        }

        INSTRUCTION(guard_env_) {
            uint32_t deoptId = readImmediate();
            advanceImmediate();
            if (FRAME_CHANGED(env) || FRAME_LEAKED(env)) {
                Function* fun = code2function(c);
                assert(bodyCode(fun) == c && "Cannot deopt from promise");
                fun->deopt = true;
                SEXP val = fun->origin;
                Function* deoptFun = sexp2function(val);
                Code* deoptCode = bodyCode(deoptFun);
                c = deoptCode;
                pc = Deoptimizer_pc(deoptId);
                PC_BOUNDSCHECK(pc, c);
            }
            NEXT();
        }

        INSTRUCTION(guard_fun_) {
            SEXP sym = readConst(ctx, readImmediate());
            advanceImmediate();
            res = readConst(ctx, readImmediate());
            advanceImmediate();
            advanceImmediate();
    #ifndef UNSOUND_OPTS
            assert(res == findFun(sym, env) && "guard_fun_ fail");
    #endif
            NEXT();
        }

        INSTRUCTION(seq_) {
            static SEXP prim = NULL;
            if (!prim) {
                // TODO: we could call seq.default here, but it messes up the error
                // call :(
                prim = findFun(Rf_install("seq"), R_GlobalEnv);
            }

            // TODO: add a real guard here...
            assert(prim == findFun(Rf_install("seq"), env));

            SEXP from = ostack_at(ctx, 2);
            SEXP to = ostack_at(ctx, 1);
            SEXP by = ostack_at(ctx, 0);
            res = NULL;

            if (IS_SIMPLE_SCALAR(from, INTSXP) && IS_SIMPLE_SCALAR(to, INTSXP) &&
                IS_SIMPLE_SCALAR(by, INTSXP)) {
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
                SEXP argslist = CONS_NR(from, CONS_NR(to, CONS_NR(by, R_NilValue)));
                ostack_push(ctx, argslist);
                res = applyClosure(call, prim, argslist, env, R_NilValue);
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
                    if (from != NA_INTEGER && to != NA_REAL &&
                            R_FINITE(to) &&	INT_MIN <= to &&
                            INT_MAX >= to && to == (int)to) {
                        res = seq_int(from, (int)to);
                    }
                }
            } else if (IS_SIMPLE_SCALAR(lhs, REALSXP)) {
                double from = *REAL(lhs);
                if (IS_SIMPLE_SCALAR(rhs, INTSXP)) {
                    int to = *INTEGER(rhs);
                    if (from != NA_REAL && to != NA_INTEGER &&
                            R_FINITE(from) &&	INT_MIN <= from &&
                            INT_MAX >= from && from == (int)from) {
                        res = seq_int((int)from, to);
                    }
                } else if (IS_SIMPLE_SCALAR(rhs, REALSXP)) {
                    double to = *REAL(rhs);
                    if (from != NA_REAL && to != NA_REAL &&
                            R_FINITE(from) && R_FINITE(to) &&
                            INT_MIN <= from && INT_MAX >= from &&
                            INT_MIN <= to && INT_MAX >= to &&
                            from == (int)from && to == (int)to) {
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

        INSTRUCTION(test_bounds_) {
            SEXP val = ostack_at(ctx, 1);
            SEXP idx = ostack_at(ctx, 0);
            // TODO: we should extract the length just once at the begining of
            // the loop and generally have somthing more clever here...
            R_xlen_t len;
            if (isVector(val)) {
                len = LENGTH(val);
            } else if (isList(val) || isNull(val)) {
                len = Rf_length(val);
            } else {
                errorcall(R_NilValue, "invalid for() loop sequence");
            }
            int x1 = asInteger(idx);
            ostack_push(ctx, x1 > 0 && x1 <= len ? R_TrueValue : R_FalseValue);
            NEXT();
        }

        INSTRUCTION(visible_) {
            R_Visible = 1;
            NEXT();
        }

        INSTRUCTION(invisible_) {
            R_Visible = 0;
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
            OpcodeT** oldPc = (OpcodeT**)(cntxt + 1);
            *oldPc = pc;

            Rf_begincontext(cntxt, CTXT_LOOP, R_NilValue, env, R_BaseEnv,
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
                OpcodeT** oldPc = (OpcodeT**)(cntxt + 1);
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
            Rf_findcontext(CTXT_BROWSER | CTXT_FUNCTION, env, res);
            NEXT();
        }

        INSTRUCTION(ret_) {
            goto eval_done;
        }

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
    if (isValidFunctionObject(f)) {
        Function* ff = sexp2function(f);
        return src_pool_at(globalContext(), bodyCode(ff)->src);
    }
    return f;
}

SEXP rirEval_f(SEXP f, SEXP env) {
    assert(TYPEOF(f) == EXTERNALSXP);
    // TODO we do not really need the arg counts now
    if (isValidCodeObject(f)) {
        Code* c = (Code*)f;
        SEXP x = evalRirCode(c, globalContext(), env, 0);
        return x;
    } else {
        Function* ff = sexp2function(f);
        return evalRirCode(bodyCode(ff), globalContext(), env, 0);
    }
}
