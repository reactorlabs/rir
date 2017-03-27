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

// bytecode accesses

INLINE Opcode readOpcode(OpcodeT** pc) {
    Opcode result = *(OpcodeT*)(*pc);
    *pc += sizeof(OpcodeT);
    return result;
}

INLINE unsigned readImmediate(OpcodeT** pc) {
    unsigned result = *(Immediate*)*pc;
    *pc += sizeof(Immediate);
    return result;
}

INLINE int readSignedImmediate(OpcodeT** pc) {
    int result = *(SignedImmediate*)*pc;
    *pc += sizeof(SignedImmediate);
    return result;
}

INLINE SEXP readConst(Context* ctx, OpcodeT** pc) {
    return cp_pool_at(ctx, readImmediate(pc));
}

INLINE int readJumpOffset(OpcodeT** pc) {
    int result = *(JumpOffset*)(*pc);
    *pc += sizeof(JumpOffset);
    return result;
}

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

// TODO remove numArgs and bp -- this is only needed for the on stack argument
// handling
#define INSTRUCTION(name)                                                      \
    INLINE void ins_##name(Code** c, SEXP env, OpcodeT** pc, Context* ctx,     \
                           unsigned numArgs)

INSTRUCTION(push_) {
    SEXP x = readConst(ctx, pc);
    R_Visible = TRUE;
    ostack_push(ctx, x);
}

static void jit(SEXP cls, Context* ctx) {
    assert(TYPEOF(cls) == CLOSXP);
    if (TYPEOF(BODY(cls)) == EXTERNALSXP)
        return;
    SEXP cmp = ctx->compiler(cls, NULL);
    SET_BODY(cls, BODY(cmp));
    SET_FORMALS(cls, FORMALS(cmp));
}

INSTRUCTION(ldfun_) {
    SEXP sym = readConst(ctx, pc);
    SEXP val = findFun(sym, env);

    // TODO something should happen here
    if (val == R_UnboundValue)
        assert(false && "Unbound var");
    else if (val == R_MissingArg)
        assert(false && "Missing argument");

    switch (TYPEOF(val)) {
    case CLOSXP:
        jit(val, ctx);
        break;
    case SPECIALSXP:
    case BUILTINSXP:
        // special and builtin functions are ok
        break;
    default:
	error("attempt to apply non-function");
    }
    ostack_push(ctx, val);
}

INSTRUCTION(ldddvar_) {
    SEXP sym = readConst(ctx, pc);
    SEXP val = Rf_ddfindVar(sym, env);
    R_Visible = TRUE;

    // TODO better errors
    if (val == R_UnboundValue) {
        Rf_error("object not found");
    } else if (val == R_MissingArg) {
        error("argument is missing, with no default");
    }

    // if promise, evaluate & return
    if (TYPEOF(val) == PROMSXP)
        val = promiseValue(val, ctx);

    // WTF? is this just defensive programming or what?
    if (NAMED(val) == 0 && val != R_NilValue)
        SET_NAMED(val, 1);

    ostack_push(ctx, val);
}

INSTRUCTION(ldlval_) {
    SEXP sym = readConst(ctx, pc);
    SEXP val = findVarInFrame(env, sym);
    R_Visible = TRUE;

    if (TYPEOF(val) == PROMSXP)
        val = PRVALUE(val);

    assert(val != R_UnboundValue);
    assert(val != R_MissingArg);

    // WTF? is this just defensive programming or what?
    if (NAMED(val) == 0 && val != R_NilValue)
        SET_NAMED(val, 1);

    ostack_push(ctx, val);
}

INSTRUCTION(ldarg_) {
    SEXP sym = readConst(ctx, pc);
    SEXP val = findVarInFrame(env, sym);
    R_Visible = TRUE;

    if (val == R_UnboundValue) {
        Rf_error("object not found");
    } else if (val == R_MissingArg) {
        Rf_error("argument \"%s\" is missing, with no default",
                 CHAR(PRINTNAME(sym)));
    }

    // if promise, evaluate & return
    if (TYPEOF(val) == PROMSXP)
        val = promiseValue(val, ctx);

    // WTF? is this just defensive programming or what?
    if (NAMED(val) == 0 && val != R_NilValue)
        SET_NAMED(val, 1);

    ostack_push(ctx, val);
}

INSTRUCTION(ldvar_) {
    SEXP sym = readConst(ctx, pc);
    SEXP val = findVar(sym, env);
    R_Visible = TRUE;

    if (val == R_UnboundValue) {
        Rf_error("object not found");
    } else if (val == R_MissingArg) {
        Rf_error("argument \"%s\" is missing, with no default", CHAR(PRINTNAME(sym)));
    }

    // if promise, evaluate & return
    if (TYPEOF(val) == PROMSXP)
        val = promiseValue(val, ctx);

    // WTF? is this just defensive programming or what?
    if (NAMED(val) == 0 && val != R_NilValue)
        SET_NAMED(val, 1);

    ostack_push(ctx, val);
}

void closureDebug(SEXP call, SEXP op, SEXP rho, SEXP newrho,
                  RCNTXT* cntxt){// TODO!!!
};

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
                SEXP arg = evalRirCode(codeAt(function(c), argi), ctx, env, 0);
                assert(TYPEOF(arg) != PROMSXP);
                __listAppend(&result, &pos, arg, name);
            } else {
                Code* arg = codeAt(function(c), argi);
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
                           unsigned nargs, OpcodeT** pc, Context* ctx) {

    SEXP body = BODY(callee);
    Function* fun = (Function*)INTEGER(body);

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
        Code* code = functionCode(fun);
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
            fun = (Function*)INTEGER(body);
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
    Code* code = functionCode(fun);

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

void profileCall(CallSiteStruct* cs, SEXP callee) {
    if (!cs->hasProfile)
        return;

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
            OpcodeT** pc, Context* ctx) {

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
                rirCallClosure(call, env, callee, argslist, nargs, pc, ctx);
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
                 OpcodeT** pc, Context* ctx) {

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
            res = rirCallClosure(call, env, callee, argslist, nargs, pc, ctx);
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
                     OpcodeT** pc, Context* ctx) {

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
                    rirCallClosure(call, env, callee, actuals, nargs, pc, ctx);
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
                OpcodeT** pc, Context* ctx) {

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
                res = rirCallClosure(call, env, callee, actuals, nargs, pc, ctx);
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

INSTRUCTION(call_stack_) {
    unsigned id = readImmediate(pc);
    unsigned nargs = readImmediate(pc);
    SEXP callee = ostack_at(ctx, nargs);
    SEXP res = doCallStack(*c, callee, nargs, id, env, pc, ctx);
    ostack_pop(ctx); // callee
    ostack_push(ctx, res);
}

INSTRUCTION(static_call_stack_) {
    unsigned id = readImmediate(pc);
    unsigned nargs = readImmediate(pc);
    SEXP callee = cp_pool_at(ctx, *CallSite_target(CallSite_get(*c, id)));
    ostack_push(ctx, doCallStack(*c, callee, nargs, id, env, pc, ctx));
}

INSTRUCTION(call_) {
    unsigned id = readImmediate(pc);
    unsigned nargs = readImmediate(pc);
    // get the closure itself
    SEXP cls = ostack_pop(ctx);
    PROTECT(cls);
    ostack_push(ctx, doCall(*c, cls, nargs, id, env, pc, ctx));
    UNPROTECT(1);
}

INSTRUCTION(dispatch_stack_) {
    unsigned id = readImmediate(pc);
    unsigned nargs = readImmediate(pc);
    ostack_push(ctx, doDispatchStack(*c, nargs, id, env, pc, ctx));
}

INSTRUCTION(dispatch_) {
    unsigned id = readImmediate(pc);
    unsigned nargs = readImmediate(pc);
    ostack_push(ctx, doDispatch(*c, nargs, id, env, pc, ctx));
}

INSTRUCTION(promise_) {
    // get the Code * pointer we need
    unsigned codeOffset = readImmediate(pc);
    Code* promiseCode = codeAt(function(*c), codeOffset);
    // create the promise and push it on stack
    ostack_push(ctx, createPromise(promiseCode, env));
}

INSTRUCTION(push_code_) {
    // get the Code * pointer we need
    unsigned codeOffset = readImmediate(pc);
    Code* promiseCode = codeAt(function(*c), codeOffset);
    // create the promise and push it on stack
    ostack_push(ctx, (SEXP)promiseCode);
}

INSTRUCTION(close_) {
    SEXP srcref = ostack_at(ctx, 0);
    SEXP body = ostack_at(ctx, 1);
    SEXP formals = ostack_at(ctx, 2);
    SEXP result = allocSExp(CLOSXP);

    assert(isValidFunctionSEXP(body));
    // Make sure to use the most optimized version of this function
    while (((Function*)INTEGER(body))->next)
        body = ((Function*)INTEGER(body))->next;
    assert(isValidFunctionSEXP(body));

    SET_FORMALS(result, formals);
    SET_BODY(result, body);
    SET_CLOENV(result, env);
    Rf_setAttrib(result, Rf_install("srcref"), srcref);
    ostack_popn(ctx, 3);
    ostack_push(ctx, result);
}

INSTRUCTION(force_) {
    SEXP p = ostack_pop(ctx);
    assert(TYPEOF(p) == PROMSXP);
    // If the promise is already evaluated then push the value inside the
    // promise
    // onto the stack, otherwise push the value from forcing the promise
    ostack_push(ctx, promiseValue(p, ctx));
}

INSTRUCTION(pop_) { ostack_pop(ctx); }

INSTRUCTION(return_) {
    SEXP res = ostack_top(ctx);
    Rf_findcontext(CTXT_BROWSER | CTXT_FUNCTION, env, res);
}

INSTRUCTION(asast_) {
    SEXP p = ostack_pop(ctx);
    assert(TYPEOF(p) == PROMSXP);
    SEXP ast = PRCODE(p);
    // if the code is NILSXP then it is rir Code object, get its ast
    if (TYPEOF(ast) == NILSXP)
        ast = cp_pool_at(ctx, ((Code*)ast)->src);
    // otherwise return whatever we had, make sure we do not see bytecode
    assert(TYPEOF(ast) != BCODESXP);
    ostack_push(ctx, ast);
}

INSTRUCTION(int3_) { asm("int3"); }

INSTRUCTION(swap_) {
    SEXP a = ostack_pop(ctx);
    SEXP b = ostack_pop(ctx);
    ostack_push(ctx, a);
    ostack_push(ctx, b);
}

INSTRUCTION(put_) {
    uint32_t i = readImmediate(pc);
    R_bcstack_t* pos = ostack_cell_at(ctx, 0);
    SEXP val = pos->u.sxpval;
    while (i--) {
        pos->u.sxpval = (pos - 1)->u.sxpval;
        pos--;
    }
    pos->u.sxpval = val;
}

INSTRUCTION(pick_) {
    uint32_t i = readImmediate(pc);
    R_bcstack_t* pos = ostack_cell_at(ctx, i);
    SEXP val = pos->u.sxpval;
    while (i--) {
        pos->u.sxpval = (pos + 1)->u.sxpval;
        pos++;
    }
    pos->u.sxpval = val;
}

INSTRUCTION(pull_) {
    uint32_t i = readImmediate(pc);
    SEXP val = ostack_at(ctx, i);
    ostack_push(ctx, val);
}

INSTRUCTION(is_) {
    SEXP test = ostack_pop(ctx);
    uint32_t i = readImmediate(pc);
    bool res;
    switch (i) {
    case NILSXP:
    case LGLSXP:
    case REALSXP:
        res = TYPEOF(test) == i;
        break;

    case VECSXP:
        res = TYPEOF(test) == VECSXP || TYPEOF(test) == LISTSXP;
        break;

    case LISTSXP:
        res = TYPEOF(test) == LISTSXP || TYPEOF(test) == NILSXP;
        break;

    default:
        assert(false);
        break;
    }
    ostack_push(ctx, res ? R_TrueValue : R_FalseValue);
}

INLINE SEXP findRootPromise(SEXP p) {
    if (TYPEOF(p) == PROMSXP) {
        while (TYPEOF(PREXPR(p)) == PROMSXP) {
            p = PREXPR(p);
        }
    }
    return p;
}

INSTRUCTION(missing_) {
    SEXP sym = readConst(ctx, pc);
    SLOWASSERT(TYPEOF(sym) == SYMSXP);
    SLOWASSERT(!DDVAL(sym));
    SEXP bind = R_findVarLocInFrame(env, sym).cell;
    if (bind == NULL)
        errorcall(getSrcAt(*c, *pc - 1, ctx),
                  "'missing' can only be used for arguments");

    if (MISSING(bind) || CAR(bind) == R_MissingArg) {
        ostack_push(ctx, R_TrueValue);
        return;
    }

    SEXP val = CAR(bind);

    if (TYPEOF(val) != PROMSXP) {
        ostack_push(ctx, R_FalseValue);
        return;
    }

    SEXP t = findRootPromise(val);
    if (!isSymbol(PREXPR(t)))
        ostack_push(ctx, R_FalseValue);
    else {
        ostack_push(ctx, R_isMissing(PREXPR(t), PRENV(t)) ? R_TrueValue
                                                          : R_FalseValue);
    }
}

INSTRUCTION(stvar_) {
    SEXP sym = readConst(ctx, pc);
    int wasChanged = FRAME_CHANGED(env);
    SLOWASSERT(TYPEOF(sym) == SYMSXP);
    SEXP val = ostack_pop(ctx);
    INCREMENT_NAMED(val);
    defineVar(sym, val, env);
    if (!wasChanged)
        CLEAR_FRAME_CHANGED(env);
}

INSTRUCTION(aslogical_) {
    SEXP t = ostack_top(ctx);
    int r = asLogical(t);
    SEXP res = ScalarLogical(r);
    ostack_pop(ctx);
    ostack_push(ctx, res);
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
}

INSTRUCTION(asbool_) {
    SEXP t = ostack_top(ctx);
    int cond = NA_LOGICAL;
    if (XLENGTH(t) > 1)
        warningcall(getSrcAt(*c, *pc - 1, ctx),
                    ("the condition has length > 1 and only the first "
                     "element will be used"));

    if (XLENGTH(t) > 0) {
        switch (TYPEOF(t)) {
        case LGLSXP:
            cond = LOGICAL(t)[0];
            break;
        case INTSXP:
            cond = INTEGER(t)[0]; // relies on NA_INTEGER == NA_LOGICAL
        default:
            cond = asLogical(t);
        }
    }

    if (cond == NA_LOGICAL) {
        const char* msg =
            Rf_length(t)
                ? (isLogical(t) ? ("missing value where TRUE/FALSE needed")
                                : ("argument is not interpretable as logical"))
                : ("argument is of length zero");
        errorcall(getSrcAt(*c, *pc - 1, ctx), msg);
    }

    ostack_pop(ctx);
    ostack_push(ctx, cond ? R_TrueValue : R_FalseValue);
}

INSTRUCTION(brobj_) {
    int offset = readJumpOffset(pc);
    if (OBJECT(ostack_top(ctx)))
        *pc = *pc + offset;
    PC_BOUNDSCHECK(*pc, *c);
}

INSTRUCTION(endcontext_) {
    SEXP cntxt_store = ostack_top(ctx);
    assert(TYPEOF(cntxt_store) == RAWSXP);
    RCNTXT* cntxt = (RCNTXT*)RAW(cntxt_store);
    Rf_endcontext(cntxt);
    ostack_pop(ctx); // Context
}

INLINE void incPerfCount(Code* c) {
    if (c->perfCounter < UINT_MAX) {
        c->perfCounter++;
        // if (c->perfCounter == 200000)
        //     printCode(c);
    }
}

INSTRUCTION(brtrue_) {
    int offset = readJumpOffset(pc);
    if (ostack_pop(ctx) == R_TrueValue) {
        *pc = *pc + offset;
        if (offset < 0)
            incPerfCount(*c);
    }
    PC_BOUNDSCHECK(*pc, *c);
}

INSTRUCTION(brfalse_) {
    int offset = readJumpOffset(pc);
    if (ostack_pop(ctx) == R_FalseValue) {
        *pc = *pc + offset;
        if (offset < 0)
            incPerfCount(*c);
    }
    PC_BOUNDSCHECK(*pc, *c);
}

INSTRUCTION(br_) {
    int offset = readJumpOffset(pc);
    if (offset < 0)
        incPerfCount(*c);
    *pc = *pc + offset;
    PC_BOUNDSCHECK(*pc, *c);
}

INSTRUCTION(subassign2_) {
    SEXP val = ostack_at(ctx, 2);
    SEXP idx = ostack_at(ctx, 1);
    SEXP orig = ostack_at(ctx, 0);

    unsigned targetI = readImmediate(pc);
    SEXP res;

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
                    if (target != R_NilValue && **pc == stvar_ &&
                        *(int*)(*pc - sizeof(int)) == *(int*)(*pc + 1)) {
                        *pc = *pc + sizeof(int) + 1;
                        if (NAMED(orig) == 0)
                            SET_NAMED(orig, 1);
                    } else {
                        ostack_push(ctx, orig);
                    }
                    return;
                }
            }
        }
    }

    INCREMENT_NAMED(orig);
    SEXP args;
    args = CONS_NR(val, R_NilValue);
    args = CONS_NR(idx, args);
    args = CONS_NR(orig, args);
    PROTECT(args);
    res = do_subassign2_dflt(R_NilValue, R_Subassign2Sym, args, env);
    ostack_popn(ctx, 3);
    UNPROTECT(1);

    ostack_push(ctx, res);
}

INSTRUCTION(subassign_) {
    SEXP val = ostack_at(ctx, 2);
    SEXP idx = ostack_at(ctx, 1);
    SEXP orig = ostack_at(ctx, 0);

    SEXP res;

    INCREMENT_NAMED(orig);
    SEXP args;
    args = CONS_NR(val, R_NilValue);
    args = CONS_NR(idx, args);
    args = CONS_NR(orig, args);
    PROTECT(args);
    res = do_subassign_dflt(R_NilValue, R_SubassignSym, args, env);
    ostack_popn(ctx, 3);
    UNPROTECT(1);

    ostack_push(ctx, res);
}

INSTRUCTION(subset2_) {
    SEXP idx2 = ostack_at(ctx, 0);
    SEXP idx1 = ostack_at(ctx, 1);
    SEXP val = ostack_at(ctx, 2);

    SEXP res;
    SEXP args;
    args = CONS_NR(idx2, R_NilValue);
    args = CONS_NR(idx1, args);
    args = CONS_NR(val, args);
    ostack_push(ctx, args);
    res = do_subset_dflt(R_NilValue, R_SubsetSym, args, env);
    ostack_popn(ctx, 4);

    R_Visible = 1;
    ostack_push(ctx, res);
}

INSTRUCTION(extract2_) {
    SEXP idx2 = ostack_at(ctx, 0);
    SEXP idx1 = ostack_at(ctx, 1);
    SEXP val = ostack_at(ctx, 2);

    SEXP res;
    SEXP args;

    args = CONS_NR(idx2, R_NilValue);
    args = CONS_NR(idx1, args);
    args = CONS_NR(val, args);
    ostack_push(ctx, args);
    res = do_subset_dflt(R_NilValue, R_Subset2Sym, args, env);
    ostack_popn(ctx, 4);

    R_Visible = 1;
    ostack_push(ctx, res);
}

INSTRUCTION(subset1_) {
    SEXP idx = ostack_at(ctx, 0);
    SEXP val = ostack_at(ctx, 1);

    SEXP res;
    SEXP args;

    args = CONS_NR(idx, R_NilValue);
    args = CONS_NR(val, args);
    ostack_push(ctx, args);
    res = do_subset_dflt(R_NilValue, R_SubsetSym, args, env);
    ostack_popn(ctx, 3);

    R_Visible = 1;
    ostack_push(ctx, res);
}

INSTRUCTION(extract1_) {
    SEXP idx = ostack_at(ctx, 0);
    SEXP val = ostack_at(ctx, 1);

    SEXP res;
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
        break;
    }

    if (i >= XLENGTH(val) || i < 0)
        goto fallback;

    switch (TYPEOF(val)) {

#define SIMPLECASE(vectype, vecaccess)                                         \
    case vectype: {                                                            \
        if (SHORT_VEC_LENGTH(val) == 1 && !MAYBE_SHARED(val))                  \
            res = val;                                                         \
        else                                                                   \
            res = allocVector(vectype, 1);                                     \
        vecaccess(res)[0] = vecaccess(val)[i];                                 \
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
    return;

// ---------
    fallback : {
        SEXP args;
        args = CONS_NR(idx, R_NilValue);
        args = CONS_NR(val, args);
        ostack_push(ctx, args);
        res = do_subset2_dflt(R_NilValue, R_Subset2Sym, args, env);
        ostack_popn(ctx, 3);
    }

    R_Visible = 1;
    ostack_push(ctx, res);
}

INSTRUCTION(dup_) { ostack_push(ctx, ostack_top(ctx)); }

INSTRUCTION(guard_env_) {
    uint32_t deoptId = readImmediate(pc);
    if (FRAME_CHANGED(env) || FRAME_LEAKED(env)) {
        Function* fun = function(*c);
        assert(functionCode(fun) == *c && "Cannot deopt from promise");
        fun->deopt = true;
        SEXP deopt = fun->origin;
        Function* deoptFun = (Function*)INTEGER(deopt);
        Code* deoptCode = functionCode(deoptFun);
        *c = deoptCode;
        *pc = Deoptimizer_pc(deoptId);
        PC_BOUNDSCHECK(*pc, *c);
    }
}

INSTRUCTION(guard_fun_) {
    SEXP sym = readConst(ctx, pc);
    SEXP expected = readConst(ctx, pc);
    readImmediate(pc);
    SEXP val = findFun(sym, env);
    assert(val == expected);
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
}

INSTRUCTION(inc_) {
    SEXP n = ostack_top(ctx);
    assert(TYPEOF(n) == INTSXP);
    int i = INTEGER(n)[0];
    if (MAYBE_SHARED(n)) {
        ostack_pop(ctx);
        SEXP nn = Rf_allocVector(INTSXP, 1);
        INTEGER(nn)[0] = i + 1;
        ostack_push(ctx, nn);
    } else {
        INTEGER(n)[0]++;
    }
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
            SEXP call = getSrcForCall(*c, *pc - 1, ctx);                       \
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
        SEXP call = getSrcForCall(*c, *pc - 1, ctx);                           \
        SEXP argslist = CONS_NR(lhs, CONS_NR(rhs, R_NilValue));                \
        ostack_push(ctx, argslist);                                            \
        if (flag < 2)                                                          \
            R_Visible = flag != 1;                                             \
        res = blt(call, prim, argslist, env);                                  \
        if (flag < 2)                                                          \
            R_Visible = flag != 1;                                             \
        ostack_pop(ctx);                                                       \
    } while (false)

#define IS_SCALAR_VALUE(e, type)                                               \
    (TYPEOF(e) == type && SHORT_VEC_LENGTH(e) == 1 && ATTRIB(e) == R_NilValue)

#define DO_BINOP(op, op2)                                                      \
    do {                                                                       \
        if (IS_SCALAR_VALUE(lhs, REALSXP)) {                                   \
            if (IS_SCALAR_VALUE(rhs, REALSXP)) {                               \
                res = Rf_allocVector(REALSXP, 1);                              \
                *REAL(res) = (*REAL(lhs) == NA_REAL || *REAL(rhs) == NA_REAL)  \
                                 ? NA_REAL                                     \
                                 : *REAL(lhs) op * REAL(rhs);                  \
                break;                                                         \
            } else if (IS_SCALAR_VALUE(rhs, INTSXP)) {                         \
                res = Rf_allocVector(REALSXP, 1);                              \
                *REAL(res) =                                                   \
                    (*REAL(lhs) == NA_REAL || *INTEGER(rhs) == NA_INTEGER)     \
                        ? NA_REAL                                              \
                        : *REAL(lhs) op * INTEGER(rhs);                        \
                break;                                                         \
            }                                                                  \
        } else if (IS_SCALAR_VALUE(lhs, INTSXP)) {                             \
            if (IS_SCALAR_VALUE(rhs, INTSXP)) {                                \
                Rboolean naflag = FALSE;                                       \
                res = Rf_allocVector(INTSXP, 1);                               \
                switch (op2) {                                                 \
                case PLUSOP:                                                   \
                    *INTEGER(res) =                                            \
                        R_integer_plus(*INTEGER(lhs), *INTEGER(rhs), &naflag); \
                    break;                                                     \
                case MINUSOP:                                                  \
                    *INTEGER(res) = R_integer_minus(*INTEGER(lhs),             \
                                                    *INTEGER(rhs), &naflag);   \
                    break;                                                     \
                case TIMESOP:                                                  \
                    *INTEGER(res) = R_integer_times(*INTEGER(lhs),             \
                                                    *INTEGER(rhs), &naflag);   \
                    break;                                                     \
                }                                                              \
                CHECK_INTEGER_OVERFLOW(res, naflag);                           \
                break;                                                         \
            } else if (IS_SCALAR_VALUE(rhs, REALSXP)) {                        \
                res = Rf_allocVector(REALSXP, 1);                              \
                *REAL(res) =                                                   \
                    (*INTEGER(lhs) == NA_INTEGER || *REAL(rhs) == NA_REAL)     \
                        ? NA_REAL                                              \
                        : *INTEGER(lhs) op * REAL(rhs);                        \
                break;                                                         \
            }                                                                  \
        }                                                                      \
        BINOP_FALLBACK(#op);                                                   \
    } while (false)

INSTRUCTION(mul_) {
    SEXP lhs = ostack_at(ctx, 1);
    SEXP rhs = ostack_at(ctx, 0);
    SEXP res;

    DO_BINOP(*, TIMESOP);

    ostack_popn(ctx, 2);
    ostack_push(ctx, res);
}

INSTRUCTION(div_) {
    SEXP lhs = ostack_at(ctx, 1);
    SEXP rhs = ostack_at(ctx, 0);
    SEXP res;

    if (IS_SCALAR_VALUE(lhs, REALSXP) && IS_SCALAR_VALUE(rhs, REALSXP)) {
        res = Rf_allocVector(REALSXP, 1);
        *REAL(res) = (*REAL(lhs) == NA_REAL || *REAL(rhs) == NA_REAL)
                         ? NA_REAL
                         : *REAL(lhs) / *REAL(rhs);
    } else if (IS_SCALAR_VALUE(lhs, INTSXP) && IS_SCALAR_VALUE(rhs, INTSXP)) {
        res = Rf_allocVector(REALSXP, 1);
        int l = *INTEGER(lhs);
        int r = *INTEGER(rhs);
        if (l == NA_INTEGER || r == NA_INTEGER)
            *REAL(res) = NA_REAL;
        else
            *REAL(res) = (double)l / (double)r;
    } else {
        BINOP_FALLBACK("/");
    }

    ostack_popn(ctx, 2);
    ostack_push(ctx, res);
}

static double myfloor(double x1, double x2) {
    double q = x1 / x2, tmp;

    if (x2 == 0.0)
        return q;
    tmp = x1 - floor(q) * x2;
    return floor(q) + floor(tmp / x2);
}

INSTRUCTION(idiv_) {
    SEXP lhs = ostack_at(ctx, 1);
    SEXP rhs = ostack_at(ctx, 0);
    SEXP res;

    if (IS_SCALAR_VALUE(lhs, REALSXP) && IS_SCALAR_VALUE(rhs, REALSXP)) {
        res = Rf_allocVector(REALSXP, 1);
        *REAL(res) = myfloor(*REAL(lhs), *REAL(rhs));
    } else if (IS_SCALAR_VALUE(lhs, INTSXP) && IS_SCALAR_VALUE(rhs, INTSXP)) {
        res = Rf_allocVector(INTSXP, 1);
        int l = *INTEGER(lhs);
        int r = *INTEGER(rhs);
        /* This had x %/% 0 == 0 prior to 2.14.1, but
           it seems conventionally to be undefined */
        if (l == NA_INTEGER || r == NA_INTEGER || r == 0)
            *INTEGER(res) = NA_INTEGER;
        else
            *INTEGER(res) = (int)floor((double)l / (double)r);
    } else {
        BINOP_FALLBACK("%/%");
    }

    ostack_popn(ctx, 2);
    ostack_push(ctx, res);
}

INSTRUCTION(pow_) {
    SEXP lhs = ostack_at(ctx, 1);
    SEXP rhs = ostack_at(ctx, 0);
    SEXP res;

    BINOP_FALLBACK("^");

    ostack_popn(ctx, 2);
    ostack_push(ctx, res);
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

INSTRUCTION(mod_) {
    SEXP lhs = ostack_at(ctx, 1);
    SEXP rhs = ostack_at(ctx, 0);
    SEXP res;

    if (IS_SCALAR_VALUE(lhs, REALSXP) && IS_SCALAR_VALUE(rhs, REALSXP)) {
        res = Rf_allocVector(REALSXP, 1);
        *REAL(res) = myfmod(*REAL(lhs), *REAL(rhs));
    } else if (IS_SCALAR_VALUE(lhs, INTSXP) && IS_SCALAR_VALUE(rhs, INTSXP)) {
        res = Rf_allocVector(INTSXP, 1);
        int l = *INTEGER(lhs);
        int r = *INTEGER(rhs);
        if (l == NA_INTEGER || r == NA_INTEGER || r == 0) {
            *INTEGER(res) = NA_INTEGER;
        } else {
            *INTEGER(res) =
                (l >= 0 && r > 0) ? l % r : (int)myfmod((double)l, (double)r);
        }
    } else {
        BINOP_FALLBACK("%%");
    }

    ostack_popn(ctx, 2);
    ostack_push(ctx, res);
}

INSTRUCTION(add_) {
    SEXP lhs = ostack_at(ctx, 1);
    SEXP rhs = ostack_at(ctx, 0);
    SEXP res;

    DO_BINOP(+, PLUSOP);

    ostack_popn(ctx, 2);
    ostack_push(ctx, res);
}

INSTRUCTION(sub_) {
    SEXP lhs = ostack_at(ctx, 1);
    SEXP rhs = ostack_at(ctx, 0);
    SEXP res;

    DO_BINOP(-, MINUSOP);

    ostack_popn(ctx, 2);
    ostack_push(ctx, res);
}


// TODO: what about (REAL op INT) and (INT op REAL) -- fallback as is
#define DO_RELOP(op)                                                           \
    do {                                                                       \
        if (IS_SCALAR_VALUE(lhs, REALSXP)) {                                   \
            if (IS_SCALAR_VALUE(rhs, REALSXP)) {                               \
                if (*REAL(lhs) == NA_REAL || *REAL(rhs) == NA_REAL) {          \
                    res = R_LogicalNAValue;                                    \
                } else {                                                       \
                    res = *REAL(lhs) op *REAL(rhs) ? R_TrueValue               \
                                                   : R_FalseValue;             \
                }                                                              \
                break;                                                         \
            }                                                                  \
        } else if (IS_SCALAR_VALUE(lhs, INTSXP)) {                             \
            if (IS_SCALAR_VALUE(rhs, INTSXP)) {                                \
                if (*INTEGER(lhs) == NA_INTEGER ||                             \
                    *INTEGER(rhs) == NA_INTEGER) {                             \
                    res = R_LogicalNAValue;                                    \
                } else {                                                       \
                    res = *INTEGER(lhs) op *INTEGER(rhs) ? R_TrueValue         \
                                                         : R_FalseValue;       \
                }                                                              \
                break;                                                         \
            }                                                                  \
        }                                                                      \
        BINOP_FALLBACK(#op);                                                   \
    } while (false)


INSTRUCTION(lt_) {
    SEXP lhs = ostack_at(ctx, 1);
    SEXP rhs = ostack_at(ctx, 0);
    SEXP res;

    DO_RELOP(<);

    ostack_popn(ctx, 2);
    ostack_push(ctx, res);
}

INSTRUCTION(gt_) {
    SEXP lhs = ostack_at(ctx, 1);
    SEXP rhs = ostack_at(ctx, 0);
    SEXP res;

    DO_RELOP(>);

    ostack_popn(ctx, 2);
    ostack_push(ctx, res);
}

INSTRUCTION(le_) {
    SEXP lhs = ostack_at(ctx, 1);
    SEXP rhs = ostack_at(ctx, 0);
    SEXP res;

    DO_RELOP(<=);

    ostack_popn(ctx, 2);
    ostack_push(ctx, res);
}

INSTRUCTION(ge_) {
    SEXP lhs = ostack_at(ctx, 1);
    SEXP rhs = ostack_at(ctx, 0);
    SEXP res;

    DO_RELOP(>=);

    ostack_popn(ctx, 2);
    ostack_push(ctx, res);
}

INSTRUCTION(eq_) {
    SEXP lhs = ostack_at(ctx, 1);
    SEXP rhs = ostack_at(ctx, 0);
    SEXP res;

    DO_RELOP(==);

    ostack_popn(ctx, 2);
    ostack_push(ctx, res);
}

INSTRUCTION(ne_) {
    SEXP lhs = ostack_at(ctx, 1);
    SEXP rhs = ostack_at(ctx, 0);
    SEXP res;

    DO_RELOP(!=);

    ostack_popn(ctx, 2);
    ostack_push(ctx, res);
}

INSTRUCTION(names_) {
    ostack_push(ctx, getAttrib(ostack_pop(ctx), R_NamesSymbol));
}

INSTRUCTION(set_names_) {
    SEXP names = ostack_pop(ctx);
    if (!isNull(names))
        setAttrib(ostack_top(ctx), R_NamesSymbol, names);
}

INSTRUCTION(alloc_) {
    SEXP l = ostack_pop(ctx);
    assert(TYPEOF(l) == INTSXP);
    int type = readSignedImmediate(pc);
    SEXP vec = Rf_allocVector(type, INTEGER(l)[0]);
    ostack_push(ctx, vec);
}

INSTRUCTION(length_) {
    SEXP t = ostack_pop(ctx);
    int len = XLENGTH(t);
    ostack_push(ctx, Rf_allocVector(INTSXP, 1));
    INTEGER(ostack_top(ctx))[0] = len;
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
    SEXP res = NULL;

    if (IS_SCALAR_VALUE(from, INTSXP) && IS_SCALAR_VALUE(to, INTSXP) &&
        IS_SCALAR_VALUE(by, INTSXP)) {
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
        SEXP call = getSrcForCall(*c, *pc - 1, ctx);
        SEXP argslist = CONS_NR(from, CONS_NR(to, CONS_NR(by, R_NilValue)));
        ostack_push(ctx, argslist);
        res = applyClosure(call, prim, argslist, env, R_NilValue);
        ostack_pop(ctx);
    }

    ostack_popn(ctx, 3);
    ostack_push(ctx, res);
}

INSTRUCTION(test_bounds_) {
    SEXP vec = ostack_at(ctx, 1);
    SEXP idx = ostack_at(ctx, 0);
    int len = Rf_length(vec);
    int i = asInteger(idx);
    ostack_push(ctx, i > 0 && i <= len ? R_TrueValue : R_FalseValue);
}

INSTRUCTION(dup2_) {
    SEXP a = ostack_at(ctx, 1);
    SEXP b = ostack_at(ctx, 0);
    ostack_push(ctx, a);
    ostack_push(ctx, b);
}

INSTRUCTION(visible_) {
    R_Visible = 1;
}

INSTRUCTION(invisible_) {
    R_Visible = 0;
}

INSTRUCTION(set_shared_) {
    SEXP v = ostack_top(ctx);
    if (NAMED(v) < 2) {
        SET_NAMED(v, 2);
    }
}

INSTRUCTION(make_unique_) {
    SEXP v = ostack_top(ctx);
    if (NAMED(v) == 2) {
        v = shallow_duplicate(v);
        ostack_set(ctx, 0, v);
        SET_NAMED(v, 1);
    }
}

extern void printCode(Code* c);
extern void printFunction(Function* f);

extern SEXP Rf_deparse1(SEXP call, Rboolean abbrev, int opts);

static int debugging = 0;
void debug(Code* c, OpcodeT* pc, const char* name, unsigned depth,
           Context* ctx) {
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

SEXP evalRirCode(Code* c, Context* ctx, SEXP env, unsigned numArgs) {
    assert(c->magic == CODE_MAGIC);

    if (!env)
	error("'rho' cannot be C NULL: detected in C-level eval");
    if (!isEnvironment(env))
	error("'rho' must be an environment not %s: detected in C-level eval",
	      type2char(TYPEOF(env)));

    // make sure there is enough room on the stack
    // there is some slack of 5 to make sure the call instruction can store
    // some intermediate values on the stack
    ostack_ensureSize(ctx, c->stackLength + 5);
    unsigned bp = ostack_length(ctx);

    OpcodeT* pc = code(c);

    R_Visible = TRUE;
    // main loop
    while (true) {
        switch (readOpcode(&pc)) {

#define INS(name)                                                              \
    case name:                                                                 \
        ins_##name(&c, env, &pc, ctx, numArgs);                                \
        debug(c, pc, #name, ostack_length(ctx) - bp, ctx);                     \
        break

            INS(seq_);
            INS(push_);
            INS(ldfun_);
            INS(ldvar_);
            INS(ldlval_);
            INS(ldarg_);
            INS(ldddvar_);
            INS(add_);
            INS(mul_);
            INS(mod_);
            INS(pow_);
            INS(div_);
            INS(idiv_);
            INS(sub_);
            INS(lt_);
            INS(gt_);
            INS(le_);
            INS(ge_);
            INS(eq_);
            INS(ne_);
            INS(call_);
            INS(call_stack_);
            INS(static_call_stack_);
            INS(dispatch_stack_);
            INS(promise_);
            INS(push_code_);
            INS(close_);
            INS(force_);
            INS(pop_);
            INS(return_);
            INS(asast_);
            INS(stvar_);
            INS(missing_);
            INS(subassign_);
            INS(subassign2_);
            INS(asbool_);
            INS(brobj_);
            INS(endcontext_);
            INS(brtrue_);
            INS(brfalse_);
            INS(br_);
            INS(dup_);
            INS(swap_);
            INS(int3_);
            INS(put_);
            INS(pick_);
            INS(pull_);
            INS(is_);
            INS(guard_fun_);
            INS(guard_env_);
            INS(isfun_);
            INS(inc_);
            INS(dup2_);
            INS(test_bounds_);
            INS(invisible_);
            INS(visible_);
            INS(extract1_);
            INS(subset1_);
            INS(extract2_);
            INS(subset2_);
            INS(dispatch_);
            INS(make_unique_);
            INS(set_shared_);
            INS(aslogical_);
            INS(lgl_and_);
            INS(lgl_or_);
            INS(names_);
            INS(set_names_);
            INS(alloc_);
            INS(length_);

        case beginloop_: {
            // Allocate a RCNTXT on the stack
            SEXP cntxt_store =
                Rf_allocVector(RAWSXP, sizeof(RCNTXT) + sizeof(pc));
            ostack_push(ctx, cntxt_store);

            RCNTXT* cntxt = (RCNTXT*)RAW(cntxt_store);

            // (ab)use the same buffe to store the current pc
            OpcodeT** oldPc = (OpcodeT**)(cntxt + 1);
            *oldPc = pc;

            Rf_begincontext(cntxt, CTXT_LOOP, R_NilValue, env, R_BaseEnv,
                            R_NilValue, R_NilValue);
            // (ab)use the unused cenddata field to store sp
            cntxt->cenddata = (void*)ostack_length(ctx);

            readJumpOffset(&pc);

            int s;
            if ((s = SETJMP(cntxt->cjmpbuf))) {
                // incomming non-local break/continue:
                // restore our stack state

                // get the RCNTXT from the stack
                SEXP cntxt_store = ostack_top(ctx);
                assert(TYPEOF(cntxt_store) == RAWSXP && "stack botched");
                RCNTXT* cntxt = (RCNTXT*)RAW(cntxt_store);
                assert(cntxt == R_GlobalContext && "stack botched");
                OpcodeT** oldPc = (OpcodeT**)(cntxt + 1);
                pc = *oldPc;

                int offset = readJumpOffset(&pc);

                if (s == CTXT_BREAK)
                    pc = pc + offset;
                PC_BOUNDSCHECK(pc, c);
            }
            break;
        }

        case ret_: {
            // not in its own function so that we can avoid nonlocal returns
            goto __eval_done;
        }
        default:
            assert(false && "wrong or unimplemented opcode");
        }
    }
__eval_done : {
    return ostack_pop(ctx);
}
}


SEXP rirExpr(SEXP f) {
    if (isValidCodeObject(f)) {
        Code* c = (Code*)f;
        return src_pool_at(globalContext(), c->src);
    }
    if (isValidFunctionObject(f)) {
        Function* ff = (Function*)(INTEGER(f));
        return src_pool_at(globalContext(), functionCode(ff)->src);
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
        Function* ff = (Function*)(INTEGER(f));
        return evalRirCode(functionCode(ff), globalContext(), env, 0);
    }
}
