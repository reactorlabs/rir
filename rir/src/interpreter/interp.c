#include <assert.h>
#include <alloca.h>

#include "interp.h"
#include "interp_context.h"
#include "runtime.h"
#include "R/Funtab.h"


#define NOT_IMPLEMENTED assert(false)

// TODO we are using the RInternals, but soud not when the code moves to GNU-R
// #include "RIntlns.h"

#undef eval

extern SEXP R_TrueValue;
extern SEXP R_FalseValue;
extern SEXP Rf_NewEnvironment(SEXP, SEXP, SEXP);
extern Rboolean R_Visible;

#include <setjmp.h>
#include <signal.h>
#define JMP_BUF jmp_buf
#define SETJMP(x) setjmp(x)
#define LONGJMP(x, i) longjmp(x, i)

/* Evaluation Context Structure */
typedef struct RCNTXT {
    struct RCNTXT* nextcontext; /* The next context up the chain */
    int callflag;               /* The context "type" */
    JMP_BUF cjmpbuf;            /* C stack and register information */
    int cstacktop;              /* Top of the pointer protection stack */
    int evaldepth;              /* evaluation depth at inception */
    SEXP promargs;              /* Promises supplied to closure */
    SEXP callfun;               /* The closure called */
    SEXP sysparent;             /* environment the closure was called from */
    SEXP call;                  /* The call that effected this context*/
    SEXP cloenv;                /* The environment */
    SEXP conexit;               /* Interpreted "on.exit" code */
    void (*cend)(void*);        /* C "on.exit" thunk */
    void* cenddata;             /* data for C "on.exit" thunk */
    void* vmax;                 /* top of R_alloc stack */
    int intsusp;                /* interrupts are suspended */
    SEXP handlerstack;          /* condition handler stack */
    SEXP restartstack;          /* stack of available restarts */
    struct RPRSTACK* prstack;   /* stack of pending promises */
    void* nodestack;
    // Since we don't know if the R we are linked against has an INT stack, we
    // have to be conservative from here on....
    void* dontuse1;
    SEXP dontuse2; /* The source line in effect */
    int dontuse3;  /* should browser finish this context without stopping */
    SEXP dontuse4; /* only set during on.exit calls */
} RCNTXT, *context;

/* The Various Context Types.

 * In general the type is a bitwise OR of the values below.
 * Note that CTXT_LOOP is already the or of CTXT_NEXT and CTXT_BREAK.
 * Only functions should have the third bit turned on;
 * this allows us to move up the context stack easily
 * with either RETURN's or GENERIC's or RESTART's.
 * If you add a new context type for functions make sure
 *   CTXT_NEWTYPE & CTXT_FUNCTION > 0
 */
enum {
    CTXT_TOPLEVEL = 0,
    CTXT_NEXT = 1,
    CTXT_BREAK = 2,
    CTXT_LOOP = 3, /* break OR next target */
    CTXT_FUNCTION = 4,
    CTXT_CCODE = 8,
    CTXT_RETURN = 12,
    CTXT_BROWSER = 16,
    CTXT_GENERIC = 20,
    CTXT_RESTART = 32,
    CTXT_BUILTIN = 64 /* used in profiling */
};

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

#define PC_BOUNDSCHECK(pc)                                                     \
    SLOWASSERT((pc) >= code(c) && (pc) < code(c) + c->codeSize);

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


/** Creates a promise from given code object and environment.

 */
INLINE SEXP createPromise(Code* code, SEXP env) {
#if RIR_AS_PACKAGE == 1
    return mkPROMISE(rir_createWrapperPromise(code), env);
#else
    SEXP p = mkPROMISE((SEXP)code, env);
    PROTECT(p);
    // TODO: This is a bit of a hack to make sure the promise keeps its function
    // reachable from the GC pov.
    SEXP a = CONS_NR(functionStore(function(code)), R_NilValue);
    SET_ATTRIB(p, a);
    UNPROTECT(1);
    return p;
#endif
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

INLINE SEXP escape(SEXP val) {
    // FIXME : as long as our code objects can leak to various places
    // outside our control, we need to make sure to convert them back
    if (isValidCodeObject(val))
        val = rirExpr(val);

    assert(!TYPEOF(val) != 31);
    return val;
}

extern RCNTXT* R_GlobalContext;
extern void Rf_begincontext(void*, int, SEXP, SEXP, SEXP, SEXP, SEXP);
extern void Rf_endcontext(RCNTXT*);

// TODO remove numArgs and bp -- this is only needed for the on stack argument
// handling
#define INSTRUCTION(name)                                                      \
    INLINE void ins_##name(Code* c, SEXP env, OpcodeT** pc, Context* ctx,      \
                           unsigned numArgs)

INSTRUCTION(push_) {
    SEXP x = readConst(ctx, pc);
    R_Visible = TRUE;
    ostack_push(ctx, x);
}

static void jit(SEXP cls, Context* ctx) {
    assert(TYPEOF(cls) == CLOSXP);
    if (TYPEOF(BODY(cls)) == INTSXP)
        return;
    SEXP body = BODY(cls);
    if (TYPEOF(body) == BCODESXP)
        body = VECTOR_ELT(CDR(body), 0);
    SET_BODY(cls, ctx->compiler(body));
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
        /** If compile on demand is active, check that the function to be called
         * is compiled already, and compile if not.
         */
        if (COMPILE_ON_DEMAND) {
            jit(val, ctx);
        }
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

SEXP createArgsListStack(Code* c, size_t nargs, SEXP names, SEXP env, SEXP call,
                         Context* ctx, bool eager) {
    // for (int i = 0; i < nargs; ++i)
    //     printf("%x\n", call);
    SEXP result = R_NilValue;
    SEXP pos = result;

    SEXP* argbase = ostack_at(ctx, nargs - 1);

    for (size_t i = 0; i < nargs; ++i) {
        SEXP name = names != R_NilValue ? VECTOR_ELT(names, i) : R_NilValue;

        SEXP arg = argbase[i];

        if (!eager && (arg == R_MissingArg || arg == R_DotsSymbol)) {
            // We have to wrap them in a promise, otherwise they are threated
            // as extression to be evaluated, when in fact they are meant to be
            // asts as values
            SEXP promise = mkPROMISE(arg, env);
            SET_PRVALUE(promise, arg);
            __listAppend(&result, &pos, promise, R_NilValue);
        } else {
            if (eager && TYPEOF(arg) == PROMSXP) {
                arg = rirEval(arg, env);
            }
            arg = escape(arg);

            __listAppend(&result, &pos, arg, name);
        }
    }

    if (result != R_NilValue)
        UNPROTECT(1);
    return result;
}

SEXP createArgsListNew(Code* c, SEXP call, size_t nargs, uint32_t* cs, SEXP env,
                       Context* ctx, bool eager) {
    // for (int i = 0; i < nargs; ++i)
    //     printf("%x\n", call);
    SEXP result = R_NilValue;
    SEXP pos = result;

    // loop through the arguments and create a promise, unless it is a missing
    // argument
    bool hasNames = *CallSite_hasNames(cs);

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
                            arg = rirEval(CAR(ellipsis), env);
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
                arg = escape(arg);
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

SEXP createArgsList(Code* c, FunctionIndex* args, SEXP call, size_t nargs,
                    SEXP names, SEXP env, Context* ctx, bool eager) {
    // for (int i = 0; i < nargs; ++i)
    //     printf("%x\n", call);
    SEXP result = R_NilValue;
    SEXP pos = result;

    // loop through the arguments and create a promise, unless it is a missing
    // argument
    for (size_t i = 0; i < nargs; ++i) {
        unsigned offset = args[i];
        SEXP name = names != R_NilValue ? VECTOR_ELT(names, i) : R_NilValue;

        // if the argument is an ellipsis, then retrieve it from the environment and 
        // flatten the ellipsis
        if (args[i] == DOTS_ARG_IDX) {
            SEXP ellipsis = findVar(R_DotsSymbol, env);
            if (TYPEOF(ellipsis) == DOTSXP) {
                while (ellipsis != R_NilValue) {
                    name = TAG(ellipsis);
                    if (eager) {
                        SEXP arg = CAR(ellipsis);
                        if (arg != R_MissingArg)
                            arg = rirEval(CAR(ellipsis), env);
                        assert(TYPEOF(arg) != PROMSXP);
                        __listAppend(&result, &pos, arg, name);
                    } else {
                        SEXP promise = mkPROMISE(CAR(ellipsis), env);
                        __listAppend(&result, &pos, promise, name);
                    }
                    ellipsis = CDR(ellipsis);
                }
            }
        } else if (args[i] == MISSING_ARG_IDX) {
            if (eager)
                Rf_errorcall(call, "argument %d is empty", i + 1);
            __listAppend(&result, &pos, R_MissingArg, R_NilValue);
        } else {
            if (eager) {
                SEXP arg =
                    evalRirCode(codeAt(function(c), offset), ctx, env, 0);
                arg = escape(arg);
                assert(TYPEOF(arg) != PROMSXP);
                __listAppend(&result, &pos, arg, name);
            } else {
                Code* arg = codeAt(function(c), offset);
                SEXP promise = createPromise(arg, env);
                __listAppend(&result, &pos, promise, name);
            }
        }
    }

    if (result != R_NilValue)
        UNPROTECT(1);
    return result;
}

#if RIR_AS_PACKAGE == 0
void closureDebug(SEXP call, SEXP op, SEXP rho, SEXP newrho, RCNTXT* cntxt);
void endClosureDebug(SEXP op, SEXP call, SEXP rho);
SEXP closureArgumentAdaptor(SEXP call, SEXP op, SEXP arglist, SEXP rho, SEXP suppliedvars);
extern SEXP R_ReturnedValue;

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

INLINE SEXP rirCallClosure(SEXP call, SEXP env, SEXP callee, SEXP actuals,
                           unsigned nargs, OpcodeT** pc, Context* ctx) {
    // match formal arguments and create the env of this new activation record
    SEXP newEnv =
        closureArgumentAdaptor(call, callee, actuals, env, R_NilValue);

    ostack_push(ctx, newEnv);

    // Create a new R context and store it on the stack
    SEXP cntxt_store = Rf_allocVector(RAWSXP, sizeof(RCNTXT));
    ostack_push(ctx, cntxt_store);

    RCNTXT* cntxt = (RCNTXT*)RAW(cntxt_store);
    if (R_GlobalContext->callflag == CTXT_GENERIC)
        Rf_begincontext(cntxt, CTXT_RETURN, call, newEnv,
                        R_GlobalContext->sysparent, actuals, callee);
    else
        Rf_begincontext(cntxt, CTXT_RETURN, call, newEnv, env, actuals, callee);

    // Exec the closure
    closureDebug(call, callee, env, newEnv, cntxt);
    SEXP body = BODY(callee);
    Function* fun = (Function*)INTEGER(body);
    fun->invocationCount++;
    if (fun->invocationCount > 1000) {
        printf("Hot %p\n", fun);
        fun->invocationCount = 0;
    }
    Code* code = functionCode(fun);

    SEXP result = rirCallTrampoline(cntxt, code, newEnv, nargs, ctx);

    endClosureDebug(callee, call, env);

// TODO we need to set the returnvalue in the context, but we cannot
// access it since we do not know if we have INTSTACK or not (which
// changes RCNTXT layout. therefore we rely on the endClosure here,
// which will not work in the package version.... Not sure what is a
// good solution here.
#if RIR_AS_PACKAGE == 0
    endClosureContext(cntxt, result);
#else
    assert(false);
#endif

    ostack_pop(ctx); // cntxt
    ostack_pop(ctx); // newEnv

    return result;
}
#define USE_RIR_CONTEXT_SETUP true
#endif

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
        printf("warning: calling special: .Internal(%s\n",
                CHAR(PRINTNAME(CAR(CADR(call)))));
    } else {
        printf("warning: calling special: %s\n",
                R_FunTab[((sexprec_rjit*)callee)->u.i].name);
    }
}

/** Performs the call.

  TODO this is currently super simple.

 */
SEXP doCall(Code* caller, SEXP callee, unsigned id, SEXP env, OpcodeT** pc,
            Context* ctx) {

    uint32_t* cs = &caller->callSites[id];
    SEXP call = cp_pool_at(ctx, *CallSite_call(cs));
    uint32_t nargs = *CallSite_nargs(cs);

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
        SEXP argslist =
            createArgsListNew(caller, call, nargs, cs, env, ctx, true);
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
            createArgsListNew(caller, call, nargs, cs, env, ctx, false);
        PROTECT(argslist);
#if RIR_AS_PACKAGE == 0
        // if body is INTSXP, it is rir serialized code, execute it directly
        SEXP body = BODY(callee);
        assert(TYPEOF(body) == INTSXP || !COMPILE_ON_DEMAND);
        if (USE_RIR_CONTEXT_SETUP && TYPEOF(body) == INTSXP) {
            assert(isValidFunctionSEXP(body));
            result =
                rirCallClosure(call, env, callee, argslist, nargs, pc, ctx);
            UNPROTECT(1);
            break;
        }
#endif
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

        SEXP target = *ostack_at(ctx, nargs - 1);

        if (target == R_MissingArg) {
            assert(!setter);
            SETCDR(call, R_NilValue);
            UNPROTECT(1);
            return call;
        }

        target = escape(target);
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

            val = escape(val);

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
SEXP doCallStack(Code* caller, SEXP call, size_t nargs, SEXP names, SEXP env,
                 OpcodeT** pc, Context* ctx) {

    SEXP res = R_NilValue;

    // TODO: in the case of closures we should not do it eagerly
    SEXP callee = *ostack_at(ctx, nargs);
    if (TYPEOF(callee) == SPECIALSXP || TYPEOF(callee) == CLOSXP)
        call = fixupAST(call, ctx, nargs);
    PROTECT(call);

    switch (TYPEOF(callee)) {
    case SPECIALSXP: {
        assert(call != R_NilValue);
        ostack_popn(ctx, nargs);
        ostack_pop(ctx); // callee
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
        SEXP argslist =
            createArgsListStack(caller, nargs, names, env, call, ctx, true);
        PROTECT(argslist);
        ostack_popn(ctx, nargs);
        ostack_pop(ctx); // callee
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
        SEXP argslist =
            createArgsListStack(caller, nargs, names, env, call, ctx, false);
        PROTECT(argslist);
        ostack_popn(ctx, nargs);
        ostack_pop(ctx); // callee
#if RIR_AS_PACKAGE == 0
        // if body is INTSXP, it is rir serialized code, execute it directly
        SEXP body = BODY(callee);
        assert(TYPEOF(body) == INTSXP || !COMPILE_ON_DEMAND);
        if (USE_RIR_CONTEXT_SETUP && TYPEOF(body) == INTSXP) {
            assert(isValidFunctionSEXP(body));
            res = rirCallClosure(call, env, callee, argslist, nargs, pc, ctx);
            UNPROTECT(1);
            break;
        }
#endif
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

// Imports from GNUR for method dispatch
SEXP R_possible_dispatch(SEXP call, SEXP op, SEXP args, SEXP rho,
                         Rboolean promisedArgs);
Rboolean R_has_methods(SEXP selector);
int Rf_usemethod(const char* generic, SEXP obj, SEXP call, SEXP args, SEXP rho,
                 SEXP callrho, SEXP defrho, SEXP* ans);

SEXP doDispatchStack(Code* caller, SEXP call, SEXP selector, size_t nargs,
                     SEXP names, SEXP env, OpcodeT** pc, Context* ctx) {

#if RIR_AS_PACKAGE == 1
    // TODO
    assert(false);
#endif

    SEXP obj = *ostack_at(ctx, nargs - 1);
    assert(isObject(obj));

    call = fixupAST(call, ctx, nargs);
    PROTECT(call);

    SEXP actuals =
        createArgsListStack(caller, nargs, names, env, call, ctx, true);

    ostack_popn(ctx, nargs);

    ostack_push(ctx, actuals);
    ostack_push(ctx, call);
    UNPROTECT(1);

    SEXP res;

    do {
        // ===============================================
        // First try S4
        if (IS_S4_OBJECT(obj) && R_has_methods(selector)) {
            res = R_possible_dispatch(call, selector, actuals, env, TRUE);
            if (res) {
                break;
            }
        }

        // ===============================================
        // Then try S3
        const char* generic = CHAR(PRINTNAME(selector));
        char cntxt[400];
        SEXP rho1 = Rf_NewEnvironment(R_NilValue, R_NilValue, env);
        ostack_push(ctx, rho1);
        initClosureContext(&cntxt, call, rho1, env, actuals, selector);
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
                SETCAR(a, rirEval(CAR(a), env));
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
#if RIR_AS_PACKAGE == 0
            // if body is INTSXP, it is rir serialized code, execute it directly
            SEXP body = BODY(callee);
            assert(TYPEOF(body) == INTSXP || !COMPILE_ON_DEMAND);
            if (USE_RIR_CONTEXT_SETUP && TYPEOF(body) == INTSXP) {
                assert(isValidFunctionSEXP(body));
                res =
                    rirCallClosure(call, env, callee, actuals, nargs, pc, ctx);
                break;
            }
#endif
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

SEXP doDispatch(Code* caller, uint32_t id, SEXP env, OpcodeT** pc,
                Context* ctx) {

#if RIR_AS_PACKAGE == 1
    // TODO
    assert(false);
#endif

    SEXP obj = ostack_top(ctx);
    assert(isObject(obj));

    uint32_t* cs = &caller->callSites[id];
    SEXP call = cp_pool_at(ctx, *CallSite_call(cs));
    uint32_t nargs = *CallSite_nargs(cs);
    SEXP selector = cp_pool_at(ctx, *CallSite_selector(cs));

    SEXP actuals = createArgsListNew(caller, call, nargs, cs, env, ctx, false);
    ostack_push(ctx, actuals);
    SEXP res = NULL;

    // Patch the already evaluated object into the first entry of the promise
    // args list
    SET_PRVALUE(CAR(actuals), obj);

    do {
        // ===============================================
        // First try S4
        if (IS_S4_OBJECT(obj) && R_has_methods(selector)) {
            res = R_possible_dispatch(call, selector, actuals, env, TRUE);
            if (res) {
                break;
            }
        }

        // ===============================================
        // Then try S3
        const char* generic = CHAR(PRINTNAME(selector));
        char cntxt[400];
        SEXP rho1 = Rf_NewEnvironment(R_NilValue, R_NilValue, env);
        ostack_push(ctx, rho1);
        initClosureContext(&cntxt, call, rho1, env, actuals, selector);
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
                SETCAR(a, rirEval(CAR(a), env));
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
#if RIR_AS_PACKAGE == 0
            // if body is INTSXP, it is rir serialized code, execute it directly
            SEXP body = BODY(callee);
            assert(TYPEOF(body) == INTSXP || !COMPILE_ON_DEMAND);
            if (USE_RIR_CONTEXT_SETUP && TYPEOF(body) == INTSXP) {
                assert(isValidFunctionSEXP(body));
                res = rirCallClosure(call, env, callee, actuals, nargs, pc, ctx);
                break;
            }
#endif
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
    unsigned nargs = readImmediate(pc);
    // get the names of the arguments (or R_NilValue) if none
    SEXP names = readConst(ctx, pc);
    // get the call
    SEXP call = readConst(ctx, pc);

    ostack_push(ctx, doCallStack(c, call, nargs, names, env, pc, ctx));
}

INSTRUCTION(call_) {
    unsigned id = readImmediate(pc);
    // get the closure itself
    SEXP cls = ostack_pop(ctx);
    PROTECT(cls);
    ostack_push(ctx, doCall(c, cls, id, env, pc, ctx));
    UNPROTECT(1);
}

INSTRUCTION(dispatch_stack_) {
    unsigned nargs = readImmediate(pc);
    // get the names of the arguments (or R_NilValue) if none
    SEXP names = readConst(ctx, pc);
    SEXP selector = readConst(ctx, pc);
    // get the call
    SEXP call = readConst(ctx, pc);

    ostack_push(ctx,
                doDispatchStack(c, call, selector, nargs, names, env, pc, ctx));
}

INSTRUCTION(dispatch_) {
    unsigned id = readImmediate(pc);
    ostack_push(ctx, doDispatch(c, id, env, pc, ctx));
}

INSTRUCTION(promise_) {
    // get the Code * pointer we need
    unsigned codeOffset = readImmediate(pc);
    Code* promiseCode = codeAt(function(c), codeOffset);
    // create the promise and push it on stack
    ostack_push(ctx, createPromise(promiseCode, env));
}

INSTRUCTION(push_code_) {
    // get the Code * pointer we need
    unsigned codeOffset = readImmediate(pc);
    Code* promiseCode = codeAt(function(c), codeOffset);
    // create the promise and push it on stack
    ostack_push(ctx, (SEXP)promiseCode);
}

INSTRUCTION(close_) {
    SEXP srcref = *ostack_at(ctx, 0);
    SEXP body = *ostack_at(ctx, 1);
    SEXP formals = *ostack_at(ctx, 2);
    SEXP result = allocSExp(CLOSXP);
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

extern SEXP Rf_findcontext(int, SEXP, SEXP);
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
    SEXP* pos = ostack_at(ctx, 0);
    SEXP val = *pos;
    while (i--) {
        *pos = *(pos - 1);
        pos--;
    }
    *pos = val;
}

INSTRUCTION(pick_) {
    uint32_t i = readImmediate(pc);
    SEXP* pos = ostack_at(ctx, i);
    SEXP val = *pos;
    while (i--) {
        *pos = *(pos + 1);
        pos++;
    }
    *pos = val;
}

INSTRUCTION(pull_) {
    uint32_t i = readImmediate(pc);
    SEXP val = *ostack_at(ctx, i);
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

int R_isMissing(SEXP symbol, SEXP rho);
extern SEXP findVarLocInFrame(SEXP rho, SEXP symbol, Rboolean* canCache);
INSTRUCTION(missing_) {
    SEXP sym = readConst(ctx, pc);
    SLOWASSERT(TYPEOF(sym) == SYMSXP);
    SLOWASSERT(!DDVAL(sym));
    SEXP bind = findVarLocInFrame(env, sym, NULL);
    if (bind == R_NilValue)
        errorcall(getSrcAt(c, *pc - 1, ctx),
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
    SLOWASSERT(TYPEOF(sym) == SYMSXP);
    SEXP val = escape(ostack_pop(ctx));
    INCREMENT_NAMED(val);
    defineVar(sym, val, env);
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
        warningcall(getSrcAt(c, *pc - 1, ctx),
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
        errorcall(getSrcAt(c, *pc - 1, ctx), msg);
    }

    ostack_pop(ctx);
    ostack_push(ctx, cond ? R_TrueValue : R_FalseValue);
}

INSTRUCTION(brobj_) {
    int offset = readJumpOffset(pc);
    if (OBJECT(ostack_top(ctx)))
        *pc = *pc + offset;
    PC_BOUNDSCHECK(*pc);
}

INSTRUCTION(endcontext_) {
    SEXP cntxt_store = ostack_top(ctx);
    assert(TYPEOF(cntxt_store) == RAWSXP);
    RCNTXT* cntxt = (RCNTXT*)RAW(cntxt_store);
    Rf_endcontext(cntxt);
    ostack_pop(ctx); // Context
}

INSTRUCTION(brtrue_) {
    int offset = readJumpOffset(pc);
    if (ostack_pop(ctx) == R_TrueValue)
        *pc = *pc + offset;
    PC_BOUNDSCHECK(*pc);
}

INSTRUCTION(brfalse_) {
    int offset = readJumpOffset(pc);
    if (ostack_pop(ctx) == R_FalseValue)
        *pc = *pc + offset;
    PC_BOUNDSCHECK(*pc);
}

INSTRUCTION(br_) {
    int offset = readJumpOffset(pc);
    *pc = *pc + offset;
    PC_BOUNDSCHECK(*pc);
}

#if RIR_AS_PACKAGE == 0
SEXP do_subset2_dflt(SEXP, SEXP, SEXP, SEXP);
SEXP do_subset_dflt(SEXP, SEXP, SEXP, SEXP);
SEXP do_subassign2_dflt(SEXP call, SEXP op, SEXP args, SEXP rho);
SEXP do_subassign_dflt(SEXP call, SEXP op, SEXP args, SEXP rho);
#endif

INSTRUCTION(subassign2_) {
    SEXP val = *ostack_at(ctx, 2);
    SEXP idx = *ostack_at(ctx, 1);
    SEXP orig = *ostack_at(ctx, 0);

    unsigned targetI = readImmediate(pc);
    SEXP res;

#if RIR_AS_PACKAGE == 0
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
                (findVarLocInFrame(env, target, NULL) != R_NilValue);

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
                        SET_VECTOR_ELT(orig, idx_, escape(val));
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
    args = CONS_NR(escape(val), R_NilValue);
    args = CONS_NR(escape(idx), args);
    args = CONS_NR(escape(orig), args);
    PROTECT(args);
    res = do_subassign2_dflt(R_NilValue, R_Subassign2Sym, args, env);
    ostack_popn(ctx, 3);
    UNPROTECT(1);
#else
    ostack_popn(ctx, 3);
    res = Rf_eval(getSrcForCall(c, *pc - 2, ctx), env);
#endif
    ostack_push(ctx, res);
}

INSTRUCTION(subassign_) {
    SEXP val = *ostack_at(ctx, 2);
    SEXP idx = *ostack_at(ctx, 1);
    SEXP orig = *ostack_at(ctx, 0);

    SEXP res;

#if RIR_AS_PACKAGE == 0
    INCREMENT_NAMED(orig);
    SEXP args;
    args = CONS_NR(escape(val), R_NilValue);
    args = CONS_NR(escape(idx), args);
    args = CONS_NR(escape(orig), args);
    PROTECT(args);
    res = do_subassign_dflt(R_NilValue, R_SubassignSym, args, env);
    ostack_popn(ctx, 3);
    UNPROTECT(1);
#else
    ostack_popn(ctx, 3);
    res = Rf_eval(getSrcForCall(c, *pc - 2, ctx), env);
#endif
    ostack_push(ctx, res);
}

INSTRUCTION(subset2_) {
    SEXP idx2 = *ostack_at(ctx, 0);
    SEXP idx1 = *ostack_at(ctx, 1);
    SEXP val = *ostack_at(ctx, 2);

    SEXP res;
#if RIR_AS_PACKAGE == 0
    SEXP args;
    args = CONS_NR(idx2, R_NilValue);
    args = CONS_NR(idx1, args);
    args = CONS_NR(val, args);
    ostack_push(ctx, args);
    res = do_subset_dflt(R_NilValue, R_SubsetSym, args, env);
    ostack_popn(ctx, 4);
#else
    res = Rf_eval(getSrcForCall(c, *pc - 1, ctx), env);
#endif

    R_Visible = 1;
    ostack_push(ctx, res);
}

INSTRUCTION(extract2_) {
    SEXP idx2 = *ostack_at(ctx, 0);
    SEXP idx1 = *ostack_at(ctx, 1);
    SEXP val = *ostack_at(ctx, 2);

    SEXP res;
#if RIR_AS_PACKAGE == 0
    SEXP args;
    args = CONS_NR(idx2, R_NilValue);
    args = CONS_NR(idx1, args);
    args = CONS_NR(val, args);
    ostack_push(ctx, args);
    res = do_subset_dflt(R_NilValue, R_Subset2Sym, args, env);
    ostack_popn(ctx, 4);
#else
    res = Rf_eval(getSrcForCall(c, *pc - 1, ctx), env);
#endif

    R_Visible = 1;
    ostack_push(ctx, res);
}

INSTRUCTION(subset1_) {
    SEXP idx = *ostack_at(ctx, 0);
    SEXP val = *ostack_at(ctx, 1);

    SEXP res;
#if RIR_AS_PACKAGE == 0
    SEXP args;
    args = CONS_NR(idx, R_NilValue);
    args = CONS_NR(val, args);
    ostack_push(ctx, args);
    res = do_subset_dflt(R_NilValue, R_SubsetSym, args, env);
    ostack_popn(ctx, 3);
#else
    res = Rf_eval(getSrcForCall(c, *pc - 1, ctx), env);
#endif

    R_Visible = 1;
    ostack_push(ctx, res);
}

INSTRUCTION(extract1_) {
    SEXP idx = *ostack_at(ctx, 0);
    SEXP val = *ostack_at(ctx, 1);

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
#if RIR_AS_PACKAGE == 0
        SEXP args;
        args = CONS_NR(idx, R_NilValue);
        args = CONS_NR(val, args);
        ostack_push(ctx, args);
        res = do_subset2_dflt(R_NilValue, R_Subset2Sym, args, env);
        ostack_popn(ctx, 3);
#else
        res = Rf_eval(getSrcForCall(c, *pc - 1, ctx), env);
        ostack_popn(ctx, 2);
#endif
    }

    R_Visible = 1;
    ostack_push(ctx, res);
}

INSTRUCTION(dup_) { ostack_push(ctx, ostack_top(ctx)); }

INSTRUCTION(isspecial_) {
    // TODO I do not think this is a proper way - we must check all the way
    // down, not just findVar (vars do not shadow closures)
    SEXP sym = readConst(ctx, pc);
    SEXP val = findFun(sym, env);
    // TODO better check
    assert(TYPEOF(val) == SPECIALSXP || TYPEOF(val) == BUILTINSXP);
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

enum op { PLUSOP, MINUSOP, TIMESOP };
#define INTEGER_OVERFLOW_WARNING "NAs produced by integer overflow"

#define CHECK_INTEGER_OVERFLOW(ans, naflag)                                    \
    do {                                                                       \
        if (naflag) {                                                          \
            PROTECT(ans);                                                      \
            SEXP call = getSrcForCall(c, *pc - 1, ctx);                        \
            Rf_warningcall(call, INTEGER_OVERFLOW_WARNING);                    \
            UNPROTECT(1);                                                      \
        }                                                                      \
    } while (0)

#define BINOP_FALLBACK(op)                                                     \
    do {                                                                       \
        static SEXP prim = NULL;                                               \
        static CCODE blt;                                                      \
        int flag;                                                              \
        if (!prim) {                                                           \
            prim = findFun(Rf_install(op), R_GlobalEnv);                       \
            blt = getBuiltin(prim);                                            \
            flag = getFlag(prim);                                              \
        }                                                                      \
        SEXP call = getSrcForCall(c, *pc - 1, ctx);                            \
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
    SEXP lhs = *ostack_at(ctx, 1);
    SEXP rhs = *ostack_at(ctx, 0);
    SEXP res;

    DO_BINOP(*, TIMESOP);

    ostack_popn(ctx, 2);
    ostack_push(ctx, res);
}

INSTRUCTION(add_) {
    SEXP lhs = *ostack_at(ctx, 1);
    SEXP rhs = *ostack_at(ctx, 0);
    SEXP res;

    DO_BINOP(+, PLUSOP);

    ostack_popn(ctx, 2);
    ostack_push(ctx, res);
}

INSTRUCTION(sub_) {
    SEXP lhs = *ostack_at(ctx, 1);
    SEXP rhs = *ostack_at(ctx, 0);
    SEXP res;

    DO_BINOP(-, MINUSOP);

    ostack_popn(ctx, 2);
    ostack_push(ctx, res);
}

INSTRUCTION(lt_) {
    SEXP lhs = *ostack_at(ctx, 1);
    SEXP rhs = *ostack_at(ctx, 0);
    SEXP res;

    do {
        if (IS_SCALAR_VALUE(lhs, REALSXP)) {
            if (IS_SCALAR_VALUE(rhs, REALSXP)) {
                if (*REAL(lhs) == NA_REAL || *REAL(rhs) == NA_REAL) {
                    res = R_LogicalNAValue;
                } else {
                    res = *REAL(lhs) < *REAL(rhs) ? R_TrueValue : R_FalseValue;
                }
                break;
            }
        } else if (IS_SCALAR_VALUE(lhs, INTSXP)) {
            if (IS_SCALAR_VALUE(rhs, INTSXP)) {
                if (*INTEGER(lhs) == NA_INTEGER ||
                    *INTEGER(rhs) == NA_INTEGER) {
                    res = R_LogicalNAValue;
                } else {
                    res = *INTEGER(lhs) < *INTEGER(rhs) ? R_TrueValue
                                                        : R_FalseValue;
                }
                break;
            }
        }
        BINOP_FALLBACK("<");
    } while (false);

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

    SEXP from = *ostack_at(ctx, 2);
    SEXP to = *ostack_at(ctx, 1);
    SEXP by = *ostack_at(ctx, 0);
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
        SEXP call = getSrcForCall(c, *pc - 1, ctx);
        SEXP argslist = CONS_NR(from, CONS_NR(to, CONS_NR(by, R_NilValue)));
        ostack_push(ctx, argslist);
        res = applyClosure(call, prim, argslist, env, R_NilValue);
        ostack_pop(ctx);
    }

    ostack_popn(ctx, 3);
    ostack_push(ctx, res);
}

INSTRUCTION(test_bounds_) {
    SEXP vec = *ostack_at(ctx, 1);
    SEXP idx = *ostack_at(ctx, 0);
    int len = Rf_length(vec);
    int i = asInteger(idx);
    ostack_push(ctx, i > 0 && i <= len ? R_TrueValue : R_FalseValue);
}

INSTRUCTION(dup2_) {
    SEXP a = *ostack_at(ctx, 1);
    SEXP b = *ostack_at(ctx, 0);
    ostack_push(ctx, a);
    ostack_push(ctx, b);
}

INSTRUCTION(visible_) {
    R_Visible = 1;
}

INSTRUCTION(invisible_) {
    R_Visible = 0;
}

INSTRUCTION(uniq_) {
    SEXP v = ostack_top(ctx);
    if (NAMED(v) < 2) {
        SET_NAMED(v, 2);
    } else {
        v = shallow_duplicate(escape(v));
        *ostack_at(ctx, 0) = v;
        SET_NAMED(v, 1);
    }
}

extern void printCode(Code* c);
extern void printFunction(Function* f);

extern SEXP Rf_deparse1(SEXP call, Rboolean abbrev, int opts);

#undef R_CheckStack

static int debugging = 0;
void debug(Code* c, OpcodeT* pc, const char* name, unsigned depth,
           Context* ctx) {
    return;
    if (debugging == 0) {
        debugging = 1;
        printf("%p : %d, %s, s: %d\n", c, *pc, name, depth);
        for (int i = 0; i < depth; ++i) {
            printf("%3d: ", i);
            Rf_PrintValue(*ostack_at(ctx, i));
        }
        printf("\n");
        debugging = 0;
    }
}

SEXP evalRirCode(Code* c, Context* ctx, SEXP env, unsigned numArgs) {
    SLOWASSERT(c->magic == CODE_MAGIC);

    if (!env)
	error("'rho' cannot be C NULL: detected in C-level eval");
    if (!isEnvironment(env))
	error("'rho' must be an environment not %s: detected in C-level eval",
	      type2char(TYPEOF(env)));

    R_CheckStack();

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
        ins_##name(c, env, &pc, ctx, numArgs);                                 \
        debug(c, pc, #name, ostack_length(ctx) - bp, ctx);                     \
        break

            INS(seq_);
            INS(push_);
            INS(ldfun_);
            INS(ldvar_);
            INS(ldarg_);
            INS(ldddvar_);
            INS(add_);
            INS(mul_);
            INS(sub_);
            INS(lt_);
            INS(call_);
            INS(call_stack_);
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
            INS(isspecial_);
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
            INS(uniq_);
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
                PC_BOUNDSCHECK(pc);
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
    assert(TYPEOF(f) != 31);
    if (isValidFunctionObject(f)) {
        Function* ff = (Function*)(INTEGER(f));
        return src_pool_at(globalContext(), functionCode(ff)->src);
    }
    return f;
}

SEXP rirEval_f(SEXP f, SEXP env) {
    // TODO we do not really need the arg counts now
    if (isValidCodeObject(f)) {
        //        Rprintf("Evaluating promise:\n");
        Code* c = (Code*)f;
        SEXP x = evalRirCode(c, globalContext(), env, 0);
      //        Rprintf("Promise evaluated, length %u, value %d",
        //        Rf_length(x), REAL(x)[0]);
        return escape(x);
    } else {
        //        Rprintf("=====================================================\n");
        //        Rprintf("Evaluating function\n");
        Function* ff = (Function*)(INTEGER(f));
        return escape(evalRirCode(functionCode(ff), globalContext(), env, 0));
    }
}

SEXP rirEval(SEXP e, SEXP env) {
    static int evalcount = 0;
    if (++evalcount > 1000) { /* was 100 before 2.8.0 */
        R_CheckUserInterrupt();
        evalcount = 0 ;
    }

    R_Visible = TRUE;

    /* handle self-evluating objects with minimal overhead */
    switch (TYPEOF(e)) {
    case INTSXP: {
        if (isValidFunctionSEXP(e)) {
            Function* ff = (Function*)(INTEGER(e));
            return escape(
                evalRirCode(functionCode(ff), globalContext(), env, 0));
        }
        // Fall through
    }
    case NILSXP:
    case LISTSXP:
    case LGLSXP:
    case REALSXP:
    case STRSXP:
    case CPLXSXP:
    case RAWSXP:
    case S4SXP:
    case SPECIALSXP:
    case BUILTINSXP:
    case ENVSXP:
    case CLOSXP:
    case VECSXP:
    case EXTPTRSXP:
    case WEAKREFSXP:
    case EXPRSXP:
	/* Make sure constants in expressions are NAMED before being
	   used as values.  Setting NAMED to 2 makes sure weird calls
	   to replacement functions won't modify constants in
	   expressions.  */
	if (NAMED(e) <= 1) SET_NAMED(e, 2);
	return e;

    case 31: {
        Code* c = (Code*)e;
        assert(isValidCodeObject(e));
        return escape(evalRirCode(c, globalContext(), env, 0));
    }

    case NATIVESXP:
        assert(false);
        break;

    case BCODESXP: {
        SEXP expr = VECTOR_ELT(CDR(e), 0);
        SEXP code = globalContext()->compiler(expr);
        PROTECT(code);
        Function* ff = (Function*)(INTEGER(code));
        SEXP res = evalRirCode(functionCode(ff), globalContext(), env, 0);
        UNPROTECT(1);
        return escape(res);
    }

    case SYMSXP: {
        if (e == R_DotsSymbol)
            error("'...' used in an incorrect context");

        SEXP val;
        
        if (DDVAL(e))
            val = ddfindVar(e, env);
        else
            val = findVar(e, env);

        // TODO better errors
        if (val == R_UnboundValue) {
            Rf_error("object not found");
        } else if (val == R_MissingArg) {
            Rf_error("argument \"%s\" is missing, with no default", CHAR(PRINTNAME(e)));
        }

        // if promise, evaluate & return
        if (TYPEOF(val) == PROMSXP)
            val = promiseValue(val, globalContext());

        // WTF? is this just defensive programming or what?
        if (NAMED(val) == 0 && val != R_NilValue)
            SET_NAMED(val, 1);

        return val;
        break;
    }

    case PROMSXP:
        return promiseValue(e, globalContext());

    case LANGSXP: {
        SEXP code = globalContext()->compiler(e);
        PROTECT(code);
        Function* ff = (Function*)(INTEGER(code));
        SEXP res = evalRirCode(functionCode(ff), globalContext(), env, 0);
        UNPROTECT(1);
        return escape(res);
    }

    case DOTSXP:
	error("'...' used in an incorrect context");
    default:
        assert(false && "UNIMPLEMENTED_TYPE");
    }

    assert(false);
    return R_NilValue;
}
