#include <assert.h>
#include <alloca.h>

#include "interp.h"
#include "interp_context.h"

#define NOT_IMPLEMENTED assert(false)

// TODO we are using the RInternals, but soud not when the code moves to GNU-R
// #include "RIntlns.h"

#undef eval

extern SEXP R_TrueValue;
extern SEXP R_FalseValue;
extern SEXP Rf_NewEnvironment(SEXP, SEXP, SEXP);
extern Rboolean R_Visible;

extern SEXP hook_forcePromise(SEXP);
extern SEXP hook_mkPROMISE(SEXP, SEXP);
extern SEXP hook_matchArgs(SEXP, SEXP, SEXP);

SEXP forcePromise(SEXP what) { return hook_forcePromise(what); }

SEXP mkPROMISE(SEXP expr, SEXP rho) {
    assert(rho != R_NilValue);
    return hook_mkPROMISE(expr, rho);
}

typedef SEXP (*CCODE)(SEXP, SEXP, SEXP, SEXP);

/* Information for Deparsing Expressions */
typedef enum {
    PP_INVALID = 0,
    PP_ASSIGN = 1,
    PP_ASSIGN2 = 2,
    PP_BINARY = 3,
    PP_BINARY2 = 4,
    PP_BREAK = 5,
    PP_CURLY = 6,
    PP_FOR = 7,
    PP_FUNCALL = 8,
    PP_FUNCTION = 9,
    PP_IF = 10,
    PP_NEXT = 11,
    PP_PAREN = 12,
    PP_RETURN = 13,
    PP_SUBASS = 14,
    PP_SUBSET = 15,
    PP_WHILE = 16,
    PP_UNARY = 17,
    PP_DOLLAR = 18,
    PP_FOREIGN = 19,
    PP_REPEAT = 20
} PPkind;

typedef enum {
    PREC_FN = 0,
    PREC_LEFT = 1,
    PREC_EQ = 2,
    PREC_RIGHT = 3,
    PREC_TILDE = 4,
    PREC_OR = 5,
    PREC_AND = 6,
    PREC_NOT = 7,
    PREC_COMPARE = 8,
    PREC_SUM = 9,
    PREC_PROD = 10,
    PREC_PERCENT = 11,
    PREC_COLON = 12,
    PREC_SIGN = 13,
    PREC_POWER = 14,
    PREC_DOLLAR = 15,
    PREC_NS = 16,
    PREC_SUBSET = 17
} PPprec;

typedef struct {
    PPkind kind;             /* deparse kind */
    PPprec precedence;       /* operator precedence */
    unsigned int rightassoc; /* right associative? */
} PPinfo;

/* The type definitions for the table of built-in functions. */
/* This table can be found in ../main/names.c */
typedef struct {
    char* name;  /* print name */
    CCODE cfun;  /* c-code address */
    int code;    /* offset within c-code */
    int eval;    /* evaluate args? */
    int arity;   /* function arity */
    PPinfo gram; /* pretty-print info */
} FUNTAB;

extern FUNTAB R_FunTab[];

typedef struct sxpinfo_struct_rjit {
    unsigned int type : 5; /* ==> (FUNSXP == 99) %% 2^5 == 3 == CLOSXP
                        * -> warning: `type' is narrower than values
                        *              of its type
                        * when SEXPTYPE was an enum */
    unsigned int obj : 1;
    unsigned int named : 2;
    unsigned int gp : 16;
    unsigned int mark : 1;
    unsigned int debug : 1;
    unsigned int trace : 1; /* functions and memory tracing */
    unsigned int spare : 1; /* currently unused */
    unsigned int gcgen : 1; /* old generation number */
    unsigned int gccls : 3; /* node class */
} sxpifo_struct_rjit;       /*		    Tot: 32 */

typedef struct cons_rjit {
    SEXP car;
    SEXP cdr;
    SEXP tag;
} cons_rjit;

typedef struct sexprec_rjit {
    struct sxpinfo_struct_rjit sxpinfo;
    SEXP attrib;
    SEXP gengc_next_node, gengc_prev_node;
    union {
        struct cons_rjit cons;
        int i;
    } u;
} sexprec_rjit;

// helpers

/** Moves the pc to next instruction, based on the current instruction length
 */
OpcodeT* advancePc(OpcodeT* pc) {
    switch (*pc++) {
#define DEF_INSTR(name, imm, ...)                                              \
    case name:                                                                 \
        pc += sizeof(ArgT) * imm;                                              \
        break;
#include "insns.h"
    default:
        assert(false && "Unknown instruction");
    }
    return pc;
}

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

INLINE SEXP getSrcAt(Code* c, OpcodeT* pc, Context* ctx) {
    // we need to determine index of the current instruction
    OpcodeT* x = code(c);
    // find the pc of the current instructions, it is ok to be slow
    unsigned insIdx = 0;
    while ((x = advancePc(x)) != pc)
        ++insIdx;
    unsigned sidx = src(c)[insIdx];
    // return the ast for the instruction, or if not defined, the ast of the
    // function
    return src_pool_at(ctx, sidx == 0 ? c->src : sidx);
}

INLINE SEXP getSrcForCall(Code* c, OpcodeT* pc, Context* ctx) {
    // we need to determine index of the current instruction
    OpcodeT* x = code(c);
    // find the pc of the current instructions, it is ok to be slow
    unsigned insIdx = 0;
    while ((x = advancePc(x)) != pc)
        ++insIdx;
    unsigned sidx = src(c)[insIdx];
    // return the ast for the instruction, or if not defined, the ast of the
    // function
    assert(sidx);
    return src_pool_at(ctx, sidx);
}


/** Creates a promise from given code object and environment.

 */
INLINE SEXP createPromise(Code* code, SEXP env) {
    SEXP p = mkPROMISE((SEXP)code, env);
    // TODO: This is a bit of a hack to make sure the promise keeps its function
    // reachable from the GC pov.
    SEXP a = CONS_NR(functionStore(function(code)), R_NilValue);
    SET_ATTRIB(p, a);
    return p;
}

// TODO check if there is a function for this in R
INLINE SEXP promiseValue(SEXP promise) {
    // if already evaluated, return the value
    if (PRVALUE(promise) && PRVALUE(promise) != R_UnboundValue) {
        promise = PRVALUE(promise);
        assert(TYPEOF(promise) != PROMSXP);
        SET_NAMED(promise, 2);
        return promise;
    }
    return forcePromise(promise);
}

// TODO remove numArgs and bp -- this is only needed for the on stack argument
// handling
#define INSTRUCTION(name)                                                      \
    INLINE void ins_##name(Code* c, SEXP env, OpcodeT** pc, Context* ctx,      \
                           unsigned numArgs, unsigned bp)

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
    SET_BODY(cls, ctx->compiler(body, CLOENV(cls)));
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
    SEXP val = ddfindVar(sym, env);
    R_Visible = TRUE;

    // TODO better errors
    if (val == R_UnboundValue) {
        Rf_error("object not found");
    } else if (val == R_MissingArg) {
        error("argument is missing, with no default");
    }

    // if promise, evaluate & return
    if (TYPEOF(val) == PROMSXP)
        val = promiseValue(val);

    // WTF? is this just defensive programming or what?
    if (NAMED(val) == 0 && val != R_NilValue)
        SET_NAMED(val, 1);

    ostack_push(ctx, val);
}


INSTRUCTION(ldvar_) {
    SEXP sym = readConst(ctx, pc);
    SEXP val = findVar(sym, env);
    R_Visible = TRUE;

    // TODO better errors
    if (val == R_UnboundValue) {
        Rf_error("object not found");
    } else if (val == R_MissingArg) {
        Rf_error("argument \"%s\" is missing, with no default", CHAR(PRINTNAME(sym)));
    }

    // if promise, evaluate & return
    if (TYPEOF(val) == PROMSXP)
        val = promiseValue(val);

    // WTF? is this just defensive programming or what?
    if (NAMED(val) == 0 && val != R_NilValue)
        SET_NAMED(val, 1);

    ostack_push(ctx, val);
}

/** Given argument code offsets, creates the argslist from their promises.
 */
// TODO unnamed only at this point
int __listAppend(SEXP* front, SEXP* last, SEXP value, SEXP name) {
    int protected = 0;

    assert(TYPEOF(*front) == LISTSXP || TYPEOF(*front) == NILSXP);
    assert(TYPEOF(*last) == LISTSXP || TYPEOF(*last) == NILSXP);

    SEXP app = CONS_NR(value, R_NilValue);
    
    SET_TAG(app, name);

    if (*front == R_NilValue) {
        *front = app;
        PROTECT(*front);
        protected++;
    }

    if (*last != R_NilValue)
        SETCDR(*last, app);
    *last = app;

    return protected;
}

SEXP createArgsList(Code * c, FunctionIndex * args, size_t nargs, SEXP names, SEXP env) {
    SEXP result = R_NilValue;
    SEXP pos = result;
    int protected = 0;

    // loop through the arguments and create a promise, unless it is a missing argument
    for (size_t i = 0; i < nargs; ++i) {
        SEXP name = names != R_NilValue ? VECTOR_ELT(names, i) : R_NilValue;

        // if the argument is an ellipsis, then retrieve it from the environment and 
        // flatten the ellipsis
        if (args[i] == DOTS_ARG_IDX) {
            SEXP ellipsis = findVar(R_DotsSymbol, env);
            if (TYPEOF(ellipsis) == DOTSXP) {
                while (ellipsis != R_NilValue) {
                    name = TAG(ellipsis);
                    SEXP promise = hook_mkPROMISE(CAR(ellipsis), env);
                    protected += __listAppend(&result, &pos, promise, name);
                    ellipsis = CDR(ellipsis);
                }
            }
        } else if (args[i] == MISSING_ARG_IDX) {
            protected += __listAppend(&result, &pos, R_MissingArg, R_NilValue);
        } else {
            unsigned offset = args[i];
            Code* arg = codeAt(function(c), offset);
            SEXP promise = createPromise(arg, env);
            protected += __listAppend(&result, &pos, promise, name);
        }
    }

    UNPROTECT(protected);
    return result;
}

SEXP createEagerArgsList(Code* c, FunctionIndex* args, size_t nargs, SEXP names,
                         SEXP env, SEXP call, Context* ctx) {
    SEXP result = R_NilValue;
    SEXP pos = result;
    int protected = 0;

    for (size_t i = 0; i < nargs; ++i) {
        unsigned offset = args[i];
        SEXP name = names != R_NilValue ? VECTOR_ELT(names, i) : R_NilValue;

        // if the argument is an ellipsis, then retrieve it from the environment and 
        // flatten the ellipsis
        if (args[i] == DOTS_ARG_IDX) {
            SEXP ellipsis = findVar(R_DotsSymbol, env);
            if (TYPEOF(ellipsis) == DOTSXP) {
                while (ellipsis != R_NilValue) {
                    SEXP arg = rirEval(CAR(ellipsis), env);
                    assert(TYPEOF(arg) != PROMSXP);
                    name = TAG(ellipsis);
                    protected += __listAppend(&result, &pos, arg, name);
                    ellipsis = CDR(ellipsis);
                }
            }
        } else if (args[i] == MISSING_ARG_IDX) {
            Rf_errorcall(call, "argument is missing, with no default");
        } else {
            SEXP arg = rirEval_c(codeAt(function(c), offset), ctx, env, 0);
            assert(TYPEOF(arg) != PROMSXP);
            protected += __listAppend(&result, &pos, arg, name);
        }
    }

    UNPROTECT(protected);
    return result;
}

/** Returns us the CCODE object from R_FunTab based on name.

  TODO This exists in gnu-r (names.c), when integrated inside, we want to make
  use of it.
 */
CCODE getBuiltin(SEXP f) {
    int i = ((sexprec_rjit*)f)->u.i;
    return R_FunTab[i].cfun;
}
int getFlag(SEXP f) {
    int i = ((sexprec_rjit*)f)->u.i;
    return (((R_FunTab[i].eval)/100)%10);
}

// hooks for the call
SEXP closureArgumentAdaptor(SEXP call, SEXP op, SEXP arglist, SEXP rho, SEXP suppliedvars);
typedef struct {
    Code* code;
    Context* ctx;
    SEXP env;
    size_t nargs;
} EvalCbArg;
SEXP hook_rirCallTrampoline(void* cntxt, SEXP (*evalCb)(EvalCbArg*), EvalCbArg* arg);
SEXP evalCbFunction(EvalCbArg* arg_) {
    EvalCbArg* arg = (EvalCbArg*)arg_;
    return rirEval_c(arg->code, arg->ctx, arg->env, arg->nargs);
}
void initClosureContext(void*, SEXP, SEXP, SEXP, SEXP, SEXP);
void endClosureContext(void*, SEXP);

/** Performs the call.

  TODO this is currently super simple.

 */
SEXP doCall(Code * caller, SEXP call, SEXP callee, unsigned * args, size_t nargs, SEXP names, SEXP env, Context * ctx) {

    size_t oldbp = ctx->ostack.length;
    size_t oldbpi = ctx->istack.length;
    SEXP result = R_NilValue;
    switch (TYPEOF(callee)) {
    case SPECIALSXP: {
        // get the ccode
        CCODE f = getBuiltin(callee);
        int flag = getFlag(callee);
        R_Visible = flag != 1;
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
        SEXP argslist = createEagerArgsList(caller, args, nargs, names, env, call, ctx);
        // callit
        PROTECT(argslist);
        if (flag < 2) R_Visible = flag != 1;
        result = f(call, callee, argslist, env);
        if (flag < 2) R_Visible = flag != 1;
        UNPROTECT(1);
        break;
    }
    case CLOSXP: {
        SEXP actuals;
        SEXP body = BODY(callee);
        actuals = createArgsList(caller, args, nargs, names, env);
        PROTECT(actuals);

        assert(isValidFunction(body));
        SEXP newEnv = closureArgumentAdaptor(call, callee, actuals, env, R_NilValue);
        PROTECT(newEnv);

        // TODO since we do not have access to the context definition we
        // just create a buffer big enough and let gnur do the rest.
        // Once we integrate we should really setup the context ourselves!
        char cntxt[400];
        initClosureContext(&cntxt, call, newEnv, env, actuals, callee);
        EvalCbArg arg = {functionCode((Function*)INTEGER(body)), ctx, newEnv, nargs};
        result = hook_rirCallTrampoline(&cntxt, evalCbFunction, &arg);
        endClosureContext(&cntxt, result);

        UNPROTECT(2); // argslist
        break;
    }
    default:
        assert(false && "Don't know how to run other stuff");
    }
    assert(oldbp == ctx->ostack.length && oldbpi == ctx->istack.length &&
           "Corrupted stacks");
    return result;
}


INSTRUCTION(call_) {
    // get the indices of argument promises
    SEXP args_ = readConst(ctx, pc);
    assert(TYPEOF(args_) == INTSXP &&
           "TODO change to INTSXP, not RAWSXP it used to be");
    unsigned nargs = Rf_length(args_) / sizeof(unsigned);
    unsigned* args = (unsigned*)INTEGER(args_);
    // get the names of the arguments (or R_NilValue) if none
    SEXP names = readConst(ctx, pc);
    // get the closure itself
    SEXP cls = ostack_pop(ctx);
    // do the call
    SEXP call = getSrcForCall(c, *pc, ctx);

    PROTECT(cls);
    ostack_push(ctx, doCall(c, call, cls, args, nargs, names, env, ctx));
    UNPROTECT(1);
}

INSTRUCTION(promise_) {
    // get the Code * pointer we need
    unsigned codeOffset = readImmediate(pc);
    Code* promiseCode = codeAt(function(c), codeOffset);
    // create the promise and push it on stack
    ostack_push(ctx, createPromise(promiseCode, env));
}

INSTRUCTION(close_) {
    SEXP body = ostack_pop(ctx);
    SEXP formals = ostack_pop(ctx);
    PROTECT(body);
    PROTECT(formals);
    SEXP result = allocSExp(CLOSXP);
    SET_FORMALS(result, formals);
    SET_BODY(result, body);
    SET_CLOENV(result, env);
    UNPROTECT(2);
    ostack_push(ctx, result);
}

INSTRUCTION(force_) {
    SEXP p = ostack_pop(ctx);
    assert(TYPEOF(p) == PROMSXP);
    // If the promise is already evaluated then push the value inside the
    // promise
    // onto the stack, otherwise push the value from forcing the promise
    ostack_push(ctx, promiseValue(p));
}

INSTRUCTION(pop_) { ostack_pop(ctx); }

INSTRUCTION(pusharg_) {
    unsigned n = readImmediate(pc);
    assert(n < numArgs);
    ostack_push(ctx, ostack_at(ctx, bp - numArgs + n));
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

INSTRUCTION(stvar_) {
    SEXP sym = ostack_pop(ctx);
    assert(TYPEOF(sym) == SYMSXP);
    SEXP val = ostack_pop(ctx);
    defineVar(sym, val, env);
    ostack_push(ctx, val);
}

INSTRUCTION(asbool_) {
    SEXP t = ostack_pop(ctx);
    int cond = NA_LOGICAL;
    if (Rf_length(t) > 1)
        warningcall(getSrcAt(c, *pc, ctx),
                    ("the condition has length > 1 and only the first "
                     "element will be used"));

    if (Rf_length(t) > 0) {
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
        errorcall(getSrcAt(c, *pc, ctx), msg);
    }

    ostack_push(ctx, cond ? R_TrueValue : R_FalseValue);
}

INSTRUCTION(brtrue_) {
    int offset = readJumpOffset(pc);
    if (ostack_pop(ctx) == R_TrueValue)
        *pc = *pc + offset;
}

INSTRUCTION(brfalse_) {
    int offset = readJumpOffset(pc);
    if (ostack_pop(ctx) == R_FalseValue)
        *pc = *pc + offset;
}

INSTRUCTION(br_) {
    int offset = readJumpOffset(pc);
    *pc = *pc + offset;
}

INSTRUCTION(lti_) {
    int rhs = istack_pop(ctx);
    int lhs = istack_pop(ctx);
    ostack_push(ctx, lhs < rhs ? R_TrueValue : R_FalseValue);
}

INSTRUCTION(eqi_) {
    int rhs = istack_pop(ctx);
    int lhs = istack_pop(ctx);
    ostack_push(ctx, lhs == rhs ? R_TrueValue : R_FalseValue);
}

INSTRUCTION(pushi_) { istack_push(ctx, readSignedImmediate(pc)); }

INSTRUCTION(dupi_) { istack_push(ctx, istack_top(ctx)); }

INSTRUCTION(dup_) { ostack_push(ctx, ostack_top(ctx)); }

INSTRUCTION(add_) {
    assert(false);
    SEXP rhs = ostack_pop(ctx);
    SEXP lhs = ostack_pop(ctx);
    if (TYPEOF(lhs) == REALSXP && TYPEOF(rhs) == REALSXP &&
        Rf_length(lhs) == 1 && Rf_length(rhs) == 1) {
        SEXP res = Rf_allocVector(REALSXP, 1);
        SET_NAMED(res, 1);
        REAL(res)[0] = REAL(lhs)[0] + REAL(rhs)[0];
        ostack_push(ctx, res);
    } else {
        NOT_IMPLEMENTED;
        // SEXP op = getPrimitive("+");
        // SEXP primfun = getPrimfun("+");
        // TODO we should change how primitives are called now that we can
        // integrate
        // SEXP res = callPrimitive(primfun, getCurrentCall(c, pc), op, { lhs,
        // rhs });
        // TODO push
    }
}

INSTRUCTION(sub_) {
    assert(false);
    SEXP rhs = ostack_pop(ctx);
    SEXP lhs = ostack_pop(ctx);
    if (TYPEOF(lhs) == REALSXP && TYPEOF(rhs) == REALSXP &&
        Rf_length(lhs) == 1 && Rf_length(rhs) == 1) {
        SEXP res = Rf_allocVector(REALSXP, 1);
        SET_NAMED(res, 1);
        REAL(res)[0] = REAL(lhs)[0] - REAL(rhs)[0];
        ostack_push(ctx, res);
    } else {
        NOT_IMPLEMENTED;
        // SEXP op = getPrimitive("-");
        // SEXP primfun = getPrimfun("-");
        // TODO we should change how primitives are called now that we can
        // integrate
        // SEXP res = callPrimitive(primfun, getCurrentCall(c, pc), op, { lhs,
        // rhs });
        // TODO push
    }
}

INSTRUCTION(lt_) {
    SEXP rhs = ostack_pop(ctx);
    SEXP lhs = ostack_pop(ctx);
    if (TYPEOF(lhs) == REALSXP && TYPEOF(rhs) == REALSXP &&
        Rf_length(lhs) == 1 && Rf_length(rhs) == 1) {
        SEXP res = Rf_allocVector(REALSXP, 1);
        SET_NAMED(res, 1);
        ostack_push(ctx,
                    REAL(lhs)[0] < REAL(rhs)[0] ? R_TrueValue : R_FalseValue);
    } else {
        NOT_IMPLEMENTED;
        // SEXP op = getPrimitive("<");
        // SEXP primfun = getPrimfun("<");
        // TODO we should change how primitives are called now that we can
        // integrate
        // SEXP res = callPrimitive(primfun, getCurrentCall(c, pc), op, { lhs,
        // rhs });
        // TODO push
    }
}

INSTRUCTION(isspecial_) {
    // TODO I do not think this is a proper way - we must check all the way
    // down, not just findVar (vars do not shadow closures)
    SEXP sym = readConst(ctx, pc);
    SEXP val = findVar(sym, env);
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

INSTRUCTION(inci_) { istack_push(ctx, istack_pop(ctx) + 1); }

INSTRUCTION(push_argi_) {
    int pos = istack_pop(ctx);
    ostack_push(ctx, cp_pool_at(ctx, bp - numArgs + pos));
}

INSTRUCTION(invisible_) {
    R_Visible = 0;
}

extern void printCode(Code* c);
extern void printFunction(Function* f);

extern SEXP Rf_deparse1(SEXP call, Rboolean abbrev, int opts);
extern void R_SetErrorHook(void (*hook)(SEXP, char *));

extern void rirBacktrace(Context* ctx) {
    if (fstack_empty(ctx))
        return;

    FStack* f = ctx->fstack;
    while(f) {
        for (int i = f->length - 1; i >= 0; i--) {
            Frame* frame = &f->data[i];
            Code* code = frame->code;
            SEXP call = src_pool_at(ctx, code->src);

            Rprintf("%d : %s\n", i, CHAR(STRING_ELT(Rf_deparse1(call, 0, 0), 0)));
            Rprintf(" env: ");
            SEXP names = R_lsInternal3(frame->env, TRUE, FALSE);
            PROTECT(names);
            for (int i = 0; i < Rf_length(names); ++i)
                Rprintf("%s ", CHAR(STRING_ELT(R_lsInternal3(frame->env, TRUE, FALSE), i)));
            Rprintf("\n");
        }
        f = f->prev;
    }
}

SEXP rirEval_c(Code* c, Context* ctx, SEXP env, unsigned numArgs) {

    // printCode(c);

    if (!env)
	error("'rho' cannot be C NULL: detected in C-level eval");
    if (!isEnvironment(env))
	error("'rho' must be an environment not %s: detected in C-level eval",
	      type2char(TYPEOF(env)));

    R_CheckStack();

    // make sure there is enough room on the stack
    ostack_ensureSize(ctx, c->stackLength);
    istack_ensureSize(ctx, c->iStackLength);
    Frame* frame = fstack_push(ctx, c, env);

    // get pc and bp regs, we do not need istack bp
    frame->pc = code(c);
    OpcodeT** pc = &frame->pc;
    size_t bp = ctx->ostack.length;

    R_Visible = TRUE;

    // main loop
    while (true) {
        // printf("%p : %p, %p\n", c, *pc, *pc - c->data);
        switch (readOpcode(pc)) {
#define INS(name)                                                              \
    case name:                                                                 \
        ins_##name(c, env, pc, ctx, numArgs, bp);                              \
        break
            INS(push_);
            INS(ldfun_);
            INS(ldvar_);
            INS(ldddvar_);
            INS(call_);
            INS(promise_);
            INS(close_);
            INS(force_);
            INS(pop_);
            INS(pusharg_);
            INS(asast_);
            INS(stvar_);
            INS(asbool_);
            INS(brtrue_);
            INS(brfalse_);
            INS(br_);
            INS(lti_);
            INS(eqi_);
            INS(pushi_);
            INS(dupi_);
            INS(dup_);
            INS(add_);
            INS(sub_);
            INS(lt_);
            INS(isspecial_);
            INS(isfun_);
            INS(inci_);
            INS(push_argi_);
            INS(invisible_);
        case ret_: {
            // not in its own function so that we can avoid nonlocal returns
            goto __eval_done;
        }
        default:
            assert(false && "wrong or unimplemented opcode");
        }
    }
__eval_done:
    fstack_pop(ctx, frame);
    return ostack_pop(ctx);
}

SEXP rirExpr(SEXP f) {
    if (isValidPromise(f)) {
        Code* c = (Code*)f;
        return src_pool_at(globalContext(), c->src);
    }
    if (isValidFunction(f)) {
        Function* ff = (Function*)(INTEGER(f));
        return src_pool_at(globalContext(), functionCode(ff)->src);
    }
    return f;
}

SEXP rirEval_f(SEXP f, SEXP env) {
    // TODO we do not really need the arg counts now
    if (isValidPromise(f)) {
        //        Rprintf("Evaluating promise:\n");
        Code* c = (Code*)f;
        SEXP x = rirEval_c(c, globalContext(), env, 0);
      //        Rprintf("Promise evaluated, length %u, value %d",
        //        Rf_length(x), REAL(x)[0]);
        return x;
    } else {
        //        Rprintf("=====================================================\n");
        //        Rprintf("Evaluating function\n");
        Function* ff = (Function*)(INTEGER(f));
        return rirEval_c(functionCode(ff), globalContext(), env, 0);
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
        if (isValidFunction(e)) {
            Function* ff = (Function*)(INTEGER(e));
            return rirEval_c(functionCode(ff), globalContext(), env, 0);
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
        assert(isValidPromise(e));
        return rirEval_c(c, globalContext(), env, 0);
    }

    case NATIVESXP:
        assert(false);
        break;

    case BCODESXP: {
        SEXP expr = VECTOR_ELT(CDR(e), 0);
        SEXP code = globalContext()->compiler(expr, env);
        PROTECT(code);
        Function* ff = (Function*)(INTEGER(code));
        SEXP res = rirEval_c(functionCode(ff), globalContext(), env, 0);
        UNPROTECT(1);
        return res;
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
            val = promiseValue(val);

        // WTF? is this just defensive programming or what?
        if (NAMED(val) == 0 && val != R_NilValue)
            SET_NAMED(val, 1);

        return val;
        break;
    }

    case PROMSXP:
        return promiseValue(e);

    case LANGSXP: {
        SEXP code = globalContext()->compiler(e, env);
        PROTECT(code);
        Function* ff = (Function*)(INTEGER(code));
        SEXP res = rirEval_c(functionCode(ff), globalContext(), env, 0);
        UNPROTECT(1);
        return res;
    }

    case DOTSXP:
	error("'...' used in an incorrect context");
    default:
        assert(false && "UNIMPLEMENTED_TYPE");
    }

    assert(false);
    return R_NilValue;
}
