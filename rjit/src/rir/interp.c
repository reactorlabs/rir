#include <assert.h>

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




SEXP forcePromise(SEXP what) {
    return hook_forcePromise(what);
}

SEXP mkPROMISE(SEXP expr, SEXP rho) {
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
} sxpifo_struct_rjit;                          /*		    Tot: 32 */

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
OpcodeT * advancePc(OpcodeT * pc) {
    switch (*pc++) {
#define DEF_INSTR(name, imm, ...) case name : pc += sizeof(ArgT) * imm; break;
#include "insns.h"
    default:
        assert(false && "Unknown instruction");
    }
    return pc;
}

// bytecode accesses


INLINE Opcode readOpcode(OpcodeT** pc) {
    Opcode result = (Opcode)(**pc);
    *pc += sizeof(OpcodeT);
    return result;
}

INLINE unsigned readImmediate(OpcodeT** pc) {
    unsigned result = (Immediate)(**pc);
    *pc += sizeof(Immediate);
    return result;
}

INLINE int readSignedImmediate(OpcodeT** pc) {
    int result = (SignedImmediate)(**pc);
    *pc += sizeof(SignedImmediate);
    return result;
}

INLINE SEXP readConst(Context * ctx, OpcodeT ** pc) {
    return cp_pool_at(ctx, readImmediate(pc));
}

INLINE int readJumpOffset(OpcodeT** pc) {
    int result = (JumpOffset)(**pc);
    *pc += sizeof(JumpOffset);
    return result;
}

// TODO perhaps this should have better name
INLINE SEXP getCurrentCall(Code * c, OpcodeT * pc, Context * ctx) {
    // we need to determine index of the current instruction
    OpcodeT * x = code(c);
    // find the pc of the current instructions, it is ok to be slow
    unsigned insIdx = 0;
    while ((x = advancePc(x)) != pc)
        ++insIdx;
    unsigned sidx = src(c)[insIdx];
    // return the ast for the instruction, or if not defined, the ast of the function
    return src_pool_at(ctx, sidx == 0 ? c->src : sidx);
}

/** Creates a promise from given code object and environment.

 */
INLINE SEXP createPromise(Code * code, SEXP env) {
    return mkPROMISE((SEXP) code, env);
}

// TODO check if there is a function for this in R
INLINE SEXP promiseValue(SEXP promise) {
    // if already evaluated, return the value
    if (PRVALUE(promise) && PRVALUE(promise) != R_UnboundValue) {
        promise = PRVALUE(promise);
        SET_NAMED(promise, 2);
        return promise;
    }
    return forcePromise(promise);
}

// TODO remove numArgs and bp -- this is only needed for the on stack argument handling
#define INSTRUCTION(name) INLINE void ins_ ## name (Code * c, SEXP env, OpcodeT **pc, Context * ctx, unsigned numArgs, unsigned bp)

INSTRUCTION(push_) {
    SEXP x = readConst(ctx, pc);
    ostack_push(ctx, x);
}

INSTRUCTION(ldfun_) {
    SEXP sym = readConst(ctx, pc);
    SEXP val = findVar(sym, env);
    R_Visible = TRUE;

    // TODO something should happen here
    if (val == R_UnboundValue)
        assert(false && "Unbound var");
    else if (val == R_MissingArg)
        assert(false && "Missing argument");

    // if promise, evaluate & return
    if (TYPEOF(val) == PROMSXP)
        val = promiseValue(val);

    // WTF? is this just defensive programming or what?
    if (NAMED(val) == 0 && val != R_NilValue)
        SET_NAMED(val, 1);

    switch (TYPEOF(val)) {
    case CLOSXP:
        /** If compile on demand is active, check that the function to be called is compiled already, and compile if not.
         */
        if (COMPILE_ON_DEMAND) {
            SEXP body = BODY(val);
            assert(TYPEOF(body) != BCODESXP && "We do not plan to handle GNU-R bytecode");
            if (TYPEOF(body) != INTSXP)
                SET_BODY(val, ctx->compiler(body));
        }
        break;
    case SPECIALSXP:
    case BUILTINSXP:
        // special and builtin functions are ok
        break;
    default:
        // TODO!
        assert(false);
    }
    ostack_push(ctx, val);
}

INSTRUCTION(ldvar_) {
    SEXP sym = readConst(ctx, pc);
    SEXP val = findVar(sym, env);
    R_Visible = TRUE;

    // TODO better errors
    if (val == R_UnboundValue)
        assert(false && "Unbound var");
    else if (val == R_MissingArg)
        assert(false && "Missing argument");

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
// TODO This is a copy from old code, but doesn't it reverse arguments order?
SEXP createArgsList(Code * c, FunctionIndex * args, size_t nargs, SEXP env) {
    SEXP result = R_NilValue;
    for (size_t i = 0; i < nargs; ++i) {
        unsigned offset = args[i];
        SEXP arg = (offset == MISSING_ARG_OFFSET) ? R_MissingArg : createPromise(codeAt(function(c), offset), env);
        result = CONS_NR(arg, result);
    }
    return result;
}

/** Returns us the CCODE object from R_FunTab based on name.

  TODO This exists in gnu-r (names.c), when integrated inside, we want to make use of it.
 */
CCODE getBuiltin(SEXP f) {
    int i = ((sexprec_rjit*)f)->u.i;
    return R_FunTab[i].cfun;
}

/** Performs the call.

  TODO this is currently super simple.

 */
SEXP doCall(Code * caller, SEXP call, SEXP callee, unsigned * args, size_t nargs, SEXP env, Context * ctx) {
    size_t oldbp = ctx->ostack.length;
    size_t oldbpi = ctx->istack.length;
    SEXP result = R_NilValue;
    switch (TYPEOF(callee)) {
    case SPECIALSXP: {
        // get the ccode
        CCODE f = getBuiltin(callee);
        // call it with the AST only
        result = f(call, callee, CDR(call), env);
        break;
    }
    case BUILTINSXP: {
        // get the ccode
        CCODE f = getBuiltin(callee);
        // create the argslist
        SEXP argslist = createArgsList(caller, args, nargs, env);
        // callit
        PROTECT(argslist);
        result = f(call, callee, argslist, env);
        UNPROTECT(1);
        break;
    }
    case CLOSXP: {
        SEXP formals = FORMALS(callee);
        assert (Rf_length(formals) == nargs && "Cannot handle different nargs yet");
        SEXP body = BODY(callee);
        SEXP argslist = createArgsList(caller, args, nargs, env);
        PROTECT(argslist);
        // if body is INTSXP, it is rir serialized code, execute it directly
        if (TYPEOF(body) == INTSXP) {
            SEXP newEnv = Rf_NewEnvironment(formals, argslist, CLOENV(callee));
            PROTECT(newEnv);
            result = rirEval_c(begin((Function*)INTEGER(callee)), ctx, newEnv, nargs);
            UNPROTECT(1);
        } else {
        // otherwise use R's own call mechanism
            result = applyClosure(call, callee, argslist, env, R_NilValue);
        }
        UNPROTECT(1); // argslist
        break;
    }
    default:
        assert(false && "Don't know how to run other stuff");
    }
    assert (oldbp == ctx->ostack.length && oldbpi == ctx->istack.length && "Corrupted stacks");
    return result;
}

INLINE SEXP matchArgumentsAndCall(Code * caller, SEXP call, SEXP callee, unsigned * args, SEXP names, size_t nargs, SEXP env, Context * ctx) {
    // Specials and builtins do not care about names
    if (TYPEOF(callee) == SPECIALSXP || TYPEOF(callee) == BUILTINSXP)
        return doCall(caller, call, callee, args, nargs, env, ctx);
    // therefore it must be closure
    assert(TYPEOF(callee) == CLOSXP);
    // get the formals
    SEXP formals = FORMALS(callee);
    // prepare matching structures
    unsigned * matched = alloca(Rf_length(formals) * sizeof(unsigned));
    bool * used = alloca(Rf_length(formals) + sizeof(unsigned));
    size_t i = 0;
    size_t e = Rf_length(formals);
    while (i < e) {
        matched[i] = 0;
        used[i] = false;
        i++;
    }

    // TODO do ellipsis

    size_t finger = 0;
    size_t positional = 0;
    SEXP formalsIter = formals;

    for (size_t i = 0, e = Rf_length(formals); i != e; ++i, ++finger) {
        bool found = false;
        SEXP formal = CAR(formalsIter);
        formalsIter = CDR(formalsIter);

        // check if any of the supplied args has a matching tag
        for (size_t current = 0, ii = 0, ee = Rf_length(names); ii != ee; ++ii) {
            SEXP supplied = VECTOR_ELT(names, ii);
            if (used[current] || supplied == R_NilValue)
                continue;
            if (TAG(formal) != supplied) // fine, it is a symbol
                continue;
            // TODO error - same name given twice
            if (found)
                assert(false);
            // match
            matched[finger] = args[current];
            used[current] = 1;
            ++current;
        }

        // check if any of the supplied args has a partially matching tag
        if (! found) {
            for (size_t current = 0, ii = 0, ee = Rf_length(names); ii != ee; ++ii) {
                SEXP supplied = VECTOR_ELT(names, ii);
                if (used[current] || supplied == R_NilValue)
                    continue;
                // check partially matching name
                char const * given = CHAR(PRINTNAME(supplied));
                char const * f = CHAR(PRINTNAME(TAG(formal)));
                if (abs(strncmp(f, given, strlen(given))) != 0)
                    continue;
                // TODO error - partially matches two or more args
                if (found)
                    assert(false);
                // match
                matched[finger] = args[current];
                used[current] = 1;
                ++current;
            }
        }

        // no match, do positionally
        if (! found) {
            while (positional < nargs) {
                if (VECTOR_ELT(names, positional++) == R_NilValue) {
                    found = true;
                    matched[finger] = args[positional - 1]; // because ++ in loop
                    used[positional - 1] = 1;
                    break;
                }
            }
        }

        // cannot match? (no more positional left

        if (! found)
            matched[finger] = MISSING_ARG_OFFSET;
    }

    // arguments have been matched, do the call
    return doCall(caller, call, callee, matched, Rf_length(formals), env, ctx);
}

INSTRUCTION(call_) {
    // get the indices of argument promises
    SEXP args_ = readConst(ctx, pc);
    assert(TYPEOF(args_) == INTSXP && "TODO change to INTSXP, not RAWSXP it used to be");
    unsigned nargs = Rf_length(args_);
    unsigned * args = (unsigned*)INTEGER(args_);
    // get the names of the arguments (or R_NilValue) if none
    SEXP names = readConst(ctx, pc);
    // get the closure itself
    SEXP cls = ostack_pop(ctx);
    // match the arguments and do the call
    if (names)
        ostack_push(ctx, matchArgumentsAndCall(c,getCurrentCall(c, *pc, ctx), cls, args, names, nargs, env, ctx));
    else
        ostack_push(ctx, doCall(c, getCurrentCall(c, *pc, ctx), cls, args, nargs, env, ctx));
}

INSTRUCTION(promise_) {
    // get the Code * pointer we need
    unsigned codeOffset = readImmediate(pc);
    Code * promiseCode = codeAt(function(c), codeOffset);
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
    // If the promise is already evaluated then push the value inside the promise
    // onto the stack, otherwise push the value from forcing the promise
    ostack_push(ctx, promiseValue(p));
}

INSTRUCTION(pop_) {
    ostack_pop(ctx);
}

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
        warningcall(getCurrentCall(c, *pc, ctx),
                    ("the condition has length > 1 and only the first "
                     "element will be used"));

    if (Rf_length(t) > 0) {
        switch(TYPEOF(t)) {
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
                ? (isLogical(t)
                       ? ("missing value where TRUE/FALSE needed")
                       : ("argument is not interpretable as logical"))
                : ("argument is of length zero");
        errorcall(getCurrentCall(c, *pc, ctx), msg);
    }

    ostack_push(ctx, cond ? R_TrueValue : R_FalseValue);
}

INSTRUCTION(brtrue_) {
    int offset = readJumpOffset(pc);
    if (ostack_pop(ctx) == R_TrueValue)
        pc = pc + offset;
}

INSTRUCTION(brfalse_) {
    int offset = readJumpOffset(pc);
    if (ostack_pop(ctx) == R_FalseValue)
        pc = pc + offset;
}

INSTRUCTION(br_) {
    pc = pc + readJumpOffset(pc);
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

INSTRUCTION(pushi_) {
    istack_push(ctx, readSignedImmediate(pc));
}

INSTRUCTION(dupi_) {
    istack_push(ctx, istack_top(ctx));
}

INSTRUCTION(dup_) {
    ostack_push(ctx, ostack_top(ctx));
}

INSTRUCTION(add_) {
    SEXP rhs = ostack_pop(ctx);
    SEXP lhs = ostack_pop(ctx);
    if (TYPEOF(lhs) == REALSXP && TYPEOF(rhs) == REALSXP && Rf_length(lhs) == 1 && Rf_length(rhs) == 1) {
        SEXP res = Rf_allocVector(REALSXP, 1);
        SET_NAMED(res, 1);
        REAL(res)[0] = REAL(lhs)[0] + REAL(rhs)[0];
        ostack_push(ctx, res);
    } else {
        NOT_IMPLEMENTED;
        //SEXP op = getPrimitive("+");
        //SEXP primfun = getPrimfun("+");
        // TODO we should change how primitives are called now that we can integrate
        //SEXP res = callPrimitive(primfun, getCurrentCall(c, pc), op, { lhs, rhs });
        // TODO push
    }
}

INSTRUCTION(sub_) {
    SEXP rhs = ostack_pop(ctx);
    SEXP lhs = ostack_pop(ctx);
    if (TYPEOF(lhs) == REALSXP && TYPEOF(rhs) == REALSXP && Rf_length(lhs) == 1 && Rf_length(rhs) == 1) {
        SEXP res = Rf_allocVector(REALSXP, 1);
        SET_NAMED(res, 1);
        REAL(res)[0] = REAL(lhs)[0] - REAL(rhs)[0];
        ostack_push(ctx, res);
    } else {
        NOT_IMPLEMENTED;
        //SEXP op = getPrimitive("-");
        //SEXP primfun = getPrimfun("-");
        // TODO we should change how primitives are called now that we can integrate
        //SEXP res = callPrimitive(primfun, getCurrentCall(c, pc), op, { lhs, rhs });
        // TODO push
    }

}

INSTRUCTION(lt_) {
    SEXP rhs = ostack_pop(ctx);
    SEXP lhs = ostack_pop(ctx);
    if (TYPEOF(lhs) == REALSXP && TYPEOF(rhs) == REALSXP && Rf_length(lhs) == 1 && Rf_length(rhs) == 1) {
        SEXP res = Rf_allocVector(REALSXP, 1);
        SET_NAMED(res, 1);
        ostack_push(ctx, REAL(lhs)[0] < REAL(rhs)[0] ? R_TrueValue : R_FalseValue);
    } else {
        NOT_IMPLEMENTED;
        //SEXP op = getPrimitive("<");
        //SEXP primfun = getPrimfun("<");
        // TODO we should change how primitives are called now that we can integrate
        //SEXP res = callPrimitive(primfun, getCurrentCall(c, pc), op, { lhs, rhs });
        // TODO push
    }
}

INSTRUCTION(isspecial_) {
    // TODO I do not think this is a proper way - we must check all the way down, not just findVar (vars do not shadow closures)
    SEXP sym = readConst(ctx, pc);
    SEXP val = findVar(sym, env);
    // TODO better check
    assert(TYPEOF(val) == SPECIALSXP || TYPEOF(val) == BUILTINSXP);
}

INSTRUCTION(isfun_) {
    SEXP val = ostack_top(ctx);

    switch (TYPEOF(val)) {
    case CLOSXP:
        /** No need to compile, we can handle functions */
        //val = (SEXP)jit(val);
        break;
    case SPECIALSXP:
    case BUILTINSXP:
        // builtins and specials are fine
        // TODO for now - we might be fancier here later
        break;
/*

    {
        // TODO do we need to compile primitives? not really I think
        SEXP prim = Primitives::compilePrimitive(val);
        if (prim)
            val = prim;
        break;
    } */

    default:
        // TODO: error not a function!
        // TODO: check our functions and how they are created
        assert(false);
    }
}

INSTRUCTION(inci_) {
    istack_push(ctx, istack_pop(ctx) + 1);
}

INSTRUCTION(push_argi_) {
    int pos = istack_pop(ctx);
    ostack_push(ctx, cp_pool_at(ctx, bp - numArgs + pos));
}

SEXP rirEval_c(Code* c, Context* ctx, SEXP env, unsigned numArgs) {
    // make sure there is enough room on the stack
    ostack_ensureSize(ctx, c->stackLength);
    istack_ensureSize(ctx, c->iStackLength);

    // get pc and bp regs, we do not need istack bp
    OpcodeT * pc = code(c);
    size_t bp = ctx->ostack.length;

    // main loop
    while (true) {
        switch (readOpcode(&pc)) {
#define INS(name) case name : ins_ ## name (c, env, &pc, ctx, numArgs, bp); break
        INS(push_);
        INS(ldfun_);
        INS(ldvar_);
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
        case ret_: {
            // not in its own function so that we can avoid nonlocal returns
            return ostack_pop(ctx);
        }
        default:
            assert(false && "wrong or unimplemented opcode");
        }
    }

}


