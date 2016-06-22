#include <assert.h>

#include "interp.h"
#include "interpreter_context.h"

// GNU-R stuff we need

extern SEXP R_TrueValue;
extern SEXP R_FalseValue;
extern SEXP Rf_NewEnvironment(SEXP, SEXP, SEXP);
extern Rboolean R_Visible;
extern SEXP forcePromise(SEXP);

extern SEXP mkPROMISE(SEXP expr, SEXP rho);

// helpers

/** Moves the pc to next instruction, based on the current instruction length
 */
OpcodeT * advancePc(OpcodeT * pc) {
    switch (*pc++) {
/*
#define DEF_INSTR(name, imm, ...) case name : pc += sizeof(ArgT) * imm; break;
#include "insns.h"
*/
    default:
        assert(false && "Unknown instruction");
    }
    return pc;
}

/** How many bytes do we need to align on 4 byte boundary?
 */
unsigned pad4(unsigned sizeInBytes) {
    unsigned x = sizeInBytes % 4;
    return (x != 0) ? (sizeInBytes + 4 - x) : sizeInBytes;
}

// Code object --------------------------------------------------------------------------------------------------------

OpcodeT* code(Code* c) {
    return (OpcodeT*)c->data;
}

unsigned* src(Code* c) {
    return (unsigned*)(c->data + pad4(c->codeSize));
}

struct Function* function(Code* c) {
    return (struct Function*)(c - c->header);
}

Code* next(Code* c) {
    return (Code*)(c->data + pad4(c->codeSize) + c->srcLength);
}

// Function -----------------------------------------------------------------------------------------------------------

bool isValidFunction(SEXP s) {
    if (TYPEOF(s) != INTSXP)
        return false;
    return INTEGER(s)[0] == 0xCAFEBABE;
}

Function* origin(Function* f) {
    // TODO
}

Code* begin(Function* f) {
    return f->data;
}

Code* end(Function* f) {
    return (Code*)((uint8_t*)f->data + f->size);
}

Code * codeAt(Function * f, unsigned offset) {
    return (Code*)((uint8_t*)f + offset);
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



// TODO check if there is a function for this in R
INLINE SEXP promiseValue(SEXP promise) {
    // if already evaluated, return the value
    if (PRVALUE(promise) && PRVALUE(promise) != R_UnboundValue)
    {
        promise = PRVALUE(promise);
        SET_NAME(promise, 2);
        return promise; 
    }
    return forcePromise(promise);
}

// TODO perhaps this should have better name
INLINE SEXP getCurrentCall(Code * c, OpcodeT * pc) {
    // we need to determine index of the current instruction
    OpcodeT * x = code(c);
    // find the pc of the current instructions, it is ok to be slow
    unsigned insIdx = 0;
    while ((x = advancePc(x)) != pc)
        ++insIdx;
    unsigned sidx = src(c)[insIdx];
    // return the ast for the instruction, or if not defined, the ast of the function
    return source(sidx == 0 ? c->src : sidx);
}

// FORMALS BODY CLOENV

INLINE void matchArguments(SEXP cls) {
    // Specials and builtins do not care about names
    if (TYPEOF(cls) == SPECIALSXP || TYPEOF(cls) == BUILTINSXP)
        return;
    // therefore it must be closure
    assert(TYPEOF(cls) == CLOSXP);
    // get the formals
    SEXP formals = FORMALS(cls);
    // prepare matching structures
    unsigned * matched = alloca(Rf_length(formals) * sizeof(unsigned));
    bool * used = alloca(Rf_length(formals) + sizeof(unsigned));
    //
    for (size_t i = 0, e = Rf_length(formals); i != e; ++i) {
        matched[i] = 0;
        used[i] = false;
    }




}


INLINE SEXP doCall() {


}



void gc_callback(void (*forward_node)(SEXP)) {
/*    for (size_t i = 0; i < stack_.length; ++i)
        forward_node(stack_.stack[i]);
    forward_node(cp_.pool);
    forward_node(src_.pool); */
    // TODO HUGE TODO
}


SEXP rirEval_c(Code* c, Context* ctx, SEXP env, unsigned numArgs) {
    SEXP call = constant(c->src);
    // make sure there is enough room on the stack
    ostack_ensureSize(ctx, c->stackLength);
    istack_ensureSize(ctx, c->iStackLength);

    // get pc and bp regs, we do not need istack bp
    OpcodeT * pc = code(c);
    size_t bp = ostack_length(ctx);

    // main loop
    while (true) {
        switch (readOpcode(&pc)) {
        case push_: {
            SEXP x = readConst(ctx, &pc);
            ostack_push(c, x);
            break;
        }
        case ldfun_: {
            SEXP sym = readConst(ctx, &pc);
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
                // TODO we do not need lazy compiling anymore
                /*
                val = (SEXP)jit(val);
                */
                break;
            case SPECIALSXP:
            case BUILTINSXP: {
                // TODO fix this
                /**
                SEXP prim = Primitives::compilePrimitive(val);
                if (prim)
                    val = prim;
                **/
                break;
            }
            default:
                // TODO!
                assert(false);
            }
            ostack_push(ctx, val);
            break;
        }
        case ldvar_: {
            SEXP sym = readConst(ctx, &pc);
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
            break;
        }
        case call_: {
            // one huge large big **** T O D O ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
            // get the indices of argument promises
            SEXP args_ = readConst(ctx, &pc);
            assert(TYPEOF(args_) == INTSXP && "TODO change to INTSXP, not RAWSXP it used to be");
            unsigned nargs = Rf_length(args_);
            unsigned * args = (unsigned*)INTEGER(args_);
            // get the names of the arguments (or R_NilValue) if none
            SEXP names = readConst(ctx, &pc);
            // get the closure itself
            SEXP cls = ostack_pop(ctx);
            // match the arguments and do the call
            if (names) {
                //
            } else {


                ostack_push(c, doCall());

            }
            break;
        }
        case promise_: {
            unsigned codeOffset = readImmediate(&pc);
            Code * promiseCode = codeAt(function(ctx), codeOffset);
            // TODO make the promise with current environment and the code object
            // and push it to the stack

            // This should be similar to mkPromise/createPromise
            break;
        }
        case close_: {
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
            break;
        }
        case ret_: {
            return ostack_pop(ctx);
        }
        case force_: {
            SEXP p = ostack_pop(ctx);
            assert(TYPEOF(p) == PROMSXP);
            // If the promise is already evaluated then push the value inside the promise
            // onto the stack, otherwise push the value from forcing the promise 
            if (PRVALUE(p) && PRVALUE(p) != R_UnboundValue){
                ostack_push(ctx, PRVALUE(p));
                SET_NAMED(p, 2);
            } else {
                ostack_push(ctx, forcePromise(p));
            }
            break;
        }
        case pop_: {
            ostack_pop(ctx);
            break;
        }
        case pusharg_: {
            unsigned n = readImmediate(&pc);
            assert(n < numArgs);
            ostack_push(ctx, ostack_at(ctx, bp - numArgs + n));
            break;
        }
        case asast_: {
            SEXP p = ostack_pop(ctx);
            assert(TYPEOF(p) == PROMSXP);
            // TODO get the ast depending on what type of promise it is and push it on the stack
            // What are the different types of promises?
            // evaluated promise
            
            break;
        }
        case stvar_: {
            SEXP sym = ostack_pop(ctx);
            assert(TYPEOF(sym) == SYMSXP);
            SEXP val = ostack_pop(ctx);
            defineVar(sym, val, env);
            ostack_push(ctx, val);
            break;
        }
        case asbool_: {
            SEXP t = ostack_pop(ctx);
            int cond = NA_LOGICAL;
            if (Rf_length(t) > 1)
                warningcall(getCurrentCall(c, pc),
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
                errorcall(getCurrentCall(c, pc), msg);
            }

            ostack_push(ctx, cond ? R_TrueValue : R_FalseValue);
            break;
        }
        case brtrue_: {
            int offset = readJumpOffset(&pc);
            if (ostack_pop(ctx) == R_TrueValue)
                pc = pc + offset;
            break;
        }
        case brfalse_: {
            int offset = readJumpOffset(&pc);
            if (ostack_pop(ctx) == R_FalseValue)
                pc = pc + offset;
            break;
        }
        case br_: {
            pc = pc + readJumpOffset(&pc);
            break;
        }
        case lti_: {
            int rhs = istack_pop(ctx);
            int lhs = istack_pop(ctx);
            ostack_push(ctx, lhs < rhs ? R_TrueValue : R_FalseValue);
            break;
        }
        case eqi_: {
            int rhs = istack_pop(ctx);
            int lhs = istack_pop(ctx);
            ostack_push(ctx, lhs == rhs ? R_TrueValue : R_FalseValue);
            break;
        }
        case pushi_: {
            istack_push(ctx, readSignedImmediate(&pc));
            break;
        }
        case dupi_: {
            istack_push(ctx, istack_top(ctx));
            break;
        }
        case dup_: {
            ostack_push(ctx, ostack_top(ctx));
            break;
        }
        // TODO add sub lt should change!
        case add_: {
            SEXP rhs = ostack_pop(ctx);
            SEXP lhs = ostack_pop(ctx);
            if (TYPEOF(lhs) == REALSXP && TYPEOF(rhs) == REALSXP && Rf_length(lhs) == 1 && Rf_length(rhs) == 1) {
                SEXP res = Rf_allocVector(REALSXP, 1);
                SET_NAMED(res, 1);
                REAL(res)[0] = REAL(lhs)[0] + REAL(rhs)[0];
                ostack_push(ctx, res);
            } else {
                SEXP op = getPrimitive("+");
                SEXP primfun = getPrimfun("+");
                // TODO we should change how primitives are called now that we can integrate
                //SEXP res = callPrimitive(primfun, getCurrentCall(c, pc), op, { lhs, rhs });
                // TODO push
            }
            break;
        }
        case sub_: {
            SEXP rhs = ostack_pop(ctx);
            SEXP lhs = ostack_pop(ctx);
            if (TYPEOF(lhs) == REALSXP && TYPEOF(rhs) == REALSXP && Rf_length(lhs) == 1 && Rf_length(rhs) == 1) {
                SEXP res = Rf_allocVector(REALSXP, 1);
                SET_NAMED(res, 1);
                REAL(res)[0] = REAL(lhs)[0] - REAL(rhs)[0];
                ostack_push(ctx, res);
            } else {
                SEXP op = getPrimitive("-");
                SEXP primfun = getPrimfun("-");
                // TODO we should change how primitives are called now that we can integrate
                //SEXP res = callPrimitive(primfun, getCurrentCall(c, pc), op, { lhs, rhs });
                // TODO push
            }
            break;
        }
        case lt_: {
            SEXP rhs = ostack_pop(ctx);
            SEXP lhs = ostack_pop(ctx);
            if (TYPEOF(lhs) == REALSXP && TYPEOF(rhs) == REALSXP && Rf_length(lhs) == 1 && Rf_length(rhs) == 1) {
                SEXP res = Rf_allocVector(REALSXP, 1);
                SET_NAMED(res, 1);
                ostack_push(ctx, REAL(lhs)[0] < REAL(rhs)[0] ? R_TrueValue : R_FalseValue);
            } else {
                SEXP op = getPrimitive("<");
                SEXP primfun = getPrimfun("<");
                // TODO we should change how primitives are called now that we can integrate
                //SEXP res = callPrimitive(primfun, getCurrentCall(c, pc), op, { lhs, rhs });
                // TODO push
            }
            break;
        }
        case isspecial_: {
            // TODO I do not think this is a proper way - we must check all the way down, not just findVar (vars do not shadow closures)
            SEXP sym = readConst(ctx, &pc);
            SEXP val = findVar(sym, env);
            // TODO better check
            assert(TYPEOF(val) == SPECIALSXP || TYPEOF(val) == BUILTINSXP);
            break;
        }
        case isfun_: {
            SEXP val = ostack_top(ctx);

            switch (TYPEOF(val)) {
            case CLOSXP:
                /** No need to compile, we can handle functions */
                //val = (SEXP)jit(val);
                break;
            case SPECIALSXP:
            case BUILTINSXP: {
                // TODO do we need to compile primitives? not really I think
                /*
                SEXP prim = Primitives::compilePrimitive(val);
                if (prim)
                    val = prim;
                break; */
            }

            default:
                // TODO: error not a function!
                // TODO: check our functions and how they are created
                assert(false);
            }
            break;
        }
        case inci_: {
            istack_push(ctx, istack_pop(ctx) + 1);
            break;
        }
        case push_argi_: {
            int pos = istack_pop(ctx);
            ostack_push(ctx, cp_pool_at(ctx, bp - numArgs + pos));
            break;
        }
        default:
            assert(false && "wrong or unimplemented opcode");
        }
    }

}
