#include <assert.h>

#include "interpreter.h"

// TODO force inlinine for clang & gcc
#define INLINE __attribute__((always_inline)) inline


// GNU-R stuff we need

extern SEXP R_TrueValue;
extern SEXP R_FalseValue;
extern SEXP Rf_NewEnvironment(SEXP, SEXP, SEXP);
extern Rboolean R_Visible;
extern SEXP forcePromise(SEXP);

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

Function* function(Code* c) {
    return (Function*)(c - c->header);
}

Code* next(Code* c) {
    return (Code*)(c->data + pad4(c->codeSize) + c->srcLength);
}

// Function -----------------------------------------------------------------------------------------------------------

bool isValidFunction(SEXP s) {
    if (TYPEOF(s) != INTSXP)
        return false;
    // TODO check magicVersion
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

// Runtime support ----------------------------------------------------------------------------------------------------

/** SEXP stack
 */
typedef struct {
    SEXP* stack;
    size_t length;
    size_t capacity;
} Stack;

Stack stack_;

INLINE bool stackEmpty() {
    return stack_.length == 0;
}

INLINE SEXP at(unsigned index) {
    return stack_.stack[index];
}

INLINE SEXP pop() {
    return stack_.stack[--stack_.length];
}

INLINE SEXP top() {
    return stack_.stack[stack_.length];
}

INLINE void push(SEXP val) {
    stack_.stack[stack_.length++] = val;
}

void checkStackSize(unsigned minFree) {
    unsigned newCap = stack_.capacity;
    while (stack_.length + minFree < newCap)
        newCap *= 2;
    if (newCap != stack_.capacity) {
        SEXP * newStack = malloc(newCap * sizeof(SEXP));
        memcpy(newStack, stack_.stack, stack_.length * sizeof(SEXP));
        free(stack_.stack);
        stack_.stack = newStack;
        stack_.capacity = newCap;
    }
}

/** Unboxed integer stack

  (separated because of the gc, should be merged in stack later on)
 */
typedef struct {
    int* stack;
    size_t length;
    size_t capacity;

} iStack;

iStack istack_;

INLINE bool iStackEmpty() {
    return istack_.length == 0;
}

INLINE int iPop() {
    return istack_.stack[--istack_.length];
}

INLINE int iTop() {
    return istack_.stack[istack_.length];
}

INLINE void iPush(int val) {
    istack_.stack[istack_.length++] = val;
}

void iCheckStackSize(unsigned minFree) {
    unsigned newCap = istack_.capacity;
    while (istack_.length + minFree < newCap)
        newCap *= 2;
    if (newCap != istack_.capacity) {
        int * newStack = malloc(newCap * sizeof(int));
        memcpy(newStack, istack_.stack, istack_.length * sizeof(int));
        free(istack_.stack);
        istack_.stack = newStack;
        istack_.capacity = newCap;
    }
}

/** Constant and ast pools

 */

typedef struct {
    SEXP pool;
    size_t length;
    size_t capacity;
} Pool;

Pool createPool(size_t capacity) {
    Pool result;
    result.length = 0;
    result.capacity = capacity;
    result.pool = Rf_allocVector(VECSXP, capacity);
    // add to precious list
    //Precious::add(result.pool);
}

Pool cp_;
Pool src_;

// grow the size of the pool
void grow(Pool * p) {
    size_t tmp = p->capacity;
    // grow capacity 2 times
    p->capacity = tmp * 2;

    // allocate new pool
    SEXP temp = Rf_allocVector(VECSXP, p->capacity);
    //poolAdd(temp);

    // transfer values over
    for (size_t i = 0; i <= tmp; ++i){
        SET_VECTOR_ELT(temp, i, VECTOR_ELT(p->pool, i));
    }

    // remove the old pool
    //poolRemove(p->pool);
    p->pool = temp;

    // add the new pool, and remove the temp pool
    //poolAdd(p->pool);
    //poolRemove(temp);
}

/** Constant pool */

INLINE SEXP constant(size_t index) {
    return VECTOR_ELT(cp_.pool, index);
}

INLINE size_t addConstant(SEXP value) {

    // check the capacity of the constant pool
    if(cp_.capacity <= cp_.length){
        grow(&cp_);
    } 

    // allocate value into the constant pool
    SET_VECTOR_ELT(cp_.pool, cp_.length, value);
    cp_.length = cp_.length + 1;
    
    // return the position length? 
    return cp_.length;
}

/** AST (source) pool */

INLINE SEXP source(size_t index) {
    return VECTOR_ELT(src_.pool, index);
}

INLINE size_t addSource(SEXP value) {
    // check the capacity of the constant pool
    if(src_.capacity <= src_.length){
        grow(&src_);
    } 

    // allocate value into the constant pool
    SET_VECTOR_ELT(src_.pool, src_.length, value);
    src_.length = src_.length + 1;
    
    // return the position length? 
    return src_.length;
}

/** */

typedef struct {
    size_t length;
    size_t size;
    SEXP data[size];
} Prot;

Prot createPool(size_t size, SEXP[] data) {
    Prot result;
    result.length = 0;
    result.size = size;
    result.data = data;
}

Prot pp_;

INLINE void poolGcCallBack(void (*forward_node)(SEXP)){

}


INLINE void poolAdd(SEXP value){

}


INLINE void poolRemove(SEXP value){

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

INLINE SEXP readConst(OpcodeT ** pc) {
    return constant(readImmediate(pc));
}

INLINE int readJumpOffset(OpcodeT** pc) {
    int result = (JumpOffset)(**pc);
    *pc += sizeof(JumpOffset);
    return result;
}








// TODO check if there is a function for this in R
INLINE SEXP promiseValue(SEXP promise) {
    // TODO if already evaluated, return the value
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






SEXP rirEval_c(Code* c, SEXP env, unsigned numArgs) {
    SEXP call = constant(c->src);
    // make sure there is enough room on the stack
    checkStackSize(c->stackLength);
    iCheckStackSize(c->iStackLength);

    // get pc and bp regs, we do not need istack bp
    OpcodeT * pc = code(c);
    size_t bp = stack_.length;

    // main loop
    while (true) {
        switch (readOpcode(&pc)) {
        case push_: {
            SEXP x = readConst(&pc);
            push(x);
            break;
        }
        case ldfun_: {
            SEXP sym = readConst(&pc);
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
            push(val);
            break;
        }
        case ldvar_: {
            SEXP sym = readConst(&pc);
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

            push(val);
            break;
        }
        case call_: {
            // one huge large big **** T O D O ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
            break;
        }
        case promise_: {
            unsigned codeOffset = readImmediate(&pc);
            Code * promiseCode = codeAt(function(c), codeOffset);
            // TODO make the promise with current environment and the code object
            // and push it to the stack
            break;
        }
        case close_: {
            SEXP body = pop();
            SEXP arglist = pop();
            // TODO create cloisure (R's ) from the arglist and body and env and push it
            break;
        }
        case ret_: {
            return pop();
        }
        case force_: {
            SEXP p = pop();
            assert(TYPEOF(p) == PROMSXP);
            // TODO what if it is forced?
            push(forcePromise(p));
            break;
        }
        case pop_: {
            pop();
            break;
        }
        case pusharg_: {
            unsigned n = readImmediate(&pc);
            assert(n < numArgs);
            push(at(bp - numArgs + n));
            break;
        }
        case asast_: {
            SEXP p = pop();
            assert(TYPEOF(p) == PROMSXP);
            // TODO get the ast depending on what type of promise it is and push it on the stack
            break;
        }
        case stvar_: {
            SEXP sym = pop();
            assert(TYPEOF(sym) == SYMSXP);
            SEXP val = pop();
            defineVar(sym, val, env);
            push(val);
            break;
        }
        case asbool_: {
            SEXP t = pop();
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

            push(cond ? R_TrueValue : R_FalseValue);
            break;
        }
        case brtrue_: {
            int offset = readJumpOffset(&pc);
            if (pop() == R_TrueValue)
                pc = pc + offset;
            break;
        }
        case brfalse_: {
            int offset = readJumpOffset(&pc);
            if (pop() == R_FalseValue)
                pc = pc + offset;
            break;
        }
        case br_: {
            pc = pc + readJumpOffset(&pc);
            break;
        }
        case lti_: {
            int rhs = iPop();
            int lhs = iPop();
            push(lhs < rhs ? R_TrueValue : R_FalseValue);
        }
        case eqi_: {
            int rhs = iPop();
            int lhs = iPop();
            push(lhs == rhs ? R_TrueValue : R_FalseValue);
        }
        case pushi_:
        case dupi_:
        case dup_:
        case add_:
        case sub_:
        case lt_:
        case isspecial_:
        case isfun_:
        default:
            assert(false && "wrong or unimplemented opcode");
        }
    }




}
