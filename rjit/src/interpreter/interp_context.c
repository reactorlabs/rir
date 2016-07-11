#include "interp_context.h"

void initializeResizeableList(ResizeableList * l, size_t capacity, SEXP parent, size_t index) {
    l->capacity = capacity;
    l->list = Rf_allocVector(VECSXP, capacity);
    SET_VECTOR_ELT(parent, index, l->list);
    rl_setLength(l, 0);
}


Context* context_create(CompilerCallback compiler) {
    Context* c = malloc(sizeof(Context));
    c->list = Rf_allocVector(VECSXP, 7);
    R_PreserveObject(c->list);
    initializeResizeableList(&c->cp, POOL_CAPACITY, c->list, CONTEXT_INDEX_CP);
    initializeResizeableList(&c->src, POOL_CAPACITY, c->list, CONTEXT_INDEX_SRC);
    initializeResizeableList(&c->ostack, STACK_CAPACITY, c->list, CONTEXT_INDEX_OSTACK);
    c->istack.data = malloc(STACK_CAPACITY * sizeof(int));
    c->istack.length = 0;
    c->istack.capacity = STACK_CAPACITY;
    c->fstack = malloc(sizeof(FStack));
    c->fstack->length = 0;
    c->fstack->prev = NULL;
    // first item in source and constant pools is R_NilValue so that we can use the index 0 for other purposes
    src_pool_add(c, R_NilValue);
    cp_pool_add(c, R_NilValue);
    // prepare the wrapper contexts
    SEXP bc = Rf_allocVector(INTSXP, 10);
    SET_VECTOR_ELT(c->list, CONTEXT_INDEX_WRAPPER_BYTECODE, bc);
    INTEGER(bc)[0] = 8;
    INTEGER(bc)[1] = 23;
    INTEGER(bc)[2] = 1;
    INTEGER(bc)[3] = 34;
    INTEGER(bc)[4] = 2;
    INTEGER(bc)[5] = 34;
    INTEGER(bc)[6] = 3;
    INTEGER(bc)[7] = 38;
    INTEGER(bc)[8] = 0;
    INTEGER(bc)[9] = 1;
    SEXP callSymbol = Rf_install(".Call");
    SET_VECTOR_ELT(c->list, CONTEXT_INDEX_WRAPPER_CALL, callSymbol);
    SEXP execSymbol = Rf_mkString("rir_executeWrapper");
    SET_VECTOR_ELT(c->list, CONTEXT_INDEX_WRAPPER_SYMBOL, execSymbol);
    SEXP ast = Rf_lang3(callSymbol, execSymbol, Rf_install("*the code*"));
    SET_VECTOR_ELT(c->list, CONTEXT_INDEX_WRAPPER_AST, ast);
    return c;
}

SEXP createBytecodeWrapper(SEXP closure, SEXP rirBytecode, Context * c) {
    SEXP cp = Rf_allocVector(VECSXP, 5);
    PROTECT(cp);
    SET_VECTOR_ELT(cp, 0, VECTOR_ELT(c->list, CONTEXT_INDEX_WRAPPER_AST));
    SET_VECTOR_ELT(cp, 1, VECTOR_ELT(c->list, CONTEXT_INDEX_WRAPPER_CALL));
    SET_VECTOR_ELT(cp, 2, VECTOR_ELT(c->list, CONTEXT_INDEX_WRAPPER_SYMBOL));
    SET_VECTOR_ELT(cp, 3, closure);
    SET_VECTOR_ELT(cp, 4, rirBytecode);
    SEXP result = Rf_allocSExp(BCODESXP);
    PROTECT(result);
    BCODE_CODE(result) = VECTOR_ELT(c->list, CONTEXT_INDEX_WRAPPER_BYTECODE);
    BCODE_CONSTS(result) = cp;
    BCODE_EXPR(result) = R_NilValue;
    UNPROTECT(2);
    return result;
}

// TODO for now we keep the context in a global value for easy gc
Context * globalContext_;

extern void R_SetErrorHook(void (*hook)(SEXP, char *));
extern void rirBacktrace(Context* ctx);

void rirErrorHook(SEXP call, char * msg) {
    Rprintf("RIR backtrace:\n");
    rirBacktrace(globalContext_);
    Rprintf("\n");
    Rf_errorcall(call, msg);
}

void interp_initialize(CompilerCallback compiler) {
    globalContext_ = context_create(compiler);
    // TODO: make this configurable
    // R_SetErrorHook(&rirErrorHook);
}

Context * globalContext() {
    return globalContext_;
}


