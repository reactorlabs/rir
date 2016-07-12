#include "interp_context.h"

void initializeResizeableList(ResizeableList * l, size_t capacity, SEXP parent, size_t index) {
    l->capacity = capacity;
    l->list = Rf_allocVector(VECSXP, capacity);
    SET_VECTOR_ELT(parent, index, l->list);
    rl_setLength(l, 0);
}


Context* context_create(CompilerCallback compiler) {
    Context* c = malloc(sizeof(Context));
    c->list = Rf_allocVector(VECSXP, 3);
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
    return c;
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


