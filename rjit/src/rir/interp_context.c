//
//  interpreter_context.c
//  
//
//  Created by Jan Vitek Jr on 6/21/16.
//
//
#include "interp_context.h"

void fstack_grow(Context* c) {
    unsigned cap = c->fstack.capacity * 2;
    Frame* data = malloc(cap * sizeof(Frame));
    memcpy(data, c->fstack.data, c->fstack.length * sizeof(Frame));
    free(c->fstack.data);
    c->fstack.data = data;
    c->fstack.capacity = cap;
}


void ostack_ensureSize(Context* c, unsigned minFree) {
    unsigned cap = c->ostack.capacity;
    while(c->ostack.length + minFree < cap) cap *= 2;
    if (cap != c->ostack.capacity) {
        SEXP * data = malloc(cap * sizeof(SEXP));
        memcpy(data, c->ostack.data, c->ostack.length * sizeof(SEXP));
        free(c->ostack.data);
        c->ostack.data = data;
        c->ostack.capacity = cap;
    }
}

void istack_ensureSize(Context* c, unsigned minFree) {
    unsigned cap = c->istack.capacity;
    while (c->istack.length + minFree < cap)  cap *= 2;
    if (cap != c->istack.capacity) {
        int * data = malloc(cap * sizeof(int));
        memcpy(data, c->istack.data, c->istack.length * sizeof(int));
        free(c->istack.data);
        c->istack.data = data;
        c->istack.capacity = cap;
    }
}

Context* context_create(CompilerCallback compiler) {
    Context* c = malloc(sizeof(Context));
    pool_init(&c->cp, POOL_CAPACITY);
    pool_init(&c->src, POOL_CAPACITY);
    c->ostack.data = malloc(STACK_CAPACITY * sizeof(SEXP));
    c->ostack.length = 0;
    c->ostack.capacity = STACK_CAPACITY;
    c->istack.data = malloc(STACK_CAPACITY * sizeof(int));
    c->istack.length = 0;
    c->istack.capacity = STACK_CAPACITY;
    c->fstack.data = malloc(STACK_CAPACITY * sizeof(Frame));
    c->fstack.length = 0;
    c->fstack.capacity = STACK_CAPACITY;
    c->compiler = compiler;
    // first item in source and constant pools is R_NilValue so that we can use the index 0 for other purposes
    src_pool_add(c, R_NilValue);
    cp_pool_add(c, R_NilValue);
    return c;
}

void pool_init(Pool* p, size_t capacity) {
    p->length = 0;
    p->capacity = capacity;
    p->data = Rf_allocVector(VECSXP, capacity);
}


void pool_grow(Pool* p) {
    p->capacity *= 2;
    SEXP temp = Rf_allocVector(VECSXP, p->capacity);

    size_t i = 0;
    while ( i < p->length){
        SET_VECTOR_ELT(temp, i, VECTOR_ELT(p->data, i));
        i++;
    }

    p->data = temp;
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
    R_SetErrorHook(&rirErrorHook);
}

void rir_interp_gc_callback(void (*forward_node)(SEXP)) {
    for (size_t i = 0; i < globalContext_->ostack.length; ++i)
        forward_node(globalContext_->ostack.data[i]);
    forward_node(globalContext_->cp.data);
    forward_node(globalContext_->src.data);
}

Context * globalContext() {
    return globalContext_;
}


