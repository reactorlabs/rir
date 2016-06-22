//
//  interpreter_context.c
//  
//
//  Created by Jan Vitek Jr on 6/21/16.
//
//
#include "interpreter_context.h"

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

#define POOL_CAPACITY 4096
#define STACK_CAPACITY 4096

Context* context_create() {
    Context* c = malloc(sizeof(Context));
    pool_init(&c->cp,POOL_CAPACITY);
    pool_init(&c->sp,POOL_CAPACITY);
    c->ostack.data = malloc(STACK_CAPACITY * sizeof(SEXP));
    c->ostack.length = 0;
    c->ostack.capacity = STACK_CAPACITY;
    c->istack.data = malloc(STACK_CAPACITY * sizeof(int));
    c->istack.length = 0;
    c->istack.capacity = STACK_CAPACITY;
    return c;
}

void pool_init(Pool* p, size_t capacity) {
    p->length = 0;
    p->capacity = capacity;
    p->data = Rf_allocVector(VECSXP, capacity);
}


void pool_grow(Pool* p) {
    p->capacity *= 2;
    SEXP temp = Rf_allocVector(VECSXP, p->capacity);

    for (size_t i = 0; i < p->length; ++i)
        SET_VECTOR_ELT(temp, i, VECTOR_ELT(p->data, i));

    p->data = temp;
}
