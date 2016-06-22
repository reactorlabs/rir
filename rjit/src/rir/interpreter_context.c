//
//  interpreter_context.c
//  
//
//  Created by Jan Vitek Jr on 6/21/16.
//
//
#include "interpreter_context.h"
//
// Primitive stack.
//
typedef struct {
    int* data;
    size_t length;
    size_t capacity;

} PStack;

//
// Ostack:: SEXP stack, or object stack
//
typedef struct {
    SEXP* data;
    size_t length;
    size_t capacity;
} OStack;

//
// Pool
//
typedef struct {
    SEXP data;
    size_t length;
    size_t capacity;
} Pool;

//
// Context
//
typedef {
    Pool cp;
    Pool src;
    OStack ostack;
    PStack istack;
} Context;


INLINE bool ostack_empty(Context* c) {
    return c->ostack.length == 0;
}

INLINE SEXP ostack_at(Context* c, unsigned index) {
    return c->ostack.data[index];
}

INLINE SEXP ostack_pop(Context* c) {
    return c->ostack.data[--c->ostack.length];
}

INLINE SEXP ostack_top(Context* c) {
    return c->ostack.data[c->ostack.length];
}

INLINE void push(Context* c, SEXP val) {
    c->ostack.data[c->ostack.length++] = val;
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

INLINE bool istack_empty(Context* c) {
    return c->istack.length == 0;
}

INLINE int istack_pop(Context* c) {
    return c->istack.data[--c->istack.length];
}

INLINE int istack_top(Context* c) {
    return c->istack.data[c->istack.length];
}

INLINE void istack_push(Context* c, int val) {
    c->istack.data[c->istack.length++] = val;
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

Context* context_create(size_t poolCapacity) {
    Context* c = malloc(sizeof(Context));
    pool_init(&c->cp,-1);
    pool_init(&c->sp,-1);
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

INLINE size_t pool_add(Pool* p, SEXP v) {
    if (p->length >= p->capacity) pool_grow(p);
    SET_VECTOR_ELT(p->data, p->length, v);
    return p->length++;
}

INLINE size_t cp_pool_add(Context* c, SEXP v) {
    pool_add(&(c->cp), v);
}

INLINE size_t src_pool_add(Context* c, SEXP v) {
    pool_add(&(c->src), v);
}

INLINE SEXP cp_pool_at(Context* c, size_t index) {
    return VECTOR_ELT(c->cp.data, index);
}

INLINE size_t src_pool_at(Context* c, SEXP value) {
    return VECTOR_ELT(c->src.data, value);
}

