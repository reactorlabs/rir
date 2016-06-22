//
//  interpreter_context.h
//  
//
//  Created by Jan Vitek Jr on 6/21/16.
//
//

#ifndef interpreter_context_h
#define interpreter_context_h

#include <R.h>
#include <Rinternals.h>
#include <stdio.h>

// TODO force inlinine for clang & gcc
#define INLINE __attribute__((always_inline)) inline


#ifdef __cplusplus
extern "C" {
#else
#define bool int
#define true 1
#define false 0
#endif


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
typedef struct {
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

void ostack_ensureSize(Context* c, unsigned minFree);

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

void istack_ensureSize(Context* c, unsigned minFree);

Context* context_create(size_t poolCapacity);

void pool_init(Pool* p, size_t capacity);

void pool_grow(Pool* p);

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

INLINE SEXP cp_pool_at(Context* c, size_t index) {
    return VECTOR_ELT(c->cp.data, index);
}

INLINE SEXP src_pool_at(Context* c, size_t value) {
    return VECTOR_ELT(c->src.data, value);
}

#ifdef __cplusplus
}
#endif


#endif // interpreter_context_h
