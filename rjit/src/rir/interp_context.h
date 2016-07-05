//
//  interpreter_context.h
//
//
//  Created by Jan Vitek Jr on 6/21/16.
//
//

#ifndef interpreter_context_h
#define interpreter_context_h

#include "interp_data.h"

#include <R.h>
#include <Rinternals.h>
#include <stdio.h>

#include <stdint.h>
#include <assert.h>

#ifdef __cplusplus
extern "C" {
#endif

#define POOL_CAPACITY 4096
#define STACK_CAPACITY 4096


/** Compiler API. Given a language object, compiles it and returns the INTSXP
  containing the Function and its Code objects.

  The idea is to call this if we want on demand compilation of closures.
 */
typedef SEXP (*CompilerCallback)(SEXP, SEXP);

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
// Frame
//
struct Code;
typedef uint8_t OpcodeT;
typedef struct {
    struct Code* code;
    SEXP env;
    OpcodeT* pc;
} Frame;

//
// Frame stack.
//
typedef struct {
    Frame* data;
    size_t length;
    size_t capacity;

} FStack;

//
// Context
//
typedef struct {
    Pool cp;
    Pool src;
    OStack ostack;
    PStack istack;
    FStack fstack;
    CompilerCallback compiler;
} Context;


INLINE int istack_top(Context* c) { return c->istack.data[c->istack.length-1]; }

INLINE SEXP ostack_top(Context* c) { return c->ostack.data[c->ostack.length-1]; }

INLINE int fstack_top(Context* c) {
    return c->fstack.length - 1;
}

INLINE size_t istack_length(Context* c) { return c->ostack.length; }

INLINE bool ostack_empty(Context* c) { return c->ostack.length == 0; }

INLINE bool fstack_empty(Context* c) { return c->fstack.length == 0; }

INLINE SEXP ostack_at(Context* c, unsigned index) {
    return c->ostack.data[index];
}

INLINE SEXP ostack_pop(Context* c) {
    return c->ostack.data[--c->ostack.length];
}

INLINE void ostack_popn(Context* c, unsigned size) { c->ostack.length -= size; }

INLINE void ostack_push(Context* c, SEXP val) {
    c->ostack.data[c->ostack.length++] = val;
}

void ostack_ensureSize(Context* c, unsigned minFree);

INLINE bool istack_empty(Context* c) { return c->istack.length == 0; }

INLINE int istack_pop(Context* c) { return c->istack.data[--c->istack.length]; }

INLINE void istack_push(Context* c, int val) {
    c->istack.data[c->istack.length++] = val;
}

void istack_ensureSize(Context* c, unsigned minFree);

INLINE Frame* fstack_at(Context* c, unsigned index) {
    return &c->fstack.data[index];
}

INLINE void fstack_pop(Context* c, Frame* frame) {
    while(&c->fstack.data[--c->fstack.length] != frame)
        assert(!fstack_empty(c));
}

void fstack_grow(Context* c);

INLINE Frame* fstack_push(Context* c, struct Code* code, SEXP env) {
    if (c->fstack.length == c->fstack.capacity)
        fstack_grow(c);

    assert(*(unsigned*)code == CODE_MAGIC);
    Frame* frame = &c->fstack.data[c->fstack.length];
    frame->code = code;
    frame->env = env;
    c->fstack.length++;
    return frame;
}


Context* context_create(CompilerCallback compiler);

void pool_init(Pool* p, size_t capacity);

void pool_grow(Pool* p);

INLINE size_t pool_add(Pool* p, SEXP v) {
    if (p->length >= p->capacity)
        pool_grow(p);
    SET_VECTOR_ELT(p->data, p->length, v);
    return p->length++;
}

INLINE size_t cp_pool_add(Context* c, SEXP v) {
    return pool_add(&(c->cp), v);
}

INLINE size_t src_pool_add(Context* c, SEXP v) {
    return pool_add(&(c->src), v);
}

INLINE SEXP cp_pool_at(Context* c, size_t index) {
    return VECTOR_ELT(c->cp.data, index);
}

INLINE SEXP src_pool_at(Context* c, size_t value) {
    return VECTOR_ELT(c->src.data, value);
}

/** Initializes the interpreter.
 */
void interp_initialize(CompilerCallback compiler);

/** TODO Makes sure the gc undersands our stacks and pools. */
void rir_interp_gc_callback(void (*forward_node)(SEXP));

/** Returns the global context for the interpreter - important to get access to
  the shared constant and source pools.

  TODO Even in multithreaded mode we probably want to have cp and src pools
  shared - it is not that we add stuff to them often.
 */
Context* globalContext();

#ifdef __cplusplus
}
#endif

#endif // interpreter_context_h
