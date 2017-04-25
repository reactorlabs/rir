
#ifndef interpreter_context_h
#define interpreter_context_h

#include "interp_data.h"

#include <stdio.h>

#include <stdint.h>
#include <assert.h>

/** Compiler API. Given a language object, compiles it and returns the
  EXTERNALSXP containing the Function and its Code objects.

  The idea is to call this if we want on demand compilation of closures.
 */
typedef SEXP (*CompilerCallback)(SEXP, SEXP);
typedef SEXP (*OptimizerCallback)(SEXP);

#ifdef __cplusplus
extern "C" {
#endif

#define POOL_CAPACITY 4096
#define STACK_CAPACITY 4096

/** Resizeable R list.

 Allocates large list and then tricks R into believing that the list is actually smaller.

 This works because R uses non-moving GC and is used for constant and source pools as well as the interpreter object stack.
 */
typedef struct {
    SEXP list;
    size_t capacity;
} ResizeableList;

/** Interpreter frame information.
 */
typedef struct {
    struct Code* code;
    SEXP env;
    OpcodeT* pc;
    size_t bp;
} Frame;

#define CONTEXT_INDEX_CP 0
#define CONTEXT_INDEX_SRC 1

/** Interpreter's context.

 Interpreter's context is a list (so that it will be marked by R's gc) that contains the SEXP pools and stack as well as other stacks that do not need to be gc'd.

 */

typedef struct {
    SEXP list;
    ResizeableList cp;
    ResizeableList src;
    CompilerCallback compiler;
    OptimizerCallback optimizer;
} Context;

// Some symbols
extern SEXP R_Subset2Sym;
extern SEXP R_SubsetSym;
extern SEXP R_SubassignSym;
extern SEXP R_Subassign2Sym;
extern SEXP R_valueSym;
extern SEXP setterPlaceholderSym;
extern SEXP getterPlaceholderSym;
extern SEXP quoteSym;

// TODO we might actually need to do more for the lengths (i.e. true length vs length)

INLINE size_t rl_length(ResizeableList * l) {
    return Rf_length(l->list);
}

INLINE void rl_setLength(ResizeableList * l, size_t length) {
    ((VECSEXP)l->list)->vecsxp.length = length;
    ((VECSEXP)l->list)->vecsxp.truelength = length;
}

INLINE void rl_grow(ResizeableList * l, SEXP parent, size_t index) {
    int oldsize = rl_length(l);
    SEXP n = Rf_allocVector(VECSXP, l->capacity * 2);
    memcpy(DATAPTR(n), DATAPTR(l->list), l->capacity * sizeof(SEXP));
    SET_VECTOR_ELT(parent, index, n);
    l->list = n;
    rl_setLength(l, oldsize);
    l->capacity *= 2;
}

INLINE void rl_append(ResizeableList * l, SEXP val, SEXP parent, size_t index) {
    size_t i = rl_length(l);
    if (i == l->capacity) {
        PROTECT(val);
        rl_grow(l, parent, index);
        UNPROTECT(1);
    }
    rl_setLength(l, i + 1);
    SET_VECTOR_ELT(l->list, i, val);
}

#define ostack_length(c) (R_BCNodeStackTop - R_BCNodeStackBase)

#ifdef TYPED_STACK
#  define ostack_top(c) ((R_BCNodeStackTop - 1)->u.sxpval)
#else
#  define ostack_top(c) (*(R_BCNodeStackTop - 1))
#endif

#ifdef TYPED_STACK
#  define ostack_at(c, i) ((R_BCNodeStackTop - 1 - (i))->u.sxpval)
#else
#  define ostack_at(c, i) (*(R_BCNodeStackTop - 1 - (i)))
#endif

#ifdef TYPED_STACK
#  define ostack_set(c, i, v) do { \
        SEXP tmp = (v); \
        int idx = (i); \
        (R_BCNodeStackTop - 1 - idx)->u.sxpval = tmp; \
        (R_BCNodeStackTop - 1 - idx)->tag = 0; \
    } while (0)
#else
#  define ostack_set(c, i, v) do { \
        SEXP tmp = (v); \
        int idx = (i); \
        *(R_BCNodeStackTop - 1 - idx) = tmp; \
    } while (0)
#endif

#define ostack_cell_at(c, i) (R_BCNodeStackTop - 1 - (i))

#define ostack_empty(c) (R_BCNodeStackTop == R_BCNodeStackBase)

#define ostack_popn(c, p) do { R_BCNodeStackTop -= (p); } while (0)

#ifdef TYPED_STACK
#  define ostack_pop(c) ((--R_BCNodeStackTop)->u.sxpval)
#else
#  define ostack_pop(c) (*(--R_BCNodeStackTop))
#endif

#ifdef TYPED_STACK
#  define ostack_push(c, v) do { \
        SEXP tmp = (v); \
        R_BCNodeStackTop->u.sxpval = tmp; \
        R_BCNodeStackTop->tag = 0; \
        ++R_BCNodeStackTop; \
    } while (0)
#else
#  define ostack_push(c, v) do { \
        SEXP tmp = (v); \
        *R_BCNodeStackTop = tmp; \
        ++R_BCNodeStackTop; \
    } while (0)
#endif

INLINE void ostack_ensureSize(Context* c, unsigned minFree) {
    if ((R_BCNodeStackTop + minFree) >= R_BCNodeStackEnd) {
        // TODO....
        assert(false);
    }
}

Context* context_create(CompilerCallback, OptimizerCallback);

#define cp_pool_length(c) (rl_length(& (c)->cp))
#define src_pool_length(c) (rl_length(& (c)->src))

INLINE size_t cp_pool_add(Context* c, SEXP v) {
    size_t result = rl_length(& c->cp);
    rl_append(& c->cp, v, c->list, CONTEXT_INDEX_CP);
    return result;
}


INLINE size_t src_pool_add(Context* c, SEXP v) {
    size_t result = rl_length( &c->src);
    rl_append(& c->src, v, c->list, CONTEXT_INDEX_SRC);
    return result;
}

#define cp_pool_at(c, index) (VECTOR_ELT((c)->cp.list, (index)))
#define src_pool_at(c, value) (VECTOR_ELT((c)->src.list, (value)))

#ifdef __cplusplus
}
#endif

#endif // interpreter_context_h
