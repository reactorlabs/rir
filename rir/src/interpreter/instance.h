#ifndef interpreter_context_h
#define interpreter_context_h

#include "R/r.h"
#include "ir/BC_inc.h"
#include "runtime/Assumptions.h"

#include "interp_incl.h"

#include <stdio.h>

#include <assert.h>
#include <functional>
#include <stdint.h>

namespace rir {

/** Compiler API. Given a language object, compiles it and returns the
  EXTERNALSXP containing the Function and its Code objects.

  The idea is to call this if we want on demand compilation of closures.
 */
typedef std::function<SEXP(SEXP expr, SEXP env)> ExprCompiler;
typedef std::function<SEXP(SEXP closure, SEXP name)> ClosureCompiler;
typedef std::function<SEXP(SEXP closure, const rir::Assumptions& assumptions,
                           SEXP name)>
    ClosureOptimizer;

#define POOL_CAPACITY 4096
#define STACK_CAPACITY 4096

/** Resizeable R list.

 Allocates large list and then tricks R into believing that the list is actually
 smaller.

 This works because R uses non-moving GC and is used for constant and source
 pools as well as the interpreter object stack.
 */
typedef struct {
    SEXP list;
    size_t capacity;
} ResizeableList;

#define CONTEXT_INDEX_CP 0
#define CONTEXT_INDEX_SRC 1

/** Interpreter's context.

 Interpreter's context is a list (so that it will be marked by R's gc) that
 contains the SEXP pools and stack as well as other stacks that do not need to
 be gc'd.

 */

struct InterpreterInstance {
    SEXP list;
    ResizeableList cp;
    ResizeableList src;
    ExprCompiler exprCompiler;
    ClosureCompiler closureCompiler;
    ClosureOptimizer closureOptimizer;
};

// TODO we might actually need to do more for the lengths (i.e. true length vs
// length)

RIR_INLINE size_t rl_length(ResizeableList* l) { return Rf_length(l->list); }

RIR_INLINE void rl_setLength(ResizeableList* l, size_t length) {
    ((VECSEXP)l->list)->vecsxp.length = length;
    ((VECSEXP)l->list)->vecsxp.truelength = length;
}

RIR_INLINE void rl_grow(ResizeableList* l, SEXP parent, size_t index) {
    int oldsize = rl_length(l);
    SEXP n = Rf_allocVector(VECSXP, l->capacity * 2);
    memcpy(DATAPTR(n), DATAPTR(l->list), l->capacity * sizeof(SEXP));
    SET_VECTOR_ELT(parent, index, n);
    l->list = n;
    rl_setLength(l, oldsize);
    l->capacity *= 2;
}

RIR_INLINE void rl_append(ResizeableList* l, SEXP val, SEXP parent,
                          size_t index) {
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

#define ostack_top(c) ((R_BCNodeStackTop - 1)->u.sxpval)

#define ostack_at(c, i) ((R_BCNodeStackTop - 1 - (i))->u.sxpval)
#define ostack_at_cell(cell) ((cell)->u.sxpval)

#define ostack_set(c, i, v)                                                    \
    do {                                                                       \
        SEXP __tmp__ = (v);                                                    \
        int idx = (i);                                                         \
        (R_BCNodeStackTop - 1 - idx)->u.sxpval = __tmp__;                      \
        (R_BCNodeStackTop - 1 - idx)->tag = 0;                                 \
    } while (0)

#define ostack_cell_at(c, i) (R_BCNodeStackTop - 1 - (i))

#define ostack_empty(c) (R_BCNodeStackTop == R_BCNodeStackBase)

#define ostack_popn(c, p)                                                      \
    do {                                                                       \
        R_BCNodeStackTop -= (p);                                               \
    } while (0)

#define ostack_pop(c) ((--R_BCNodeStackTop)->u.sxpval)

#define ostack_push(c, v)                                                      \
    do {                                                                       \
        SEXP __tmp__ = (v);                                                    \
        R_BCNodeStackTop->u.sxpval = __tmp__;                                  \
        R_BCNodeStackTop->tag = 0;                                             \
        ++R_BCNodeStackTop;                                                    \
    } while (0)

RIR_INLINE void ostack_ensureSize(InterpreterInstance* c, unsigned minFree) {
    if ((R_BCNodeStackTop + minFree) >= R_BCNodeStackEnd) {
        // TODO....
        assert(false);
    }
}

class Locals final {
    // NOTE: must not own any resources, because the destructor is not called
    //       if there is a longjmp from the evalRirCode call
  private:
    R_bcstack_t* base;
    unsigned localsCount;
    bool existingLocals;

  public:
    explicit Locals(R_bcstack_t* base, unsigned count, bool existingLocals)
        : base(base), localsCount(count), existingLocals(existingLocals) {
        if (!existingLocals)
            R_BCNodeStackTop += localsCount;
    }

    ~Locals() {
        if (!existingLocals)
            R_BCNodeStackTop -= localsCount;
    }

    SEXP load(unsigned offset) {
        SLOWASSERT(offset < localsCount &&
                   "Attempt to load invalid local variable.");
        return (base + offset)->u.sxpval;
    }

    void store(unsigned offset, SEXP val) {
        SLOWASSERT(offset < localsCount &&
                   "Attempt to store invalid local variable.");
        (base + offset)->u.sxpval = val;
        SLOWASSERT((base + offset)->tag == 0);
    }

    Locals(Locals const&) = delete;
    Locals(Locals&&) = delete;
    Locals& operator=(Locals const&) = delete;
    Locals& operator=(Locals&&) = delete;
    static void* operator new(size_t) = delete;
};

InterpreterInstance* context_create();

#define cp_pool_length(c) (rl_length(&(c)->cp))
#define src_pool_length(c) (rl_length(&(c)->src))

RIR_INLINE size_t cp_pool_add(InterpreterInstance* c, SEXP v) {
    size_t result = rl_length(&c->cp);
    rl_append(&c->cp, v, c->list, CONTEXT_INDEX_CP);
    return result;
}

RIR_INLINE size_t src_pool_add(InterpreterInstance* c, SEXP v) {
    size_t result = rl_length(&c->src);
    rl_append(&c->src, v, c->list, CONTEXT_INDEX_SRC);
    return result;
}

RIR_INLINE SEXP cp_pool_at(InterpreterInstance* c, unsigned index) {
    SLOWASSERT(c->cp.capacity > index);
    return VECTOR_ELT(c->cp.list, index);
}

RIR_INLINE SEXP src_pool_at(InterpreterInstance* c, unsigned index) {
    SLOWASSERT(c->src.capacity > index);
    return VECTOR_ELT(c->src.list, index);
}

RIR_INLINE void cp_pool_set(InterpreterInstance* c, unsigned index, SEXP e) {
    SLOWASSERT(c->cp.capacity > index);
    SET_VECTOR_ELT(c->cp.list, index, e);
}

} // namespace rir

#endif // interpreter_context_h
