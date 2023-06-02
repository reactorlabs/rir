#ifndef INTERPRETER_CONTEXT_H
#define INTERPRETER_CONTEXT_H

#include "R/r.h"
#include "bc/BC_inc.h"
#include "interp_incl.h"
#include "runtime/Context.h"

#include <assert.h>
#include <functional>
#include <stdint.h>
#include <stdio.h>

namespace rir {

/** Compiler API. Given a language object, compiles it and returns the
  EXTERNALSXP containing the Function and its Code objects.

  The idea is to call this if we want on demand compilation of closures.
 */
typedef std::function<SEXP(SEXP closure, SEXP name)> ClosureCompiler;
typedef std::function<SEXP(SEXP closure, const rir::Context& assumptions,
                           SEXP name)>
    ClosureOptimizer;

/** Resizeable R list.

 Allocates large list and then tricks R into believing that the list is actually
 smaller.

 This works because R uses non-moving GC and is used for constant and source
 pools as well as the interpreter object stack.
 */
struct ResizeableList {
    SEXP list;
    size_t capacity;
    static const size_t CONTEXT_INDEX_CP = 0;
    static const size_t CONTEXT_INDEX_SRC = 1;
    static const size_t POOL_CAPACITY = 4096;
};

/** Interpreter's context.

 Interpreter's context is a list (so that it will be marked by R's gc) that
 contains the SEXP pools and stack as well as other stacks that do not need to
 be gc'd.

 */

struct InterpreterInstance {
    SEXP list;
    ResizeableList cp;
    ResizeableList src;
    ClosureCompiler closureCompiler;
    ClosureOptimizer closureOptimizer;
};

// TODO we might actually need to do more for the lengths (i.e. true length vs
// length)

inline size_t rl_length(ResizeableList* l) { return Rf_length(l->list); }

inline void rl_setLength(ResizeableList* l, size_t length) {
    ((VECSEXP)l->list)->vecsxp.length = length;
    ((VECSEXP)l->list)->vecsxp.truelength = length;
}

inline void rl_grow(ResizeableList* l, SEXP parent, size_t index) {
    int oldsize = rl_length(l);
    SEXP n = Rf_allocVector(VECSXP, l->capacity * 2);
    memcpy(DATAPTR(n), DATAPTR(l->list), l->capacity * sizeof(SEXP));
    SET_VECTOR_ELT(parent, index, n);
    l->list = n;
    rl_setLength(l, oldsize);
    l->capacity *= 2;
}

inline void rl_append(ResizeableList* l, SEXP val, SEXP parent, size_t index) {
    size_t i = rl_length(l);
    if (i == l->capacity) {
        PROTECT(val);
        rl_grow(l, parent, index);
        UNPROTECT(1);
    }
    rl_setLength(l, i + 1);
    SET_VECTOR_ELT(l->list, i, val);
}

inline size_t ostack_length() {
    assert(R_BCNodeStackTop >= R_BCNodeStackBase);
    return R_BCNodeStackTop - R_BCNodeStackBase;
}

inline R_bcstack_t* ostack_cell_at(int i) { return R_BCNodeStackTop - 1 - i; }

inline SEXP ostack_at(int i) { return ostack_cell_at(i)->u.sxpval; }

inline SEXP ostack_top() { return ostack_at(0); }

inline SEXP ostack_at_cell(const R_bcstack_t* cell) { return cell->u.sxpval; }

inline void ostack_set_cell(R_bcstack_t* cell, SEXP v) {
    cell->u.sxpval = v;
    cell->tag = 0;
}

inline void ostack_set(int i, SEXP v) { ostack_set_cell(ostack_cell_at(i), v); }

inline void ostack_popn(size_t n) { R_BCNodeStackTop -= n; }

inline SEXP ostack_pop() { return (--R_BCNodeStackTop)->u.sxpval; }

inline void ostack_push(SEXP v) {
    ostack_set_cell(R_BCNodeStackTop, v);
    ++R_BCNodeStackTop;
}

inline void ostack_ensureSize(unsigned minFree) {
    if ((R_BCNodeStackTop + minFree) >= R_BCNodeStackEnd) {
        // TODO....
        assert(false);
    }
}

void context_init();

inline size_t cp_pool_length() { return rl_length(&globalContext()->cp); }

inline size_t cp_pool_add(SEXP v) {
    InterpreterInstance* c = globalContext();
    size_t result = rl_length(&c->cp);
    rl_append(&c->cp, v, c->list, ResizeableList::CONTEXT_INDEX_CP);
    return result;
}

inline SEXP cp_pool_at(unsigned index) {
    InterpreterInstance* c = globalContext();
    SLOWASSERT(c->cp.capacity > index);
    return VECTOR_ELT(c->cp.list, index);
}

inline void cp_pool_set(unsigned index, SEXP e) {
    InterpreterInstance* c = globalContext();
    SLOWASSERT(c->cp.capacity > index);
    SET_VECTOR_ELT(c->cp.list, index, e);
}

inline size_t src_pool_length() { return rl_length(&globalContext()->src); }

inline size_t src_pool_add(SEXP v) {
    InterpreterInstance* c = globalContext();
    size_t result = rl_length(&c->src);
    rl_append(&c->src, v, c->list, ResizeableList::CONTEXT_INDEX_SRC);
    return result;
}

inline SEXP src_pool_at(unsigned index) {
    InterpreterInstance* c = globalContext();
    SLOWASSERT(c->src.capacity > index);
    return VECTOR_ELT(c->src.list, index);
}

size_t src_pool_read_item(SEXP ref_table, R_inpstream_t in);
void src_pool_write_item(size_t idx, SEXP ref_table, R_outpstream_t out);

} // namespace rir

#endif // INTERPRETER_CONTEXT_H
