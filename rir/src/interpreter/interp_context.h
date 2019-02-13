#ifndef interpreter_context_h
#define interpreter_context_h

#include "R/r.h"
#include "interp_data.h"
#include "ir/BC_inc.h"
#include "runtime/Assumptions.h"

#include <stdio.h>

#include <assert.h>
#include <functional>
#include <stdint.h>

// --- Misc Declarations

const static SEXP loopTrampolineMarker = (SEXP)0x7007;

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

struct Context {
    SEXP list;
    ResizeableList cp;
    ResizeableList src;
    ExprCompiler exprCompiler;
    ClosureCompiler closureCompiler;
    ClosureOptimizer closureOptimizer;
};

// Some symbols
extern SEXP R_Subset2Sym;
extern SEXP R_SubsetSym;
extern SEXP R_SubassignSym;
extern SEXP R_Subassign2Sym;
extern SEXP R_valueSym;
extern SEXP setterPlaceholderSym;
extern SEXP getterPlaceholderSym;
extern SEXP quoteSym;

// --- Resizable List

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

// --- Stack

// Determines whether simple scalars are represented unboxed
// Otherwise they'll be STACK_OBJ_SEXPs like other expressions
#define USE_TYPED_STACK

typedef enum {
    STACK_OBJ_SEXP,
    STACK_OBJ_INT,
    STACK_OBJ_REAL,
    STACK_OBJ_LOGICAL
} stack_obj_type;

RIR_INLINE R_bcstack_t int_stack_obj(int x) {
    R_bcstack_t res;
#ifdef USE_TYPED_STACK
    res.tag = STACK_OBJ_INT;
    res.u.ival = x;
#else
    res.tag = STACK_OBJ_SEXP;
    res.u.sxpval = Rf_allocVector(INTSXP, 1);
    R_PreserveObject(res.u.sxpval);
    *INTEGER(res.u.sxpval) = x;
#endif
    return res;
}

RIR_INLINE R_bcstack_t real_stack_obj(double x) {
    R_bcstack_t res;
#ifdef USE_TYPED_STACK
    res.tag = STACK_OBJ_REAL;
    res.u.dval = x;
#else
    res.tag = STACK_OBJ_SEXP;
    res.u.sxpval = Rf_allocVector(REALSXP, 1);
    R_PreserveObject(res.u.sxpval);
    *REAL(res.u.sxpval) = x;
#endif
    return res;
}

RIR_INLINE R_bcstack_t logical_stack_obj(int x) {
    R_bcstack_t res;
#ifdef USE_TYPED_STACK
    res.tag = STACK_OBJ_LOGICAL;
    res.u.ival = x;
#else
    res.tag = STACK_OBJ_SEXP;
    res.u.sxpval = Rf_allocVector(LGLSXP, 1);
    R_PreserveObject(res.u.sxpval);
    *LOGICAL(res.u.sxpval) = x;
#endif
    return res;
}

RIR_INLINE R_bcstack_t sexp_to_stack_obj(SEXP x, bool unprotect) {
    assert(x != NULL);
#ifdef USE_TYPED_STACK
    if (x == loopTrampolineMarker || ATTRIB(x) != R_NilValue) {
        R_bcstack_t res;
        res.tag = STACK_OBJ_SEXP;
        res.u.sxpval = x;
        return res;
    } else if (IS_SIMPLE_SCALAR(x, INTSXP)) {
        return int_stack_obj(*INTEGER(x));
    } else if (IS_SIMPLE_SCALAR(x, REALSXP)) {
        return real_stack_obj(*REAL(x));
    } else if (IS_SIMPLE_SCALAR(x, LGLSXP)) {
        return logical_stack_obj(*INTEGER(x));
    } else {
        R_bcstack_t res;
        res.tag = STACK_OBJ_SEXP;
        res.u.sxpval = x;
        return res;
    }
#else
    R_bcstack_t res;
    res.tag = STACK_OBJ_SEXP;
    res.u.sxpval = x;
    return res;
#endif
}

RIR_INLINE SEXP stack_obj_to_sexp(R_bcstack_t x) {
    switch (x.tag) {
    case STACK_OBJ_INT:
#ifdef USE_TYPED_STACK
        SEXP res;
        res = Rf_allocVector(INTSXP, 1);
        R_PreserveObject(res);
        *INTEGER(res) = x.u.ival;
        return res;
    case STACK_OBJ_REAL:
        res = Rf_allocVector(REALSXP, 1);
        R_PreserveObject(res);
        *REAL(res) = x.u.dval;
        return res;
    case STACK_OBJ_LOGICAL:
        res = Rf_allocVector(LGLSXP, 1);
        R_PreserveObject(res);
        *LOGICAL(res) = x.u.ival;
        return res;
#endif
    case STACK_OBJ_SEXP:
        return x.u.sxpval;
    default:
        assert(false);
    }
}

// Doesn't consider reals integers
RIR_INLINE bool stack_obj_is_integer(R_bcstack_t x) {
    switch (x.tag) {
    case STACK_OBJ_INT:
        return true;
    case STACK_OBJ_REAL:
    case STACK_OBJ_LOGICAL:
        return false;
    case STACK_OBJ_SEXP:
        return IS_SIMPLE_SCALAR(x.u.sxpval, INTSXP);
    default:
        assert(false);
    }
}

// Returns NA_INTEGER if not an integer, doesn't consider reals integers
RIR_INLINE int try_stack_obj_to_integer(R_bcstack_t x) {
    switch (x.tag) {
    case STACK_OBJ_INT:
        return x.u.ival;
    case STACK_OBJ_REAL:
    case STACK_OBJ_LOGICAL:
        return NA_INTEGER;
    case STACK_OBJ_SEXP:
        if (IS_SIMPLE_SCALAR(x.u.sxpval, INTSXP)) {
            return *INTEGER(x.u.sxpval);
        } else {
            return NA_INTEGER;
        }
    default:
        assert(false);
    }
}

// Doesn't consider integers reals
RIR_INLINE bool stack_obj_is_real(R_bcstack_t x) {
    switch (x.tag) {
    case STACK_OBJ_REAL:
        return true;
    case STACK_OBJ_INT:
    case STACK_OBJ_LOGICAL:
        return false;
    case STACK_OBJ_SEXP:
        return IS_SIMPLE_SCALAR(x.u.sxpval, REALSXP);
    default:
        assert(false);
    }
}

// Returns NA_REAL if not a real, doesn't consider integers reals
RIR_INLINE double try_stack_obj_to_real(R_bcstack_t x) {
    switch (x.tag) {
    case STACK_OBJ_REAL:
        return x.u.dval;
    case STACK_OBJ_INT:
    case STACK_OBJ_LOGICAL:
        return NA_REAL;
    case STACK_OBJ_SEXP:
        if (IS_SIMPLE_SCALAR(x.u.sxpval, REALSXP)) {
            return *REAL(x.u.sxpval);
        } else {
            return NA_REAL;
        }
    default:
        assert(false);
    }
}

RIR_INLINE bool stack_obj_is_logical(R_bcstack_t x) {
    switch (x.tag) {
    case STACK_OBJ_LOGICAL:
        return true;
    case STACK_OBJ_INT:
    case STACK_OBJ_REAL:
        return false;
    case STACK_OBJ_SEXP:
        return IS_SIMPLE_SCALAR(x.u.sxpval, LGLSXP);
    default:
        assert(false);
    }
}

// Returns NA_LOGICAL if not a logical
RIR_INLINE int try_stack_obj_to_logical(R_bcstack_t x) {
    switch (x.tag) {
    case STACK_OBJ_LOGICAL:
        return x.u.ival;
    case STACK_OBJ_INT:
    case STACK_OBJ_REAL:
        return NA_LOGICAL;
    case STACK_OBJ_SEXP:
        if (IS_SIMPLE_SCALAR(x.u.sxpval, LGLSXP)) {
            return *LOGICAL(x.u.sxpval);
        } else {
            return NA_LOGICAL;
        }
    default:
        assert(false);
    }
}

// Fails if not a logical or NA
RIR_INLINE int try_stack_obj_to_logical_na(R_bcstack_t x) {
    switch (x.tag) {
    case STACK_OBJ_LOGICAL:
        return x.u.ival;
    case STACK_OBJ_SEXP:
        if (TYPEOF(x.u.sxpval) == LGLSXP) {
            return XLENGTH(x.u.sxpval) == 0 ? NA_LOGICAL : *LOGICAL(x.u.sxpval);
        } else {
            assert(false);
        }
    default:
        assert(false);
    }
}

// Returns regular if int, truncated if real, -1 otherwise
RIR_INLINE int try_stack_obj_to_idx(R_bcstack_t x) {
    if (stack_obj_is_integer(x)) {
        return try_stack_obj_to_integer(x) - 1;
    } else if (stack_obj_is_real(x)) {
        return (int)try_stack_obj_to_real(x) - 1;
    } else {
        return -1;
    }
}

RIR_INLINE SEXPTYPE stack_obj_sexp_type(R_bcstack_t x) {
    switch (x.tag) {
    case STACK_OBJ_INT:
        return INTSXP;
    case STACK_OBJ_REAL:
        return REALSXP;
    case STACK_OBJ_LOGICAL:
        return LGLSXP;
    case STACK_OBJ_SEXP:
        return TYPEOF(x.u.sxpval);
    default:
        assert(false);
    }
}

RIR_INLINE bool stack_obj_is_vector(R_bcstack_t x) {
    switch (x.tag) {
    case STACK_OBJ_INT:
    case STACK_OBJ_REAL:
    case STACK_OBJ_LOGICAL:
        return true;
    case STACK_OBJ_SEXP:
        return Rf_isVector(x.u.sxpval);
    default:
        assert(false);
    }
}

RIR_INLINE R_xlen_t stack_obj_length(R_bcstack_t x) {
    switch (x.tag) {
    case STACK_OBJ_INT:
    case STACK_OBJ_REAL:
    case STACK_OBJ_LOGICAL:
        return 1;
    case STACK_OBJ_SEXP:
        return XLENGTH(x.u.sxpval);
    default:
        assert(false);
    }
}

RIR_INLINE bool stack_objs_equal(R_bcstack_t x, R_bcstack_t y) {
    if (x.tag != y.tag) {
        return false;
    }

    switch (x.tag) { // == y.tag
    case STACK_OBJ_INT:
    case STACK_OBJ_LOGICAL:
        return x.u.ival == y.u.ival;
    case STACK_OBJ_REAL:
        return x.u.dval == y.u.dval;
    case STACK_OBJ_SEXP:
        return x.u.sxpval == y.u.sxpval;
    default:
        assert(false);
    }
}

#define ostack_length(c) (R_BCNodeStackTop - R_BCNodeStackBase)

#ifdef TYPED_STACK
#define ostack_top(c) *(R_BCNodeStackTop - 1)
#endif

#ifdef TYPED_STACK
#define ostack_at(c, i) *(R_BCNodeStackTop - 1 - (i))
#endif

#ifdef TYPED_STACK
#define ostack_set(c, i, v)                                                    \
    do {                                                                       \
        int idx = (i);                                                         \
        *(R_BCNodeStackTop - 1 - idx) = (v);                                   \
    } while (0)
#endif

#define ostack_cell_at(c, i) (R_BCNodeStackTop - 1 - (i))

#define ostack_empty(c) (R_BCNodeStackTop == R_BCNodeStackBase)

#define ostack_popn(c, p)                                                      \
    do {                                                                       \
        R_BCNodeStackTop -= (p);                                               \
    } while (0)

#ifdef TYPED_STACK
#define ostack_pop(c) (*(--R_BCNodeStackTop))
#endif

#ifdef TYPED_STACK
#define ostack_push(c, v)                                                      \
    do {                                                                       \
        *R_BCNodeStackTop = (v);                                               \
        ++R_BCNodeStackTop;                                                    \
    } while (0)
#endif

RIR_INLINE void ostack_ensureSize(Context* c, unsigned minFree) {
    if ((R_BCNodeStackTop + minFree) >= R_BCNodeStackEnd) {
        // TODO....
        assert(false);
    }
}

// --- Locals

class Locals final {
    // NOTE: must not own any resources, because the destructor is not called
    //       if there is a longjmp from the evalRirCode call
  private:
    R_bcstack_t* base;
    unsigned localsCount;

  public:
    explicit Locals(R_bcstack_t* base, unsigned count)
        : base(base), localsCount(count) {
        R_BCNodeStackTop += localsCount;
    }

    ~Locals() { R_BCNodeStackTop -= localsCount; }

    R_bcstack_t load(unsigned offset) {
        SLOWASSERT(offset < localsCount &&
                   "Attempt to load invalid local variable.");
#ifdef TYPED_STACK
        return *(base + offset);
#endif
    }

    void store(unsigned offset, R_bcstack_t val) {
        SLOWASSERT(offset < localsCount &&
                   "Attempt to store invalid local variable.");
#ifdef TYPED_STACK
        *(base + offset) = val;
#endif
    }

    Locals(Locals const&) = delete;
    Locals(Locals&&) = delete;
    Locals& operator=(Locals const&) = delete;
    Locals& operator=(Locals&&) = delete;
    static void* operator new(size_t) = delete;
};

// --- Context

Context* context_create();

#define cp_pool_length(c) (rl_length(&(c)->cp))
#define src_pool_length(c) (rl_length(&(c)->src))

RIR_INLINE size_t cp_pool_add(Context* c, SEXP v) {
    size_t result = rl_length(&c->cp);
    rl_append(&c->cp, v, c->list, CONTEXT_INDEX_CP);
    return result;
}

RIR_INLINE size_t src_pool_add(Context* c, SEXP v) {
    size_t result = rl_length(&c->src);
    rl_append(&c->src, v, c->list, CONTEXT_INDEX_SRC);
    return result;
}

RIR_INLINE SEXP cp_pool_at(Context* c, unsigned index) {
    SLOWASSERT(c->cp.capacity > index);
    return VECTOR_ELT(c->cp.list, index);
}

RIR_INLINE SEXP src_pool_at(Context* c, unsigned index) {
    SLOWASSERT(c->src.capacity > index);
    return VECTOR_ELT(c->src.list, index);
}

RIR_INLINE void cp_pool_set(Context* c, unsigned index, SEXP e) {
    SLOWASSERT(c->cp.capacity > index);
    SET_VECTOR_ELT(c->cp.list, index, e);
}

#endif // interpreter_context_h
