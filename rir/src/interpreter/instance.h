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

#include <unordered_map>
namespace rir {

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

struct InterpreterInstance {
    SEXP list;
    ResizeableList cp;
    ResizeableList src;
    ExprCompiler exprCompiler;
    ClosureCompiler closureCompiler;
    ClosureOptimizer closureOptimizer;
};

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

#ifdef PROFILE_TYPED_STACK

class TSProfile {
  public:
    std::unordered_map<std::string, size_t> counter;

    void mark(const std::string& event) { counter[event]++; }

    ~TSProfile() {
        std::cout << "Typed stack profile\n";
        for (auto& o : counter) {
            std::cout << o.first << ": " << o.second << "\n";
        }
    }
};

TSProfile TSPROFILE;

#endif

typedef enum {
    STACK_OBJ_SEXP = 0,
    STACK_OBJ_INT = INTSXP,
    STACK_OBJ_REAL = REALSXP,
    STACK_OBJ_LOGICAL = LGLSXP,
    STACK_OBJ_NULL = -1,
} stackObjType;

static const R_bcstack_t nullStackObj = {STACK_OBJ_NULL, {0xBAD}};
typedef union {
    double dval;
    int ival;
} scalar_value_t;

RIR_INLINE R_bcstack_t intStackObj(int x) {
    R_bcstack_t res;
#ifdef USE_TYPED_STACK
    res.tag = STACK_OBJ_INT;
    res.u.ival = x;
#ifdef PROFILE_TYPED_STACK
    TSPROFILE.mark("int stack object created");
#endif
#else
    res.tag = STACK_OBJ_SEXP;
    res.u.sxpval = Rf_allocVector(INTSXP, 1);
    *INTEGER(res.u.sxpval) = x;
#endif
    return res;
}

RIR_INLINE R_bcstack_t realStackObj(double x) {
    R_bcstack_t res;
#ifdef USE_TYPED_STACK
    res.tag = STACK_OBJ_REAL;
    res.u.dval = x;
#ifdef PROFILE_TYPED_STACK
    TSPROFILE.mark("real stack object created");
#endif
#else
    res.tag = STACK_OBJ_SEXP;
    res.u.sxpval = Rf_allocVector(REALSXP, 1);
    *REAL(res.u.sxpval) = x;
#endif
    return res;
}

RIR_INLINE R_bcstack_t logicalStackObj(int x) {
    R_bcstack_t res;
#ifdef USE_TYPED_STACK
    res.tag = STACK_OBJ_LOGICAL;
    res.u.ival = x;
#ifdef PROFILE_TYPED_STACK
    TSPROFILE.mark("logical stack object created");
#endif
#else
    res.tag = STACK_OBJ_SEXP;
    res.u.sxpval = Rf_allocVector(LGLSXP, 1);
    *LOGICAL(res.u.sxpval) = x;
#endif
    return res;
}

RIR_INLINE R_bcstack_t sexpToStackObj(SEXP x) {
    R_bcstack_t res;
    bool tryUnbox = x != loopTrampolineMarker && MAYBE_SHARED(x);
    if (tryUnbox && IS_SCALAR(x, INTSXP)) {
#ifdef PROFILE_TYPED_STACK
        TSPROFILE.mark("int stack object created from sexp");
#endif
        res.tag = STACK_OBJ_INT;
        res.u.ival = *INTEGER(x);
    } else if (tryUnbox && IS_SCALAR(x, REALSXP)) {
#ifdef PROFILE_TYPED_STACK
        TSPROFILE.mark("real stack object created from sexp");
#endif
        res.tag = STACK_OBJ_REAL;
        res.u.dval = *REAL(x);
    } else if (tryUnbox && IS_SCALAR(x, LGLSXP)) {
#ifdef PROFILE_TYPED_STACK
        TSPROFILE.mark("logical stack object created from sexp");
#endif
        res.tag = STACK_OBJ_LOGICAL;
        res.u.ival = *LOGICAL(x);

    } else {
#ifdef PROFILE_TYPED_STACK
        TSPROFILE.mark("sexp stack object created from sexp");
#endif
        res.tag = STACK_OBJ_SEXP;
        res.u.sxpval = x;
    }
    return res;
}

RIR_INLINE SEXP stackObjToSexp(R_bcstack_t x) {
    switch (x.tag) {
#ifdef USE_TYPED_STACK
    case STACK_OBJ_INT:
        SEXP res;
        res = Rf_allocVector(INTSXP, 1);
        *INTEGER(res) = x.u.ival;
#ifdef PROFILE_TYPED_STACK
        TSPROFILE.mark("int stack object boxed");
#endif
#ifdef LOG_SEXP_BOX
        std::cout << "Boxed int " << x.u.ival << "\n";
#endif
        return res;
    case STACK_OBJ_REAL:
        res = Rf_allocVector(REALSXP, 1);
        *REAL(res) = x.u.dval;
#ifdef PROFILE_TYPED_STACK
        TSPROFILE.mark("real stack object boxed");
#endif
#ifdef LOG_SEXP_BOX
        std::cout << "Boxed real " << x.u.dval << "\n";
#endif
        return res;
    case STACK_OBJ_LOGICAL:
        res = Rf_allocVector(LGLSXP, 1);
        *LOGICAL(res) = x.u.ival;
#ifdef PROFILE_TYPED_STACK
        TSPROFILE.mark("logical stack object boxed");
#endif
#ifdef LOG_SEXP_BOX
        std::cout << "Boxed logical " << x.u.ival << "\n";
#endif
        return res;
#endif
    case STACK_OBJ_SEXP:
#ifdef PROFILE_TYPED_STACK
        TSPROFILE.mark("sexp stack object accessed");
#endif
        return x.u.sxpval;
    default:
        assert(false);
    }
}

RIR_INLINE bool stackObjIsNull(R_bcstack_t x) {
    return x.tag == STACK_OBJ_NULL;
}

// Doesn't consider reals integers
RIR_INLINE bool stackObjIsInteger(R_bcstack_t x) {
#ifdef PROFILE_TYPED_STACK
    TSPROFILE.mark("type test");
#endif
    switch (x.tag) {
    case STACK_OBJ_INT:
        return true;
    case STACK_OBJ_REAL:
    case STACK_OBJ_LOGICAL:
        return false;
    case STACK_OBJ_SEXP:
#ifdef PROFILE_TYPED_STACK
        TSPROFILE.mark("type test boxed");
#endif
        return IS_SCALAR(x.u.sxpval, INTSXP);
    default:
        assert(false);
    }
}

// Returns NA_INTEGER if not an integer, doesn't consider reals integers
RIR_INLINE int tryStackObjToInteger(R_bcstack_t x) {
#ifdef PROFILE_TYPED_STACK
    TSPROFILE.mark("stack access");
#endif
    switch (x.tag) {
    case STACK_OBJ_INT:
        return x.u.ival;
    case STACK_OBJ_REAL:
    case STACK_OBJ_LOGICAL:
        return NA_INTEGER;
    case STACK_OBJ_SEXP:
#ifdef PROFILE_TYPED_STACK
        TSPROFILE.mark("stack access boxed");
#endif
        if (IS_SCALAR(x.u.sxpval, INTSXP)) {
            return *INTEGER(x.u.sxpval);
        } else {
            return NA_INTEGER;
        }
    default:
        assert(false);
    }
}

// Doesn't consider integers reals
RIR_INLINE bool stackObjIsReal(R_bcstack_t x) {
#ifdef PROFILE_TYPED_STACK
    TSPROFILE.mark("type test");
#endif
    switch (x.tag) {
    case STACK_OBJ_REAL:
        return true;
    case STACK_OBJ_INT:
    case STACK_OBJ_LOGICAL:
        return false;
    case STACK_OBJ_SEXP:
#ifdef PROFILE_TYPED_STACK
        TSPROFILE.mark("type test boxed");
#endif
        return IS_SCALAR(x.u.sxpval, REALSXP);
    default:
        assert(false);
    }
}

// Returns NA_REAL if not a real, doesn't consider integers reals
RIR_INLINE double tryStackObjToReal(R_bcstack_t x) {
#ifdef PROFILE_TYPED_STACK
    TSPROFILE.mark("stack access");
#endif
    switch (x.tag) {
    case STACK_OBJ_REAL:
        return x.u.dval;
    case STACK_OBJ_INT:
    case STACK_OBJ_LOGICAL:
        return NA_REAL;
    case STACK_OBJ_SEXP:
#ifdef PROFILE_TYPED_STACK
        TSPROFILE.mark("stack access boxed");
#endif
        if (IS_SCALAR(x.u.sxpval, REALSXP)) {
            return *REAL(x.u.sxpval);
        } else {
            return NA_REAL;
        }
    default:
        assert(false);
    }
}

RIR_INLINE bool stackObjIsLogical(R_bcstack_t x) {
#ifdef PROFILE_TYPED_STACK
    TSPROFILE.mark("type test");
#endif
    switch (x.tag) {
    case STACK_OBJ_LOGICAL:
        return true;
    case STACK_OBJ_INT:
    case STACK_OBJ_REAL:
        return false;
    case STACK_OBJ_SEXP:
#ifdef PROFILE_TYPED_STACK
        TSPROFILE.mark("type test boxed");
#endif
        return IS_SCALAR(x.u.sxpval, LGLSXP);
    default:
        assert(false);
    }
}

// Returns NA_LOGICAL if not a logical
RIR_INLINE int tryStackObjToLogical(R_bcstack_t x) {
#ifdef PROFILE_TYPED_STACK
    TSPROFILE.mark("stack access");
#endif
    switch (x.tag) {
    case STACK_OBJ_LOGICAL:
        return x.u.ival;
    case STACK_OBJ_INT:
    case STACK_OBJ_REAL:
        return NA_LOGICAL;
    case STACK_OBJ_SEXP:
#ifdef PROFILE_TYPED_STACK
        TSPROFILE.mark("stack access boxed");
#endif
        if (IS_SCALAR(x.u.sxpval, LGLSXP)) {
            return *LOGICAL(x.u.sxpval);
        } else {
            return NA_LOGICAL;
        }
    default:
        assert(false);
    }
}

// Fails if not a logical or NA
RIR_INLINE int tryStackObjToLogicalNa(R_bcstack_t x) {
#ifdef PROFILE_TYPED_STACK
    TSPROFILE.mark("stack access");
#endif
    switch (x.tag) {
    case STACK_OBJ_LOGICAL:
        return x.u.ival;
    case STACK_OBJ_SEXP:
#ifdef PROFILE_TYPED_STACK
        TSPROFILE.mark("stack access boxed");
#endif
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
RIR_INLINE int tryStackObjToIdx(R_bcstack_t x) {
    if (stackObjIsInteger(x)) {
        return tryStackObjToInteger(x) - 1;
    } else if (stackObjIsReal(x)) {
        return (int)tryStackObjToReal(x) - 1;
    } else {
        return -1;
    }
}

RIR_INLINE SEXPTYPE stackObjSexpType(R_bcstack_t x) {
#ifdef PROFILE_TYPED_STACK
    TSPROFILE.mark("stack access");
#endif
    switch (x.tag) {
    case STACK_OBJ_INT:
        return INTSXP;
    case STACK_OBJ_REAL:
        return REALSXP;
    case STACK_OBJ_LOGICAL:
        return LGLSXP;
    case STACK_OBJ_SEXP:
#ifdef PROFILE_TYPED_STACK
        TSPROFILE.mark("stack access boxed");
#endif
        return TYPEOF(x.u.sxpval);
    default:
        assert(false);
    }
}

RIR_INLINE bool stackObjIsVector(R_bcstack_t x) {
#ifdef PROFILE_TYPED_STACK
    TSPROFILE.mark("stack access");
#endif
    switch (x.tag) {
    case STACK_OBJ_INT:
    case STACK_OBJ_REAL:
    case STACK_OBJ_LOGICAL:
        return true;
    case STACK_OBJ_SEXP:
#ifdef PROFILE_TYPED_STACK
        TSPROFILE.mark("stack access boxed");
#endif
        return Rf_isVector(x.u.sxpval);
    default:
        assert(false);
    }
}

#ifdef USE_TYPED_STACK
RIR_INLINE SEXP stackValueSexp(R_bcstack_t* value) { return value->u.sxpval; }
#else
RIR_INLINE SEXP stackValueSexp(R_bcstack_t* value) { return *value; }
#endif

static R_INLINE int tryStackScalar(R_bcstack_t* s, scalar_value_t* v,
                                   SEXP* pv) {
#ifdef TYPED_STACK
    int tag = s->tag;

    if (tag)
        switch (tag) {
        case REALSXP:
            v->dval = s->u.dval;
            return tag;
        case INTSXP:
            v->ival = s->u.ival;
            return tag;
        case LGLSXP:
            v->ival = s->u.ival;
            return tag;
        }
#endif
    SEXP x = stackValueSexp(s);
    if (IS_SIMPLE_SCALAR(x, REALSXP)) {
#ifndef NO_SAVE_ALLOC
        if (pv && NO_REFERENCES(x))
            *pv = x;
#endif
        v->dval = SCALAR_DVAL(x);
        return REALSXP;
    } else if (IS_SIMPLE_SCALAR(x, INTSXP)) {
#ifndef NO_SAVE_ALLOC
        if (pv && NO_REFERENCES(x))
            *pv = x;
#endif
        v->ival = SCALAR_IVAL(x);
        return INTSXP;
    } else if (IS_SIMPLE_SCALAR(x, LGLSXP)) {
        v->ival = SCALAR_LVAL(x);
        return LGLSXP;
    } else
        return 0;
}

RIR_INLINE bool stackObjIsSimpleScalar(R_bcstack_t x, SEXPTYPE type) {
#ifdef PROFILE_TYPED_STACK
    TSPROFILE.mark("stack access");
#endif
    switch (x.tag) {
    case STACK_OBJ_INT:
        return type == INTSXP;
    case STACK_OBJ_REAL:
        return type == REALSXP;
    case STACK_OBJ_LOGICAL:
        return type == LGLSXP;
    case STACK_OBJ_SEXP:
#ifdef PROFILE_TYPED_STACK
        TSPROFILE.mark("stack access boxed");
#endif
        return IS_SCALAR(x.u.sxpval, type);
    default:
        assert(false);
    }
}

RIR_INLINE bool stackObjIsScalar(R_bcstack_t x) {
#ifdef PROFILE_TYPED_STACK
    TSPROFILE.mark("stack access");
#endif
    switch (x.tag) {
    case STACK_OBJ_INT:
    case STACK_OBJ_REAL:
    case STACK_OBJ_LOGICAL:
        return true;
    case STACK_OBJ_SEXP:
#ifdef PROFILE_TYPED_STACK
        TSPROFILE.mark("stack access boxed");
#endif
        return x.u.sxpval->sxpinfo.scalar;
    default:
        assert(false);
    }
}

RIR_INLINE R_xlen_t stackObjLength(R_bcstack_t x) {
#ifdef PROFILE_TYPED_STACK
    TSPROFILE.mark("stack access");
#endif
    switch (x.tag) {
    case STACK_OBJ_INT:
    case STACK_OBJ_REAL:
    case STACK_OBJ_LOGICAL:
        return 1;
    case STACK_OBJ_SEXP:
#ifdef PROFILE_TYPED_STACK
        TSPROFILE.mark("stack access boxed");
#endif
        return XLENGTH(x.u.sxpval);
    default:
        assert(false);
    }
}

RIR_INLINE int stackObjAsLogical(R_bcstack_t x) {
    if (stackObjIsSimpleScalar(x, REALSXP)) {
        return tryStackObjToReal(x) != 0.0;
    } else if (stackObjIsSimpleScalar(x, INTSXP)) {
        return tryStackObjToInteger(x) != 0;
    } else if (stackObjIsSimpleScalar(x, LGLSXP)) {
        return tryStackObjToLogical(x);
    } else {
        return Rf_asLogical(x.u.sxpval);
    }
}

RIR_INLINE bool stackObjsIdentical(R_bcstack_t x, R_bcstack_t y) {
#ifdef PROFILE_TYPED_STACK
    TSPROFILE.mark("stack access");
#endif
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
#ifdef PROFILE_TYPED_STACK
        TSPROFILE.mark("stack access boxed");
#endif
        return x.u.sxpval == y.u.sxpval;
    default:
        assert(false);
    }
}

RIR_INLINE bool trySetInPlace(SEXP old, R_bcstack_t val) {
    switch (val.tag) {
    case STACK_OBJ_INT:
        if (TYPEOF(old) == INTSXP && NOT_SHARED(old)) {
#ifdef PROFILE_TYPED_STACK
            TSPROFILE.mark("int box reused");
#endif
#ifdef LOG_SEXP_BOX
            std::cout << "Reused int from " << *INTEGER(old) << " to "
                      << val.u.ival << "\n";
#endif
            *INTEGER(old) = val.u.ival;
            return true;
        } else {
            return false;
        }
    case STACK_OBJ_REAL:
        if (TYPEOF(old) == REALSXP && NOT_SHARED(old)) {
#ifdef PROFILE_TYPED_STACK
            TSPROFILE.mark("real box reused");
#endif
#ifdef LOG_SEXP_BOX
            std::cout << "Reused real from " << *REAL(old) << " to "
                      << val.u.dval << "\n";
#endif
            *REAL(old) = val.u.dval;
            return true;
        } else {
            return false;
        }
    case STACK_OBJ_LOGICAL:
        if (TYPEOF(old) == LGLSXP && NOT_SHARED(old)) {
#ifdef PROFILE_TYPED_STACK
            TSPROFILE.mark("logical box reused");
#endif
#ifdef LOG_SEXP_BOX
            std::cout << "Reused logical from " << *LOGICAL(old) << " to "
                      << val.u.ival << "\n";
#endif
            *LOGICAL(old) = val.u.ival;
            return true;
        } else {
            return false;
        }
    case STACK_OBJ_SEXP:
        assert(false);
    default:
        assert(false);
    }
}

#define ostackLength(c) (R_BCNodeStackTop - R_BCNodeStackBase)

#define ostackEmpty(c) (R_BCNodeStackTop == R_BCNodeStackBase)

#define ostackTop(c) *(R_BCNodeStackTop - 1)

#define ostackAt(c, i) *(R_BCNodeStackTop - 1 - (i))

#define ostackCellAt(c, i) (R_BCNodeStackTop - 1 - (i))

#define ostackSet(c, i, v) *(R_BCNodeStackTop - 1 - (i)) = (v)

#define ostackSetInt(c, i, v)                                                  \
    do {                                                                       \
        R_bcstack_t* stk = R_BCNodeStackTop - 1 - (i);                         \
        stk->u.ival = (v);                                                     \
        stk->tag = STACK_OBJ_INT;                                              \
    } while (0)

#define ostackSetReal(c, i, v)                                                 \
    do {                                                                       \
        R_bcstack_t* stk = R_BCNodeStackTop - 1 - (i);                         \
        stk->u.dval = (v);                                                     \
        stk->tag = STACK_OBJ_REAL;                                             \
    } while (0)

#define ostackSetLogical(c, i, v)                                              \
    do {                                                                       \
        R_bcstack_t* stk = R_BCNodeStackTop - 1 - (i);                         \
        stk->u.ival = (v);                                                     \
        stk->tag = STACK_OBJ_LOGICAL;                                          \
    } while (0)

#define ostackSetSexp(c, i, v)                                                 \
    do {                                                                       \
        R_bcstack_t* stk = R_BCNodeStackTop - 1 - (i);                         \
        stk->u.sxpval = (v);                                                   \
        stk->tag = STACK_OBJ_SEXP;                                             \
    } while (0)

#define ostackPopn(c, p)                                                       \
    do {                                                                       \
        R_BCNodeStackTop -= (p);                                               \
    } while (0)

#define ostackPop(c) (*(--R_BCNodeStackTop))

#define ostackPush(c, v)                                                       \
    do {                                                                       \
        *R_BCNodeStackTop = (v);                                               \
        ++R_BCNodeStackTop;                                                    \
    } while (0)

#define ostackPushInt(c, v)                                                    \
    do {                                                                       \
        R_BCNodeStackTop->u.ival = (v);                                        \
        R_BCNodeStackTop->tag = STACK_OBJ_INT;                                 \
        ++R_BCNodeStackTop;                                                    \
    } while (0)

#define ostackPushReal(c, v)                                                   \
    do {                                                                       \
        R_BCNodeStackTop->u.dval = (v);                                        \
        R_BCNodeStackTop->tag = STACK_OBJ_REAL;                                \
        ++R_BCNodeStackTop;                                                    \
    } while (0)

#define ostackPushLogical(c, v)                                                \
    do {                                                                       \
        R_BCNodeStackTop->u.ival = (v);                                        \
        R_BCNodeStackTop->tag = STACK_OBJ_LOGICAL;                             \
        ++R_BCNodeStackTop;                                                    \
    } while (0)

#define ostackPushSexp(c, v)                                                   \
    do {                                                                       \
        R_BCNodeStackTop->u.sxpval = (v);                                      \
        R_BCNodeStackTop->tag = STACK_OBJ_SEXP;                                \
        ++R_BCNodeStackTop;                                                    \
    } while (0)

RIR_INLINE SEXP ostackObjToSexpAt(R_bcstack_t& x, InterpreterInstance* ctx,
                                  unsigned idx) {
    if (x.tag != STACK_OBJ_SEXP) {
        SEXP sexp = stackObjToSexp(x);
        x.u.sxpval = sexp;
        x.tag = STACK_OBJ_SEXP;
        ostackSet(ctx, idx, x);
    }
    return x.u.sxpval;
}

RIR_INLINE SEXP ostackSexpAt(InterpreterInstance* ctx, unsigned idx) {
    R_bcstack_t x = ostackAt(ctx, idx);
    SEXP sexp = ostackObjToSexpAt(x, ctx, idx);
    return sexp;
}

RIR_INLINE SEXP ostackPopSexp(InterpreterInstance* ctx) {
    return stackObjToSexp(ostackPop(ctx));
}

RIR_INLINE void ostackEnsureSize(InterpreterInstance* ctx, unsigned minFree) {
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

    R_bcstack_t load(unsigned offset) {
        SLOWASSERT(offset < localsCount &&
                   "Attempt to load invalid local variable.");
        return *(base + offset);
    }

    void store(unsigned offset, R_bcstack_t val) {
        SLOWASSERT(offset < localsCount &&
                   "Attempt to store invalid local variable.");
        *(base + offset) = val;
    }

    Locals(Locals const&) = delete;
    Locals(Locals&&) = delete;
    Locals& operator=(Locals const&) = delete;
    Locals& operator=(Locals&&) = delete;
    static void* operator new(size_t) = delete;
};

// --- Context

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
