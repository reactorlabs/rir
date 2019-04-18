#ifndef interpreter_context_h
#define interpreter_context_h

#include "R/r.h"
#include "ir/BC_inc.h"
#include "runtime/Assumptions.h"

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

InterpreterInstance* globalContext();

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

#define INTEGER_TO_REAL(x) ((x) == NA_INTEGER ? NA_REAL : (x))
#define INTEGER_TO_LOGICAL(x)                                                  \
    ((x) == NA_INTEGER ? NA_LOGICAL : (x) ? TRUE : FALSE)
#define LOGICAL_TO_REAL(x) ((x) == NA_LOGICAL ? NA_REAL : (x))

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

static R_INLINE bool FIND_ON_STACK(SEXP x, R_bcstack_t* base, int skip) {
    /* Check whether the value is on the stack before modifying.  If
       'skip' is true the top value on the stack is ignored. LT */
    R_bcstack_t* checktop = skip ? R_BCNodeStackTop - 1 : R_BCNodeStackTop;
    for (R_bcstack_t* p = base; p < checktop; p++) {
        if (p->tag == RAWMEM_TAG)
            p += p->u.ival;
        else if (p->u.sxpval == x && p->tag == 0)
            return true;
    }
    return false;
}

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
    res.u.sxpval = intAsSexp(x);
#endif
    return res;
}

RIR_INLINE SEXP intAsSexp(int x) {
    SEXP res = Rf_allocVector(INTSXP, 1);
    *INTEGER(res) = x;
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
    res.u.sxpval = realAsSexp(x);
#endif
    return res;
}

RIR_INLINE SEXP realAsSexp(double x) {
    SEXP res = Rf_allocVector(REALSXP, 1);
    *REAL(res) = x;
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
    res.u.sxpval = logicalAsSexp(x);
#endif
    return res;
}

RIR_INLINE SEXP logicalAsSexp(int x) {
    SEXP res = Rf_allocVector(LGLSXP, 1);
    *LOGICAL(res) = x;
    return res;
}

// Para borrar?
RIR_INLINE R_bcstack_t sexpStackObj(SEXP value) {
    R_bcstack_t res;
    res.tag = STACK_OBJ_SEXP;
    res.u.sxpval = value;
    return res;
}

// Revisar
RIR_INLINE R_bcstack_t sexpStackObjTryUnbox(SEXP x) {
    R_bcstack_t res;
    bool tryUnbox = x != loopTrampolineMarker && MAYBE_SHARED(x);
    if (tryUnbox && IS_SIMPLE_SCALAR(x, INTSXP)) {
#ifdef PROFILE_TYPED_STACK
        TSPROFILE.mark("int stack object created from sexp");
#endif
        res.tag = STACK_OBJ_INT;
        res.u.ival = *INTEGER(x);
    } else if (tryUnbox && IS_SIMPLE_SCALAR(x, REALSXP)) {
#ifdef PROFILE_TYPED_STACK
        TSPROFILE.mark("real stack object created from sexp");
#endif
        res.tag = STACK_OBJ_REAL;
        res.u.dval = *REAL(x);
    } else if (tryUnbox && IS_SIMPLE_SCALAR(x, LGLSXP)) {
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

RIR_INLINE SEXP asBoxed(R_bcstack_t* stackCell) {
    switch (stackCell->tag) {
    case STACK_OBJ_INT:
        return ScalarInteger(stackCell->u.ival);
#ifdef PROFILE_TYPED_STACK
        TSPROFILE.mark("int stack object boxed");
#endif
#ifdef LOG_SEXP_BOX
        std::cout << "Boxed int " << stackCell->u.ival << "\n";
#endif
    case STACK_OBJ_REAL:
        return ScalarReal(stackCell->u.dval);
#ifdef PROFILE_TYPED_STACK
        TSPROFILE.mark("real stack object boxed");
#endif
#ifdef LOG_SEXP_BOX
        std::cout << "Boxed real " << stackCell->u.dval << "\n";
#endif
    case STACK_OBJ_LOGICAL:
        return ScalarLogical(stackCell->u.ival);
#ifdef PROFILE_TYPED_STACK
        TSPROFILE.mark("logical stack object boxed");
#endif
#ifdef LOG_SEXP_BOX
        std::cout << "Boxed logical " << stackCell->u.ival << "\n";
#endif
    default:
        assert(false);
        return nullptr;
    }
}

RIR_INLINE SEXP stackObjToSexp(R_bcstack_t* stackCell) {
#ifdef USE_TYPED_STACK
    if (stackCell->tag != STACK_OBJ_SEXP) {
        stackCell->u.sxpval = asBoxed(stackCell);
        stackCell->tag = STACK_OBJ_SEXP;
    }
#endif
    return stackCell->u.sxpval;
}

RIR_INLINE SEXP stackObjAsSexp(R_bcstack_t* stackCell) {
#ifdef USE_TYPED_STACK
    if (stackCell->tag != STACK_OBJ_SEXP) {
        return asBoxed(stackCell);
    }
#endif
    return stackCell->u.sxpval;
}

RIR_INLINE SEXP stackSexp(R_bcstack_t* stackCell) {
    return stackCell->u.sxpval;
}

RIR_INLINE bool stackObjIsNull(R_bcstack_t x) {
    return x.tag == STACK_OBJ_NULL;
}

// Returns NA_INTEGER if not an integer, doesn't consider reals integers
RIR_INLINE int stackObjAsInteger(R_bcstack_t* stackCell) {
#ifdef PROFILE_TYPED_STACK
    TSPROFILE.mark("stack access");
#endif
    switch (stackCell->tag) {
    case STACK_OBJ_INT:
        return stackCell->u.ival;
    case STACK_OBJ_REAL:
    case STACK_OBJ_LOGICAL:
        return NA_INTEGER;
    case STACK_OBJ_SEXP:
#ifdef PROFILE_TYPED_STACK
        TSPROFILE.mark("stack access boxed");
#endif
        if (IS_SIMPLE_SCALAR(stackCell->u.sxpval, INTSXP)) {
            return *INTEGER(stackCell->u.sxpval);
        } else {
            return NA_INTEGER;
        }
    default:
        assert(false);
        return 0;
    }
}

// Returns NA_REAL if not a real, doesn't consider integers reals
RIR_INLINE double stackObjAsReal(R_bcstack_t* stackCell) {
#ifdef PROFILE_TYPED_STACK
    TSPROFILE.mark("stack access");
#endif
    switch (stackCell->tag) {
    case STACK_OBJ_REAL:
        return stackCell->u.dval;
    case STACK_OBJ_INT:
    case STACK_OBJ_LOGICAL:
        return NA_REAL;
    case STACK_OBJ_SEXP:
#ifdef PROFILE_TYPED_STACK
        TSPROFILE.mark("stack access boxed");
#endif
        if (IS_SIMPLE_SCALAR(stackCell->u.sxpval, REALSXP)) {
            return *REAL(stackCell->u.sxpval);
        } else {
            return NA_REAL;
        }
    default:
        assert(false);
        return 0;
    }
}

// Returns NA_LOGICAL if not a logical
RIR_INLINE int stackObjAsLogical(R_bcstack_t* stackCell) {
#ifdef PROFILE_TYPED_STACK
    TSPROFILE.mark("stack access");
#endif
    switch (stackCell->tag) {
    case STACK_OBJ_LOGICAL:
        return stackCell->u.ival;
    case STACK_OBJ_INT:
    case STACK_OBJ_REAL:
        return NA_LOGICAL;
    case STACK_OBJ_SEXP:
#ifdef PROFILE_TYPED_STACK
        TSPROFILE.mark("stack access boxed");
#endif
        if (IS_SIMPLE_SCALAR(stackCell->u.sxpval, LGLSXP)) {
            return *LOGICAL(stackCell->u.sxpval);
        } else {
            return NA_LOGICAL;
        }
    default:
        assert(false);
        return false;
    }
}

RIR_INLINE int stackObjAsLglScalar(R_bcstack_t* stackCell) {
    switch (stackCell->tag) {
    case STACK_OBJ_LOGICAL:
        return stackCell->u.ival;
    case STACK_OBJ_INT:
        return stackCell->u.ival != 0;
    case STACK_OBJ_REAL:
        return stackCell->u.dval != 0.0;
    case STACK_OBJ_SEXP:
        return Rf_asLogical(stackCell->u.sxpval);
    default:
        assert(false);
        return false;
    }
}

// Doesn't consider reals integers
RIR_INLINE bool stackObjIsInteger(R_bcstack_t* stackCell) {
#ifdef PROFILE_TYPED_STACK
    TSPROFILE.mark("type test");
#endif
    switch (stackCell->tag) {
    case STACK_OBJ_INT:
        return true;
    case STACK_OBJ_REAL:
    case STACK_OBJ_LOGICAL:
        return false;
    case STACK_OBJ_SEXP:
#ifdef PROFILE_TYPED_STACK
        TSPROFILE.mark("type test boxed");
#endif
        return IS_SIMPLE_SCALAR(stackCell->u.sxpval, INTSXP);
    default:
        assert(false);
        return false;
    }
}

// Doesn't consider integers reals
RIR_INLINE bool stackObjIsReal(R_bcstack_t* stackCell) {
#ifdef PROFILE_TYPED_STACK
    TSPROFILE.mark("type test");
#endif
    switch (stackCell->tag) {
    case STACK_OBJ_REAL:
        return true;
    case STACK_OBJ_INT:
    case STACK_OBJ_LOGICAL:
        return false;
    case STACK_OBJ_SEXP:
#ifdef PROFILE_TYPED_STACK
        TSPROFILE.mark("type test boxed");
#endif
        return IS_SIMPLE_SCALAR(stackCell->u.sxpval, REALSXP);
    default:
        assert(false);
        return (false);
    }
}

RIR_INLINE bool stackObjIsLogical(R_bcstack_t* stackCell) {
#ifdef PROFILE_TYPED_STACK
    TSPROFILE.mark("type test");
#endif
    switch (stackCell->tag) {
    case STACK_OBJ_LOGICAL:
        return true;
    case STACK_OBJ_INT:
    case STACK_OBJ_REAL:
        return false;
    case STACK_OBJ_SEXP:
#ifdef PROFILE_TYPED_STACK
        TSPROFILE.mark("type test boxed");
#endif
        return IS_SIMPLE_SCALAR(stackCell->u.sxpval, LGLSXP);
    default:
        assert(false);
        return (false);
    }
}

RIR_INLINE bool stackObjIsSimpleScalar(R_bcstack_t* stackCell, SEXPTYPE type) {
#ifdef PROFILE_TYPED_STACK
    TSPROFILE.mark("stack access");
#endif
    switch (stackCell->tag) {
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
        return IS_SIMPLE_SCALAR(stackCell->u.sxpval, type);
    default:
        assert(false);
        return false;
    }
}

RIR_INLINE bool stackObjIsScalar(R_bcstack_t* stackCell) {
#ifdef PROFILE_TYPED_STACK
    TSPROFILE.mark("stack access");
#endif
    switch (stackCell->tag) {
    case STACK_OBJ_INT:
    case STACK_OBJ_REAL:
    case STACK_OBJ_LOGICAL:
        return true;
    case STACK_OBJ_SEXP:
#ifdef PROFILE_TYPED_STACK
        TSPROFILE.mark("stack access boxed");
#endif
        return stackCell->u.sxpval->sxpinfo.scalar;
    default:
        assert(false);
        return false;
    }
}

RIR_INLINE SEXPTYPE stackObjIsBoxed(R_bcstack_t* stackCell) {
    return stackCell->tag == STACK_OBJ_SEXP;
}

RIR_INLINE bool stackObjIsVector(R_bcstack_t* stackCell) {
#ifdef PROFILE_TYPED_STACK
    TSPROFILE.mark("stack access");
#endif
    switch (stackCell->tag) {
    case STACK_OBJ_INT:
    case STACK_OBJ_REAL:
    case STACK_OBJ_LOGICAL:
        return true;
    case STACK_OBJ_SEXP:
#ifdef PROFILE_TYPED_STACK
        TSPROFILE.mark("stack access boxed");
#endif
        return Rf_isVector(stackCell->u.sxpval);
    default:
        assert(false);
        return false;
    }
}

RIR_INLINE bool stackObjsIdentical(R_bcstack_t* x, R_bcstack_t* y) {
#ifdef PROFILE_TYPED_STACK
    TSPROFILE.mark("stack access");
#endif
    if (x->tag != y->tag) {
        return false;
    }

    switch (x->tag) { // == y.tag
    case STACK_OBJ_INT:
    case STACK_OBJ_LOGICAL:
        return x->u.ival == y->u.ival;
    case STACK_OBJ_REAL:
        return x->u.dval == y->u.dval;
    case STACK_OBJ_SEXP:
#ifdef PROFILE_TYPED_STACK
        TSPROFILE.mark("stack access boxed");
#endif
        return x->u.sxpval == y->u.sxpval;
    default:
        assert(false);
        return false;
    }
}

// Fails if not a logical or NA
RIR_INLINE int tryStackObjToLogicalNa(R_bcstack_t* stackCell) {
#ifdef PROFILE_TYPED_STACK
    TSPROFILE.mark("stack access");
#endif
    switch (stackCell->tag) {
    case STACK_OBJ_LOGICAL:
        return stackCell->u.ival;
    case STACK_OBJ_SEXP:
#ifdef PROFILE_TYPED_STACK
        TSPROFILE.mark("stack access boxed");
#endif
        if (TYPEOF(stackCell->u.sxpval) == LGLSXP) {
            return XLENGTH(stackCell->u.sxpval) == 0
                       ? NA_LOGICAL
                       : *LOGICAL(stackCell->u.sxpval);
        } else {
            assert(false);
        }
    default:
        assert(false);
        return 0;
    }
}

// Returns regular if int, truncated if real, -1 otherwise
RIR_INLINE int tryStackObjToIdx(R_bcstack_t* stackCell) {
    if (stackObjIsInteger(stackCell)) {
        return stackObjAsInteger(stackCell) - 1;
    } else if (stackObjIsReal(stackCell)) {
        return (int)stackObjAsReal(stackCell) - 1;
    } else {
        return -1;
    }
}

RIR_INLINE SEXPTYPE stackObjTypeof(R_bcstack_t* stackCell) {
#ifdef PROFILE_TYPED_STACK
    TSPROFILE.mark("stack access");
#endif
    switch (stackCell->tag) {
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
        return TYPEOF(stackCell->u.sxpval);
    default:
        assert(false);
        return 0;
    }
}

static R_INLINE int tryStackScalar(R_bcstack_t* stackCell, scalar_value_t* v,
                                   SEXP* pv) {
#ifdef TYPED_STACK
    int tag = stackCell->tag;

    if (tag)
        switch (tag) {
        case REALSXP:
            v->dval = stackCell->u.dval;
            return tag;
        case INTSXP:
            v->ival = stackCell->u.ival;
            return tag;
        case LGLSXP:
            v->ival = stackCell->u.ival;
            return tag;
        }
#endif
    SEXP x = stackCell->u.sxpval;
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

static R_INLINE int tryStackScalarReal(R_bcstack_t* stackCell,
                                       scalar_value_t* v, SEXP* pv) {
    int typex = tryStackScalar(stackCell, v, pv);
    if (typex == INTSXP) {
        typex = REALSXP;
        v->dval = INTEGER_TO_REAL(v->ival);
        if (pv)
            *pv = NULL;
    }
    return typex;
}

RIR_INLINE R_xlen_t stackObjLength(R_bcstack_t* stackCell) {
#ifdef PROFILE_TYPED_STACK
    TSPROFILE.mark("stack access");
#endif
    switch (stackCell->tag) {
    case STACK_OBJ_INT:
    case STACK_OBJ_REAL:
    case STACK_OBJ_LOGICAL:
        return 1;
    case STACK_OBJ_SEXP:
#ifdef PROFILE_TYPED_STACK
        TSPROFILE.mark("stack access boxed");
#endif
        return XLENGTH(stackCell->u.sxpval);
    default:
        assert(false);
        return 0;
    }
}

RIR_INLINE void setInPlace(SEXP old, R_bcstack_t* val) {
    switch (val->tag) {
    case STACK_OBJ_INT:
        *INTEGER(old) = val->u.ival;
    case STACK_OBJ_REAL:
        *REAL(old) = val->u.dval;
    case STACK_OBJ_LOGICAL:
        *LOGICAL(old) = val->u.ival;
    }
}

#define ostackLength(c) (R_BCNodeStackTop - R_BCNodeStackBase)

#define ostackTop(c) *(R_BCNodeStackTop - 1)
#define ostackCellTop(c) ostackCellAt(c, 0)
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

#define ostackSetSexpCache(cacheStart, i, v)                                   \
    do {                                                                       \
        R_bcstack_t* stk = cacheStart + (i);                                   \
        stk->u.sxpval = (v);                                                   \
        stk->tag = STACK_OBJ_SEXP;                                             \
    } while (0)

#define ostackPopn(c, p)                                                       \
    do {                                                                       \
        R_BCNodeStackTop -= (p);                                               \
    } while (0)

#define ostackPop(c) (*(--R_BCNodeStackTop))
#define ostackCellPop(c) (--R_BCNodeStackTop)

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

RIR_INLINE SEXP ostackSexpAt(InterpreterInstance* ctx, unsigned idx) {
    return stackObjToSexp(ostackCellAt(ctx, idx));
}

RIR_INLINE SEXP ostackPopSexp(InterpreterInstance* ctx) {
    return stackObjToSexp(ostackCellPop(ctx));
}

RIR_INLINE SEXP ostackTopSexp(InterpreterInstance* ctx) {
    return stackObjToSexp(ostackCellTop(ctx));
}

RIR_INLINE void ostackEnsureSize(InterpreterInstance* ctx, unsigned minFree) {
    if ((R_BCNodeStackTop + minFree) >= R_BCNodeStackEnd) {
        // TODO....
        assert(false);
    }
}

// --- Locals

class Locals final {
    // NOTE: must not own any resources, because the destructor is not
    // called
    //       if there is a longjmp from the evalRirCode call
  private:
    R_bcstack_t* base;
    unsigned localsCount;
    bool existingLocals;
    R_bcstack_t* cacheStart;

  public:
    explicit Locals(R_bcstack_t* base, unsigned count, bool existingLocals,
                    R_bcstack_t* cacheStart)
        : base(base), localsCount(count), existingLocals(existingLocals),
          cacheStart(cacheStart) {
        if (!existingLocals) {
#ifdef TYPED_STACK
            // Zero the region of the locals to avoid keeping stuff alive and to
            // zero all the type tags. Note: this trick does not work with the
            // stack in general, since there intermediate callees might set the
            // type tags to something else.
            memset(R_BCNodeStackTop, 0,
                   sizeof(*R_BCNodeStackTop) * localsCount);
#endif
            R_BCNodeStackTop += localsCount;
        }
    }

    ~Locals() {
        if (cacheStart)
            R_BCNodeStackTop = cacheStart;
        else {
            if (!existingLocals)
                R_BCNodeStackTop -= localsCount;
        }
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
