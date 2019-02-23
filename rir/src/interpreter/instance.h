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

typedef enum {
    STACK_OBJ_SEXP,
    STACK_OBJ_INT,
    STACK_OBJ_REAL,
    STACK_OBJ_LOGICAL
} stackObjType;

bool shouldBoxSexp(SEXP x);
void preventBoxingSexp(SEXP x);

R_bcstack_t intStackObj(int x);
R_bcstack_t realStackObj(double x);
R_bcstack_t logicalStackObj(int x);
R_bcstack_t sexpToStackObj(SEXP x, bool unprotect);
// Warning: If the SEXP is modified, the original stack object won't change.
// If the object is in the ostack, use 'ostackSexpAt' or 'ostackObjToSexpAt'.
SEXP stackObjToSexp(R_bcstack_t x);
// Doesn't consider reals integers
bool stackObjIsInteger(R_bcstack_t x);
// Returns NA_INTEGER if not an integer, doesn't consider reals integers
int tryStackObjToInteger(R_bcstack_t x);
// Doesn't consider integers reals
bool stackObjIsReal(R_bcstack_t x);
// Returns NA_REAL if not a real, doesn't consider integers reals
double tryStackObjToReal(R_bcstack_t x);
bool stackObjIsLogical(R_bcstack_t x);
// Returns NA_LOGICAL if not a logical
int tryStackObjToLogical(R_bcstack_t x);
// Fails if not a logical or NA
int tryStackObjToLogicalNa(R_bcstack_t x);
// Returns regular if int, truncated if real, -1 otherwise
int tryStackObjToIdx(R_bcstack_t x);
SEXPTYPE stackObjSexpType(R_bcstack_t x);
bool stackObjIsVector(R_bcstack_t x);
bool stackObjIsSimpleScalar(R_bcstack_t x, SEXPTYPE type);
R_xlen_t stackObjLength(R_bcstack_t x);
// Uses pointer equality for SEXPs
bool stackObjsIdentical(R_bcstack_t x, R_bcstack_t y);

#define ostackLength(c) (R_BCNodeStackTop - R_BCNodeStackBase)

#define ostackTop(c) *(R_BCNodeStackTop - 1)

#define ostackAt(c, i) *(R_BCNodeStackTop - 1 - (i))

#define ostackCellAt(c, i) (R_BCNodeStackTop - 1 - (i))

#define ostackEmpty(c) (R_BCNodeStackTop == R_BCNodeStackBase)

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

bool trySetInPlace(SEXP old, R_bcstack_t val);
void ostackSet(InterpreterInstance* ctx, unsigned idx, R_bcstack_t x);
SEXP ostackObjToSexpAt(R_bcstack_t& x, InterpreterInstance* ctx, unsigned idx);
SEXP ostackSexpAt(InterpreterInstance* ctx, unsigned idx);
SEXP ostackPopSexp(InterpreterInstance* ctx);
void ostackEnsureSize(InterpreterInstance* ctx, unsigned minFree);

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
