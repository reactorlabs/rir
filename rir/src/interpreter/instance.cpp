#include "instance.h"
#include "api.h"

namespace rir {

void initializeResizeableList(ResizeableList* l, size_t capacity, SEXP parent,
                              size_t index) {
    l->capacity = capacity;
    l->list = Rf_allocVector(VECSXP, capacity);
    SET_VECTOR_ELT(parent, index, l->list);
    rl_setLength(l, 0);
}

SEXP R_Subset2Sym;
SEXP R_SubsetSym;
SEXP R_SubassignSym;
SEXP R_Subassign2Sym;
SEXP R_valueSym;
SEXP setterPlaceholderSym;
SEXP getterPlaceholderSym;
SEXP quoteSym;

// How much the SEXP needs to be accessed until it gets unboxed.
static const unsigned sexpBoxThreshold = 63;

#define LOG_SEXP_BOX

// Will return 'true' once every 'sexpBoxThreshold + 1' times for each SEXP
// If successful, resets the counter, otherwise increments it.
bool tryUnboxSexp(SEXP x) {
    assert(x != loopTrampolineMarker);
    unsigned ratio = x->sxpinfo.extra & 0x8F;
    x->sxpinfo.extra &= ~0x8F;
    if (ratio < sexpBoxThreshold) {
        ratio++;
        x->sxpinfo.extra |= ratio;
        return false;
    } else {
#ifdef LOG_SEXP_BOX
        std::cout << "Unboxed SEXP\n";
        Rf_PrintValue(x);
#endif
        return true;
    }
}

R_bcstack_t intStackObj(int x) {
    R_bcstack_t res;
#ifdef USE_TYPED_STACK
    res.tag = STACK_OBJ_INT;
    res.u.ival = x;
#else
    res.tag = STACK_OBJ_SEXP;
    res.u.sxpval = Rf_allocVector(INTSXP, 1);
    *INTEGER(res.u.sxpval) = x;
#endif
    return res;
}

R_bcstack_t realStackObj(double x) {
    R_bcstack_t res;
#ifdef USE_TYPED_STACK
    res.tag = STACK_OBJ_REAL;
    res.u.dval = x;
#else
    res.tag = STACK_OBJ_SEXP;
    res.u.sxpval = Rf_allocVector(REALSXP, 1);
    *REAL(res.u.sxpval) = x;
#endif
    return res;
}

R_bcstack_t logicalStackObj(int x) {
    R_bcstack_t res;
#ifdef USE_TYPED_STACK
    res.tag = STACK_OBJ_LOGICAL;
    res.u.ival = x;
#else
    res.tag = STACK_OBJ_SEXP;
    res.u.sxpval = Rf_allocVector(LGLSXP, 1);
    *LOGICAL(res.u.sxpval) = x;
#endif
    return res;
}

R_bcstack_t sexpToStackObj(SEXP x, bool unprotect) {
    assert(x != NULL);
#ifdef USE_TYPED_STACK
    if (x != loopTrampolineMarker && ATTRIB(x) == R_NilValue) {
        if (IS_SIMPLE_SCALAR(x, INTSXP) && tryUnboxSexp(x)) {
            return intStackObj(*INTEGER(x));
        } else if (IS_SIMPLE_SCALAR(x, REALSXP) && tryUnboxSexp(x)) {
            return realStackObj(*REAL(x));
        } else if (IS_SIMPLE_SCALAR(x, LGLSXP) && tryUnboxSexp(x)) {
            return logicalStackObj(*INTEGER(x));
        }
    }
#endif
    R_bcstack_t res;
    res.tag = STACK_OBJ_SEXP;
    res.u.sxpval = x;
    return res;
}

SEXP stackObjToSexp(R_bcstack_t x) {
    switch (x.tag) {
    case STACK_OBJ_INT:
#ifdef USE_TYPED_STACK
        SEXP res;
        res = Rf_allocVector(INTSXP, 1);
        *INTEGER(res) = x.u.ival;
#ifdef LOG_SEXP_BOX
        std::cout << "Boxed int " << x.u.ival << "\n";
#endif
        return res;
    case STACK_OBJ_REAL:
        res = Rf_allocVector(REALSXP, 1);
        *REAL(res) = x.u.dval;
#ifdef LOG_SEXP_BOX
        std::cout << "Boxed real " << x.u.dval << "\n";
#endif
        return res;
    case STACK_OBJ_LOGICAL:
        res = Rf_allocVector(LGLSXP, 1);
        *LOGICAL(res) = x.u.ival;
#ifdef LOG_SEXP_BOX
        std::cout << "Boxed logical " << x.u.ival << "\n";
#endif
        return res;
#endif
    case STACK_OBJ_SEXP:
        return x.u.sxpval;
    default:
        assert(false);
    }
}

// Doesn't consider reals integers
bool stackObjIsInteger(R_bcstack_t x) {
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
int tryStackObjToInteger(R_bcstack_t x) {
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
bool stackObjIsReal(R_bcstack_t x) {
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
double tryStackObjToReal(R_bcstack_t x) {
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

bool stackObjIsLogical(R_bcstack_t x) {
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
int tryStackObjToLogical(R_bcstack_t x) {
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
int tryStackObjToLogicalNa(R_bcstack_t x) {
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
int tryStackObjToIdx(R_bcstack_t x) {
    if (stackObjIsInteger(x)) {
        return tryStackObjToInteger(x) - 1;
    } else if (stackObjIsReal(x)) {
        return (int)tryStackObjToReal(x) - 1;
    } else {
        return -1;
    }
}

SEXPTYPE stackObjSexpType(R_bcstack_t x) {
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

bool stackObjIsVector(R_bcstack_t x) {
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

bool stackObjIsSimpleScalar(R_bcstack_t x, SEXPTYPE type) {
    switch (x.tag) {
    case STACK_OBJ_INT:
        return type == INTSXP;
    case STACK_OBJ_REAL:
        return type == REALSXP;
    case STACK_OBJ_LOGICAL:
        return type == LGLSXP;
    case STACK_OBJ_SEXP:
        return IS_SIMPLE_SCALAR(x.u.sxpval, type);
    default:
        assert(false);
    }
}

R_xlen_t stackObjLength(R_bcstack_t x) {
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

bool stackObjsIdentical(R_bcstack_t x, R_bcstack_t y) {
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

bool trySetInPlace(SEXP old, R_bcstack_t val) {
    switch (val.tag) {
    case STACK_OBJ_INT:
        if (NOT_SHARED(old) && IS_SIMPLE_SCALAR(old, INTSXP)) {
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
        if (NOT_SHARED(old) && IS_SIMPLE_SCALAR(old, REALSXP)) {
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
        if (NOT_SHARED(old) && IS_SIMPLE_SCALAR(old, LGLSXP)) {
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

SEXP ostackObjToSexpAt(R_bcstack_t& x, InterpreterInstance* ctx, unsigned idx) {
    if (x.tag != STACK_OBJ_SEXP) {
        SEXP sexp = stackObjToSexp(x);
        x.u.sxpval = sexp;
        x.tag = STACK_OBJ_SEXP;
        ostackSet(ctx, idx, x);
    }
    return x.u.sxpval;
}

SEXP ostackSexpAt(InterpreterInstance* ctx, unsigned idx) {
    R_bcstack_t x = ostackAt(ctx, idx);
    SEXP sexp = ostackObjToSexpAt(x, ctx, idx);
    return sexp;
}

SEXP ostackPopSexp(InterpreterInstance* ctx) {
    return stackObjToSexp(ostackPop(ctx));
}

void ostackEnsureSize(InterpreterInstance* ctx, unsigned minFree) {
    if ((R_BCNodeStackTop + minFree) >= R_BCNodeStackEnd) {
        // TODO....
        assert(false);
    }
}

InterpreterInstance* context_create() {
    InterpreterInstance* c = new InterpreterInstance;
    c->list = Rf_allocVector(VECSXP, 2);
    R_PreserveObject(c->list);
    initializeResizeableList(&c->cp, POOL_CAPACITY, c->list, CONTEXT_INDEX_CP);
    initializeResizeableList(&c->src, POOL_CAPACITY, c->list,
                             CONTEXT_INDEX_SRC);
    // first item in source and constant pools is R_NilValue so that we can use
    // the index 0 for other purposes
    src_pool_add(c, R_NilValue);
    cp_pool_add(c, R_NilValue);
    R_Subset2Sym = Rf_install("[[");
    R_SubsetSym = Rf_install("[");
    R_SubassignSym = Rf_install("[<-");
    R_Subassign2Sym = Rf_install("[[<-");
    R_valueSym = Rf_install("value");
    setterPlaceholderSym = Rf_install("*.placeholder.setter.*");
    getterPlaceholderSym = Rf_install("*.placeholder.getter.*");
    quoteSym = Rf_install("quote");

    auto pir = getenv("PIR_ENABLE");

    c->exprCompiler = rir_compile;
    c->closureCompiler = [](SEXP closure, SEXP name) {
        return rir_compile(closure, R_NilValue);
    };
    c->closureOptimizer = [](SEXP f, const Assumptions&, SEXP n) { return f; };

    if (pir && std::string(pir).compare("off") == 0) {
        // do nothing; use defaults
    } else if (pir && std::string(pir).compare("force") == 0) {
        c->closureCompiler = [](SEXP f, SEXP n) {
            SEXP rir = rir_compile(f, R_NilValue);
            return rirOptDefaultOpts(rir, Assumptions(), n);
        };
    } else if (pir && std::string(pir).compare("force_dryrun") == 0) {
        c->closureCompiler = [](SEXP f, SEXP n) {
            SEXP rir = rir_compile(f, R_NilValue);
            return rirOptDefaultOptsDryrun(rir, Assumptions(), n);
        };
    } else {
        c->closureOptimizer = rirOptDefaultOpts;
    }

    return c;
}

extern InterpreterInstance* globalInterpreterInstance_;

} // namespace rir
