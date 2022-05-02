#ifndef RIR_R_H
#define RIR_R_H

#include "common.h"

#define R_NO_REMAP
#define USE_RINTERNALS
#include <R.h>
#include <Rinterface.h>
#include <Rinternals.h>
#include <R_ext/Print.h>

// Use the function versions (some of them clash with LLVM)
#undef isNull
#undef isSymbol
#undef isLogical
#undef isReal
#undef isComplex
#undef isExpression
#undef isEnvironment
#undef isString
#undef isObject

// Clash with LLVM
#undef PI

// Get rid of the macro version of this
#undef R_CheckStack

// Bypass PREXPR from GNU R which causes code objects to be converted to AST
#undef PREXPR
inline SEXP PREXPR(SEXP pr) {
    SLOWASSERT(TYPEOF(pr) == PROMSXP);
    auto res = pr->u.promsxp.expr;
    if (TYPEOF(res) == BCODESXP)
        return R_PromiseExpr(pr);
    return res;
}

extern "C" {
extern FUNTAB R_FunTab[];
extern SEXP R_TrueValue;
extern SEXP R_FalseValue;
extern SEXP R_LogicalNAValue;

Rboolean Rf_isNull(SEXP s);
Rboolean Rf_isSymbol(SEXP s);
Rboolean Rf_isLogical(SEXP s);
Rboolean Rf_isReal(SEXP s);
Rboolean Rf_isComplex(SEXP s);
Rboolean Rf_isExpression(SEXP s);
Rboolean Rf_isEnvironment(SEXP s);
Rboolean Rf_isString(SEXP s);
Rboolean Rf_isObject(SEXP s);
}

// Performance critical stuff copied from Rinlinedfun.h

#ifdef ENABLE_SLOWASSERT
inline void CHKVEC(SEXP x) {
    switch (TYPEOF(x)) {
    case CHARSXP:
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case VECSXP:
    case EXPRSXP:
    case RAWSXP:
    case WEAKREFSXP:
    case EXTERNALSXP: // added by RIR
        break;
    default:
        assert(false);
    }
}
#else
#define CHKVEC(x)                                                              \
    do {                                                                       \
    } while (0)
#endif

inline void* DATAPTR(SEXP x) {
    CHKVEC(x);
    if (ALTREP(x))
        return ALTVEC_DATAPTR(x);
#ifdef CATCH_ZERO_LENGTH_ACCESS
    /* Attempts to read or write elements of a zero length vector will
       result in a segfault, rather than read and write random memory.
       Returning NULL would be more natural, but Matrix seems to assume
       that even zero-length vectors have non-NULL data pointers, so
       return (void *) 1 instead. Zero-length CHARSXP objects still
       have a trailing zero byte so they are not handled. */
    else if (STDVEC_LENGTH(x) == 0 && TYPEOF(x) != CHARSXP)
        return (void*)1;
#endif
    else
        return STDVEC_DATAPTR(x);
}

inline R_xlen_t XLENGTH_EX(SEXP x) {
    return ALTREP(x) ? ALTREP_LENGTH(x) : STDVEC_LENGTH(x);
}

typedef struct {
    int ibeta, it, irnd, ngrd, machep, negep, iexp, minexp, maxexp;
    double eps, epsneg, xmin, xmax;
} AccuracyInfo;
LibExtern AccuracyInfo R_AccuracyInfo;

extern int R_PPStackTop;

#define CLEAR_ATTRIB(x)                                                        \
    do {                                                                       \
        SEXP __x__ = (x);                                                      \
        if (ATTRIB(__x__) != R_NilValue) {                                     \
            SET_ATTRIB(__x__, R_NilValue);                                     \
            if (OBJECT(__x__))                                                 \
                SET_OBJECT(__x__, 0);                                          \
            if (IS_S4_OBJECT(__x__))                                           \
                UNSET_S4_OBJECT(__x__);                                        \
        }                                                                      \
    } while (0)

#endif
