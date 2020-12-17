#ifndef RIR_R_H
#define RIR_R_H

#include "common.h"

#include <R.h>
#include <Rinterface.h>
#define USE_RINTERNALS
#include <Rinternals.h>

// r print statement
#include <R_ext/Print.h>

#undef error
#undef TRUE
#undef FALSE
#undef length
#undef eval
#undef cons

#undef PREXPR
inline SEXP PREXPR(SEXP pr) {
    // bypassing PREXPR from Gnur, which causes code objects to be converted to
    // AST
    SLOWASSERT(TYPEOF(pr) == PROMSXP);
    auto res = pr->u.promsxp.expr;
    if (TYPEOF(res) == BCODESXP)
        return R_PromiseExpr(pr);
    return res;
}

extern "C" {
extern SEXP R_TrueValue;
extern SEXP R_FalseValue;
extern SEXP R_LogicalNAValue;
};

// Performance critical stuff copied from Rinlinedfun.h

#ifdef ENABLE_SLOWASSERT
RIR_INLINE void CHKVEC(SEXP x) {
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

RIR_INLINE void* DATAPTR(SEXP x) {
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

RIR_INLINE R_xlen_t XLENGTH_EX(SEXP x) {
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
