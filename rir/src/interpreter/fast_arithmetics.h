#ifndef fast_arithmetics_h
#define fast_arithmetics_h

#define R_INT_MAX INT_MAX
#define R_INT_MIN -INT_MAX
// .. relying on fact that NA_INTEGER is outside of these
#define GOODIPROD(x, y, z) ((double)(x) * (double)(y) == (z))
#define INTEGER_OVERFLOW_WARNING "NAs produced by integer overflow"

#define DO_UNOP(op)                                                            \
    do {                                                                       \
        scalar_value_t scalarOp;                                               \
        SEXP reusableSexpOp = NULL;                                            \
        int typeOp = tryStackScalar(val, &scalarOp, &reusableSexpOp);          \
        if (typeOp == REALSXP) {                                               \
            double res =                                                       \
                (scalarOp.dval == NA_REAL) ? NA_REAL : op scalarOp.dval;       \
            STORE_UNOP_FAST(res, reusableSexpOp, Real, REAL);                  \
        } else if (typeOp == INTSXP && scalarOp.ival != NA_INTEGER) {          \
            STORE_UNOP_FAST(op scalarOp.ival, reusableSexpOp, Int, INTEGER);   \
        } else {                                                               \
            UNOP_FALLBACK(#op);                                                \
            break;                                                             \
        }                                                                      \
    } while (false)

#define DO_BINOP(op, realRes)                                                  \
    do {                                                                       \
        scalar_value_t lhsScalar;                                              \
        scalar_value_t rhsScalar;                                              \
        SEXP reusableSexpLhs = NULL;                                           \
        SEXP reusableSexpRhs = NULL;                                           \
        reusableSexpLhs = reusableSexpLhs ? reusableSexpLhs : reusableSexpRhs; \
        int typeLhs = tryStackScalar(lhs, &lhsScalar, &reusableSexpLhs);       \
        int typeRhs = tryStackScalar(rhs, &rhsScalar, &reusableSexpRhs);       \
        auto handled = true;                                                   \
        if (typeLhs == REALSXP) {                                              \
            if (typeRhs == REALSXP) {                                          \
                double real_res = lhsScalar.dval op rhsScalar.dval;            \
                STORE_BINOP_FAST(real_res, reusableSexpLhs, Real, REAL);       \
            } else if (typeRhs == INTSXP) {                                    \
                double real_res = (lhsScalar.dval == NA_REAL ||                \
                                   rhsScalar.ival == NA_INTEGER)               \
                                      ? NA_REAL                                \
                                      : lhsScalar.dval op rhsScalar.ival;      \
                STORE_BINOP_FAST(real_res, reusableSexpLhs, Real, REAL);       \
            } else                                                             \
                handled = false;                                               \
        } else if (typeLhs == INTSXP && lhsScalar.ival != NA_INTEGER) {        \
            if (typeRhs == REALSXP) {                                          \
                double real_res = (lhsScalar.ival == NA_INTEGER ||             \
                                   rhsScalar.dval == NA_REAL)                  \
                                      ? NA_REAL                                \
                                      : lhsScalar.ival op rhsScalar.dval;      \
                STORE_BINOP_FAST(real_res, reusableSexpRhs, Real, REAL);       \
            } else if (typeRhs == INTSXP && rhsScalar.ival != NA_INTEGER) {    \
                if (realRes) {                                                 \
                    double real_res =                                          \
                        (lhsScalar.ival == NA_INTEGER ||                       \
                         rhsScalar.dval == NA_REAL)                            \
                            ? NA_REAL                                          \
                            : (double)lhsScalar.ival op(double)                \
                                  rhsScalar.ival;                              \
                    STORE_BINOP_FAST(real_res, NULL, Real, REAL);              \
                } else {                                                       \
                    int int_res = (lhsScalar.ival == NA_INTEGER ||             \
                                   rhsScalar.ival == NA_INTEGER)               \
                                      ? NA_INTEGER                             \
                                      : lhsScalar.ival op rhsScalar.ival;      \
                    STORE_BINOP_FAST(int_res, reusableSexpLhs, Int, INTEGER);  \
                }                                                              \
            } else {                                                           \
                handled = false;                                               \
            }                                                                  \
        } else                                                                 \
            handled = false;                                                   \
        if (!handled) {                                                        \
            BINOP_FALLBACK(#op);                                               \
        }                                                                      \
    } while (false)

#define DO_RELOP(op)                                                           \
    do {                                                                       \
        scalar_value_t lhsScalar;                                              \
        scalar_value_t rhsScalar;                                              \
        int typeLhs = tryStackScalar(lhs, &lhsScalar, nullptr);                \
        int typeRhs = tryStackScalar(rhs, &rhsScalar, nullptr);                \
        auto handled = true;                                                   \
        if (typeLhs == REALSXP && !ISNAN(lhsScalar.dval)) {                    \
            if (typeRhs == REALSXP && !ISNAN(rhsScalar.dval))                  \
                STORE_BINOP_FAST(lhsScalar.dval op rhsScalar.dval, NULL,       \
                                 Logical, LOGICAL);                            \
            else if (typeRhs == INTSXP && rhsScalar.ival != NA_INTEGER)        \
                STORE_BINOP_FAST(lhsScalar.dval op rhsScalar.ival, NULL,       \
                                 Logical, LOGICAL);                            \
            else                                                               \
                handled = false;                                               \
        } else if (typeLhs == INTSXP && lhsScalar.ival != NA_INTEGER) {        \
            if (typeRhs == REALSXP && !ISNAN(rhsScalar.dval))                  \
                STORE_BINOP_FAST(lhsScalar.ival op rhsScalar.dval, NULL,       \
                                 Logical, LOGICAL);                            \
            else if (typeRhs == INTSXP && rhsScalar.ival != NA_INTEGER)        \
                STORE_BINOP_FAST(lhsScalar.ival op rhsScalar.ival, NULL,       \
                                 Logical, LOGICAL);                            \
            else                                                               \
                handled = false;                                               \
        } else                                                                 \
            handled = false;                                                   \
        if (!handled)                                                          \
            BINOP_FALLBACK(#op);                                               \
    } while (false)

#define UNOP_FALLBACK(op)                                                      \
    do {                                                                       \
        static SEXP prim = NULL;                                               \
        static CCODE blt;                                                      \
        static int flag;                                                       \
        if (!prim) {                                                           \
            prim = Rf_findFun(Rf_install(op), R_GlobalEnv);                    \
            blt = getBuiltin(prim);                                            \
            flag = getFlag(prim);                                              \
        }                                                                      \
        SEXP call = getSrcForCall(c, pc - 1, ctx);                             \
        PROTECT(call);                                                         \
        SEXP valSexp = ostackSexpAt(ctx, 0);                                   \
        PROTECT(valSexp);                                                      \
        SEXP argslist = CONS_NR(valSexp, R_NilValue);                          \
        UNPROTECT(2);                                                          \
        ostackPushSexp(ctx, argslist);                                         \
        if (flag < 2)                                                          \
            R_Visible = static_cast<Rboolean>(flag != 1);                      \
        SEXP res = blt(call, prim, argslist, env);                             \
        if (flag < 2)                                                          \
            R_Visible = static_cast<Rboolean>(flag != 1);                      \
        ostackPopn(ctx, 2);                                                    \
        ostackPushSexp(ctx, res);                                              \
    } while (false)

#define BINOP_FALLBACK(op)                                                     \
    do {                                                                       \
        static SEXP prim = NULL;                                               \
        static CCODE blt;                                                      \
        static int flag;                                                       \
        if (!prim) {                                                           \
            prim = Rf_findFun(Rf_install(op), R_GlobalEnv);                    \
            blt = getBuiltin(prim);                                            \
            flag = getFlag(prim);                                              \
        }                                                                      \
        SEXP call = getSrcForCall(c, pc - 1, ctx);                             \
        PROTECT(call);                                                         \
        SEXP lhsSexp = ostackSexpAt(ctx, 1);                                   \
        PROTECT(lhsSexp);                                                      \
        SEXP rhsSexp = ostackSexpAt(ctx, 0);                                   \
        SEXP argslist = CONS_NR(lhsSexp, CONS_NR(rhsSexp, R_NilValue));        \
        UNPROTECT(2);                                                          \
        ostackPushSexp(ctx, argslist);                                         \
        if (flag < 2)                                                          \
            R_Visible = static_cast<Rboolean>(flag != 1);                      \
        SEXP res = blt(call, prim, argslist, env);                             \
        if (flag < 2)                                                          \
            R_Visible = static_cast<Rboolean>(flag != 1);                      \
        ostackPopn(ctx, 3);                                                    \
        ostackPushSexp(ctx, res);                                              \
    } while (false)

#define STORE_UNOP(res)                                                        \
    do {                                                                       \
        ostackSet(ctx, 0, res);                                                \
    } while (false)

#define STORE_UNOP_FAST(res, sexpVal, Type, TYPE)                              \
    do {                                                                       \
        if (sexpVal) {                                                         \
            *TYPE(sexpVal) = res;                                              \
            ostackSetSexp(ctx, 0, sexpVal);                                    \
        } else {                                                               \
            ostackSet##Type(ctx, 0, res);                                      \
        }                                                                      \
    } while (false)

#define STORE_BINOP(res)                                                       \
    do {                                                                       \
        ostackPop(ctx);                                                        \
        STORE_UNOP(res);                                                       \
    } while (false)

#define STORE_BINOP_FAST(res, sexpVal, Type, TYPE)                             \
    do {                                                                       \
        ostackPop(ctx);                                                        \
        STORE_UNOP_FAST(res, sexpVal, Type, TYPE);                             \
    } while (false)

// Fast vector macros from gnu-r (adapted)
static R_INLINE SEXP mkVector1(SEXP s) {
    SEXP t = allocVector(VECSXP, 1);
    SET_VECTOR_ELT(t, 0, s);
    return t;
}

static R_INLINE SEXP getMatrixDim(SEXP mat) {
    SEXP attr = ATTRIB(mat);
    /* look for the common case of 'dim' as the only attribute first */
    SEXP dim =
        TAG(attr) == R_DimSymbol ? CAR(attr) : getAttrib(mat, R_DimSymbol);
    if (TYPEOF(dim) == INTSXP && LENGTH(dim) == 2)
        return dim;
    else
        return R_NilValue;
}

static R_INLINE Rboolean setElementFromScalar(SEXP vec, R_xlen_t index,
                                              int typev,
                                              rir::scalar_value_t* scalar) {
    if (index < 0)
        return FALSE;

    if (TYPEOF(vec) == REALSXP) {
        if (XLENGTH(vec) <= index)
            return FALSE;
        switch (typev) {
        case REALSXP:
            REAL(vec)[index] = scalar->dval;
            return TRUE;
        case INTSXP:
            REAL(vec)[index] = INTEGER_TO_REAL(scalar->ival);
            return TRUE;
        case LGLSXP:
            REAL(vec)[index] = LOGICAL_TO_REAL(scalar->ival);
            return TRUE;
        }
    } else if (typev == TYPEOF(vec)) {
        switch (typev) {
        case INTSXP:
            if (XLENGTH(vec) <= index)
                return FALSE;
            INTEGER(vec)[index] = scalar->ival;
            return TRUE;
        case LGLSXP:
            if (XLENGTH(vec) <= index)
                return FALSE;
            LOGICAL(vec)[index] = INTEGER_TO_LOGICAL(scalar->ival);
            return TRUE;
        }
    }
    return FALSE;
}

#define FAST_VECELT_OK(vec)                                                    \
    (ATTRIB(vec) == R_NilValue ||                                              \
     (TAG(ATTRIB(vec)) == R_DimSymbol && CDR(ATTRIB(vec)) == R_NilValue))

#define FAST_VECELT(vec, index, popCount, subset2)                             \
    do {                                                                       \
        ;                                                                      \
        switch (TYPEOF(vec)) {                                                 \
        case REALSXP:                                                          \
            if (XLENGTH(vec) > index) {                                        \
                handled = true;                                                \
                ostackPopn(ctx, popCount);                                     \
                ostackPushReal(ctx, REAL_ELT(vec, index));                     \
            }                                                                  \
            break;                                                             \
        case INTSXP:                                                           \
            if (XLENGTH(vec) > index) {                                        \
                handled = true;                                                \
                ostackPopn(ctx, popCount);                                     \
                ostackPushInt(ctx, INTEGER_ELT(vec, index));                   \
            }                                                                  \
            break;                                                             \
        case LGLSXP:                                                           \
            if (XLENGTH(vec) > index) {                                        \
                handled = true;                                                \
                ostackPopn(ctx, popCount);                                     \
                ostackPushLogical(ctx, LOGICAL_ELT(vec, index));               \
            }                                                                  \
            break;                                                             \
        case CPLXSXP:                                                          \
            if (XLENGTH(vec) > index) {                                        \
                handled = true;                                                \
                ostackPopn(ctx, popCount);                                     \
                ostackPushSexp(ctx, ScalarComplex(COMPLEX_ELT(vec, index)));   \
            }                                                                  \
            break;                                                             \
        case RAWSXP:                                                           \
            if (XLENGTH(vec) > index) {                                        \
                handled = true;                                                \
                ostackPopn(ctx, popCount);                                     \
                ostackPushSexp(ctx, ScalarRaw(RAW(vec)[index]));               \
            }                                                                  \
            break;                                                             \
        case VECSXP:                                                           \
            if (XLENGTH(vec) > index) {                                        \
                handled = true;                                                \
                SEXP elt = VECTOR_ELT(vec, index);                             \
                ENSURE_NAMED(elt);                                             \
                ostackPopn(ctx, popCount);                                     \
                if (subset2)                                                   \
                    ostackPushSexp(ctx, elt);                                  \
                else                                                           \
                    ostackPushSexp(ctx, mkVector1(elt));                       \
            }                                                                  \
            break;                                                             \
        }                                                                      \
    } while (false)

#define FAST_SETVECELT(value, vec, index, popCount, subassign2)                \
    do {                                                                       \
        scalar_value_t v;                                                      \
        int typev = tryStackScalar(value, &v, nullptr);                        \
        if (setElementFromScalar(vec, index, typev, &v)) {                     \
            ostackPopn(ctx, popCount);                                         \
            ostackPushSexp(ctx, vec);                                          \
            SETTER_CLEAR_NAMED(vec);                                           \
            handled = true;                                                    \
        } else if (subassign2 && TYPEOF(vec) == VECSXP &&                      \
                   index < XLENGTH(vec)) {                                     \
            SEXP rhs = R_FixupRHS(vec, stackObjToSexp(value));                 \
            if (rhs != R_NilValue) {                                           \
                SET_VECTOR_ELT(vec, index, rhs);                               \
                SETTER_CLEAR_NAMED(vec);                                       \
                ostackPopn(ctx, popCount);                                     \
                ostackPushSexp(ctx, vec);                                      \
                handled = true;                                                \
            }                                                                  \
        }                                                                      \
    } while (0)

#define CHECK_INTEGER_OVERFLOW(ans, naflag)                                    \
    do {                                                                       \
        if (naflag) {                                                          \
            PROTECT(ans);                                                      \
            SEXP call = getSrcForCall(c, pc - 1, ctx);                         \
            Rf_warningcall(call, INTEGER_OVERFLOW_WARNING);                    \
            UNPROTECT(1);                                                      \
        }                                                                      \
    } while (0)

static double myfloor(double x1, double x2) {
    double q = x1 / x2, tmp;

    if (x2 == 0.0)
        return q;
    tmp = x1 - floor(q) * x2;
    return floor(q) + floor(tmp / x2);
}

static double myfmod(double x1, double x2) {
    if (x2 == 0.0)
        return R_NaN;
    double q = x1 / x2, tmp = x1 - floor(q) * x2;
    if (R_FINITE(q) && (fabs(q) > 1 / R_AccuracyInfo.eps))
        Rf_warning("probable complete loss of accuracy in modulus");
    q = floor(tmp / x2);
    return tmp - q * x2;
}

#endif