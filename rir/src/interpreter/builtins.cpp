#include "builtins.h"
#include "ArgsLazyData.h"
#include "R/BuiltinIds.h"
#include "R/Funtab.h"
#include "interp.h"
#include <algorithm>
#include <stdlib.h>

namespace rir {

struct bitShiftL {
    int operator()(int lhs, int rhs) const { return lhs << rhs; }
};

struct bitShiftR {
    int operator()(int lhs, int rhs) const { return lhs >> rhs; }
};

template <class Func>
static RIR_INLINE SEXP bitwiseOp(Func operation, SEXP lhs, SEXP rhs,
                                 bool testLimits) {
    if (TYPEOF(lhs) != REALSXP && TYPEOF(lhs) != INTSXP)
        return nullptr;
    if (TYPEOF(rhs) != REALSXP && TYPEOF(rhs) != INTSXP)
        return nullptr;

    R_xlen_t lhsLength = XLENGTH(lhs), rhsLength = XLENGTH(rhs);
    R_xlen_t resultLength;
    if (lhsLength && rhsLength)
        resultLength = (lhsLength >= rhsLength) ? lhsLength : rhsLength;
    else
        resultLength = 0;
    SEXP res = allocVector(INTSXP, resultLength);
    int* resValues = INTEGER(res);

    R_xlen_t iLhs = 0, iRhs = 0;
    for (R_xlen_t i = 0; i < resultLength;
         iLhs = (++iLhs == lhsLength) ? 0 : iLhs,
                  iRhs = (++iRhs == rhsLength) ? 0 : iRhs, ++i) {

        int currentValLeft =
            TYPEOF(lhs) == INTSXP ? INTEGER(lhs)[iLhs] : REAL(lhs)[iLhs];
        int currentValRight =
            TYPEOF(rhs) == INTSXP ? INTEGER(rhs)[iRhs] : REAL(rhs)[iRhs];
        bool guard =
            currentValLeft == NA_INTEGER || currentValRight == NA_INTEGER;
        if (testLimits)
            guard = guard || currentValLeft < 0 || currentValRight > 31;
        resValues[i] =
            guard ? NA_INTEGER : operation(currentValLeft, currentValRight);
    }
    return res;
}

R_xlen_t asVecSize(SEXP x) {
    if (isVectorAtomic(x) && LENGTH(x) >= 1) {
        switch (TYPEOF(x)) {
        case INTSXP: {
            int res = INTEGER(x)[0];
            if (res == NA_INTEGER)
                break;
            return (R_xlen_t)res;
        }
        case REALSXP: {
            double d = REAL(x)[0];
            if (ISNAN(d))
                break;
            if (!R_FINITE(d))
                break;
            if (d > R_XLEN_T_MAX)
                break;
            return (R_xlen_t)d;
        }
        case STRSXP: {
            double d = asReal(x);
            if (ISNAN(d))
                break;
            if (!R_FINITE(d))
                break;
            if (d > R_XLEN_T_MAX)
                break;
            return (R_xlen_t)d;
        }
        default:
            break;
        }
    }
    return -999; /* which gives error in the caller */
}

SEXP tryFastSpecialCall(const CallContext& call, InterpreterInstance* ctx) {
    SLOWASSERT(!call.hasNames());
    return nullptr;
}

SEXP tryFastBuiltinCall(const CallContext& call, InterpreterInstance* ctx) {
    SLOWASSERT(!call.hasNames());

    static constexpr size_t MAXARGS = 16;
    std::array<SEXP, MAXARGS> args;
    auto nargs = call.suppliedArgs;

    if (nargs > MAXARGS)
        return nullptr;

    for (size_t i = 0; i < call.suppliedArgs; ++i) {
        auto arg = call.stackArg(i);
        if (TYPEOF(arg) == PROMSXP)
            arg = PRVALUE(arg);
        if (arg == R_UnboundValue || arg == R_MissingArg ||
            ATTRIB(arg) != R_NilValue)
            return nullptr;
        args[i] = arg;
    }

    switch (call.callee->u.primsxp.offset) {
    case blt("nargs"): {
        if (nargs != 0)
            return nullptr;

        int nargs = -1;
        for (RCNTXT* cptr = R_GlobalContext; cptr != NULL;
             cptr = cptr->nextcontext) {
            if ((cptr->callflag & CTXT_FUNCTION) &&
                cptr->cloenv == call.callerEnv) {
                if (auto l = ArgsLazyDataContent::check(cptr->promargs)) {
                    nargs = l->length;
                    break;
                }
                nargs = Rf_length(cptr->promargs);
                break;
            }
        }
        return ScalarInteger(nargs);
    }

    case blt("length"): {
        if (nargs != 1)
            return nullptr;

        switch (TYPEOF(args[0])) {
        case INTSXP:
        case REALSXP:
        case LGLSXP:
            return Rf_ScalarInteger(XLENGTH(args[0]));
        default:
            return nullptr;
        }
        assert(false);
    }

    case blt("c"): {
        if (nargs == 0)
            return R_NilValue;

        auto type = TYPEOF(args[0]);
        if (type != REALSXP && type != LGLSXP && type != INTSXP)
            return nullptr;
        long total = XLENGTH(args[0]);
        for (size_t i = 1; i < nargs; ++i) {
            auto thistype = TYPEOF(args[i]);
            if (thistype != REALSXP && thistype != LGLSXP && thistype != INTSXP)
                return nullptr;

            if (thistype == INTSXP && type == LGLSXP)
                type = INTSXP;

            if (thistype == REALSXP && type != REALSXP)
                type = REALSXP;

            total += XLENGTH(args[i]);
        }

        if (total == 0)
            return nullptr;

        long pos = 0;
        auto res = Rf_allocVector(type, total);
        for (size_t i = 0; i < nargs; ++i) {
            auto len = XLENGTH(args[i]);
            for (long j = 0; j < len; ++j) {
                assert(pos < total);
                // We handle LGL and INT in the same case here. That is
                // fine, because they are essentially the same type.
                SLOWASSERT(NA_INTEGER == NA_LOGICAL);
                if (type == REALSXP) {
                    if (TYPEOF(args[i]) == REALSXP) {
                        REAL(res)[pos++] = REAL(args[i])[j];
                    } else {
                        if (INTEGER(args[i])[j] == NA_INTEGER) {
                            REAL(res)[pos++] = NA_REAL;
                        } else {
                            REAL(res)[pos++] = INTEGER(args[i])[j];
                        }
                    }
                } else {
                    INTEGER(res)[pos++] = INTEGER(args[i])[j];
                }
            }
        }
        return res;
    }

    case blt("vector"): {
        if (nargs != 2)
            return nullptr;
        if (TYPEOF(args[0]) != STRSXP)
            return nullptr;
        if (XLENGTH(args[0]) != 1)
            return nullptr;
        if (Rf_length(args[1]) != 1)
            return nullptr;
        auto length = asVecSize(args[1]);
        if (length < 0)
            return nullptr;
        int type = str2type(CHAR(VECTOR_ELT(args[0], 0)));

        switch (type) {
        case LGLSXP:
        case INTSXP: {
            auto res = allocVector(type, length);
            Memzero(INTEGER(res), length);
            return res;
        }
        case CPLXSXP: {
            auto res = allocVector(type, length);
            Memzero(COMPLEX(res), length);
            return res;
        }
        case RAWSXP: {
            auto res = allocVector(type, length);
            Memzero(RAW(res), length);
            return res;
        }
        case REALSXP: {
            auto res = allocVector(type, length);
            Memzero(REAL(res), length);
            return res;
        }
        case STRSXP:
        case EXPRSXP:
        case VECSXP:
            return allocVector(type, length);
        case LISTSXP:
            if (length > INT_MAX)
                return nullptr;
            return allocList((int)length);
        default:
            return nullptr;
        }
        assert(false);
        break;
    }

    case blt("which"): {
        if (nargs != 1)
            return nullptr;
        auto arg = args[0];
        if (TYPEOF(arg) != LGLSXP)
            return nullptr;
        std::vector<size_t> which;
        for (long i = 0; i < XLENGTH(arg); ++i) {
            if (LOGICAL(arg)[i] == TRUE) {
                which.push_back(i);
            }
        }
        auto res = Rf_allocVector(INTSXP, which.size());
        size_t pos = 0;
        for (auto i : which)
            INTEGER(res)[pos++] = i + 1;
        return res;
    }

    case blt("abs"): {
        if (nargs != 1)
            return nullptr;
        auto x = args[0];
        SEXP s = R_NilValue;
        if (isInteger(x) || isLogical(x)) {
            /* integer or logical ==> return integer,
               factor was covered by Math.factor. */
            R_xlen_t i, n = XLENGTH(x);
            s = (NO_REFERENCES(x) && TYPEOF(x) == INTSXP)
                    ? x
                    : allocVector(INTSXP, n);
            /* Note: relying on INTEGER(.) === LOGICAL(.) : */
            int* pa = INTEGER(s);
            const int* px = INTEGER_RO(x);
            for (i = 0; i < n; i++) {
                int xi = px[i];
                pa[i] = (xi == NA_INTEGER) ? xi : abs(xi);
            }
            return s;
        } else if (TYPEOF(x) == REALSXP) {
            R_xlen_t i, n = XLENGTH(x);
            s = NO_REFERENCES(x) ? x : allocVector(REALSXP, n);
            double* pa = REAL(s);
            const double* px = REAL_RO(x);
            for (i = 0; i < n; i++)
                pa[i] = fabs(px[i]);
            return s;
        } else if (isComplex(x)) {
            const Rcomplex* px = COMPLEX_RO(x);
            R_xlen_t i, n = XLENGTH(x);

            auto y = allocVector(REALSXP, n);
            double* py = REAL(y);
            for (i = 0; i < n; i++)
                py[0] = hypot(px[0].r, px[0].i);
            return y;
        }
        return nullptr;
    }

    case blt("min"):
    case blt("max"): {
        if (nargs != 2)
            return nullptr;

        auto a = args[0];
        auto b = args[1];

        if (XLENGTH(a) != 1 || XLENGTH(b) != 1)
            return nullptr;

        auto combination = (TYPEOF(args[0]) << 8) + TYPEOF(args[1]);

#define CMP(a, b) ((call.callee->u.primsxp.offset == 301) ? a < b : b < a)

        switch (combination) {
        case (INTSXP << 8) + INTSXP:
            if (*INTEGER(a) == NA_INTEGER || *INTEGER(b) == NA_INTEGER)
                return nullptr;
            return CMP(*INTEGER(a), *INTEGER(b)) ? a : b;

        case (INTSXP << 8) + REALSXP:
            if (ISNAN(*REAL(b)))
                return b;
            if (*INTEGER(a) == NA_INTEGER)
                return ScalarReal(NA_REAL);
            return CMP(*INTEGER(a), *REAL(b)) ? ScalarReal(*INTEGER(a)) : b;

        case (REALSXP << 8) + INTSXP:
            if (ISNAN(*REAL(a)))
                return a;
            if (*INTEGER(b) == NA_INTEGER)
                return ScalarReal(NA_REAL);
            return CMP(*REAL(a), *INTEGER(b)) ? a : ScalarReal(*INTEGER(b));

        case (REALSXP << 8) + REALSXP:
            if (ISNAN(*REAL(a)) || ISNAN(*REAL(b)))
                return a;
            return CMP(*REAL(a), *REAL(b)) ? a : b;

        default:
            return nullptr;
        }

#undef CMP
    }

    case blt("as.character"): {
        if (nargs != 1)
            return nullptr;
        if (TYPEOF(args[0]) == STRSXP)
            return args[0];
        if (IS_SIMPLE_SCALAR(args[0], INTSXP)) {
            auto i = INTEGER(args[0])[0];
            if (i >= 0 && i < 1000) {
                static std::array<SEXP, 1000> stringCache{};
                if (!stringCache[i]) {
                    char buf[6];
                    sprintf(buf, "%d", i);
                    auto r = mkChar(buf);
                    auto res = allocVector(STRSXP, 1);
                    SET_STRING_ELT(res, 0, r);
                    R_PreserveObject(res);
                    ENSURE_NAMEDMAX(res);
                    stringCache[i] = res;
                }
                return stringCache[i];
            }
            if (i > -10000 && i < 100000) {
                char buf[6];
                sprintf(buf, "%d", i);
                auto r = mkChar(buf);
                auto res = allocVector(STRSXP, 1);
                SET_STRING_ELT(res, 0, r);
                return res;
            }
        }
        break;
    }

    case blt("as.integer"): {
        if (nargs != 1)
            return nullptr;
        if (TYPEOF(args[0]) == INTSXP)
            return args[0];
        break;
    }

    case blt("stdin"):
    case blt("stdout"):
    case blt("stderr"): {
        if (nargs != 0)
            return nullptr;
        auto f = getBuiltin(call.callee);
        return f(R_NilValue, call.callee, R_NilValue, R_NilValue);
    }

    case blt("is.logical"): {
        if (nargs != 1)
            return nullptr;
        return TYPEOF(args[0]) == LGLSXP ? R_TrueValue : R_FalseValue;
    }

    case blt("is.symbol"): {
        if (nargs != 1)
            return nullptr;
        return TYPEOF(args[0]) == SYMSXP ? R_TrueValue : R_FalseValue;
    }

    case blt("is.expression"): {
        if (nargs != 1)
            return nullptr;
        return TYPEOF(args[0]) == EXPRSXP ? R_TrueValue : R_FalseValue;
    }

    case blt("is.object"): {
        if (nargs != 1)
            return nullptr;
        return OBJECT(args[0]) ? R_TrueValue : R_FalseValue;
    }

    case blt("is.numeric"): {
        if (nargs != 1)
            return nullptr;
        return isNumeric(args[0]) && !isLogical(args[0]) ? R_TrueValue
                                                         : R_FalseValue;
    }

    case blt("is.matrix"): {
        if (nargs != 1)
            return nullptr;
        return isMatrix(args[0]) ? R_TrueValue : R_FalseValue;
    }

    case blt("is.array"): {
        if (nargs != 1)
            return nullptr;
        return isArray(args[0]) ? R_TrueValue : R_FalseValue;
    }

    case blt("is.atomic"): {
        if (nargs != 1)
            return nullptr;
        switch (TYPEOF(args[0])) {
        case NILSXP:
        /* NULL is atomic (S compatibly), but not in isVectorAtomic(.) */
        case CHARSXP:
        case LGLSXP:
        case INTSXP:
        case REALSXP:
        case CPLXSXP:
        case STRSXP:
        case RAWSXP:
            return R_TrueValue;
        default:
            return R_FalseValue;
        }
        assert(false);
    }

    case blt("is.call"): {
        if (nargs != 1)
            return nullptr;
        return TYPEOF(args[0]) == LANGSXP ? R_TrueValue : R_FalseValue;
    }

    case blt("is.function"): {
        if (nargs != 1)
            return nullptr;
        return isFunction(args[0]) ? R_TrueValue : R_FalseValue;
    }

    case blt("is.na"): {
        if (nargs != 1)
            return nullptr;

        auto arg = args[0];
        if (XLENGTH(arg) != 1)
            return nullptr;

        switch (TYPEOF(arg)) {
        case INTSXP:
            return INTEGER(arg)[0] == NA_INTEGER ? R_TrueValue : R_FalseValue;
        case LGLSXP:
            return LOGICAL(arg)[0] == NA_LOGICAL ? R_TrueValue : R_FalseValue;
        case REALSXP:
            return ISNAN(REAL(arg)[0]) ? R_TrueValue : R_FalseValue;
        default:
            return nullptr;
        }
        assert(false);
        break;
    }

    case blt("is.vector"): {
        if (nargs != 1)
            return nullptr;

        auto arg = args[0];
        if (XLENGTH(arg) != 1)
            return nullptr;
        return TYPEOF(arg) == VECSXP ? R_TrueValue : R_FalseValue;
    }

    case blt("list"): {
        // "lists" at the R level are VECSXP's in the implementation
        auto res = Rf_allocVector(VECSXP, nargs);
        for (size_t i = 0; i < nargs; ++i)
            SET_VECTOR_ELT(res, i, args[i]);
        return res;
    }

    case blt("rep.int"): {
        if (nargs != 2)
            return nullptr;

        auto theTimes = args[1];
        if (TYPEOF(theTimes) != INTSXP || XLENGTH(theTimes) != 1)
            return nullptr;

        auto times = INTEGER(theTimes)[0];

        auto x = args[0];
        if (TYPEOF(x) != INTSXP && TYPEOF(x) != REALSXP)
            return nullptr;

        auto len = XLENGTH(x);

        auto res = Rf_allocVector(TYPEOF(x), len * times);

        switch (TYPEOF(x)) {

        case INTSXP:
            for (long i = 0; i < times; ++i)
                for (long j = 0; j < len; ++j)
                    INTEGER(res)[i * len + j] = INTEGER(x)[j];
            break;

        case REALSXP:
            for (long i = 0; i < times; ++i)
                for (long j = 0; j < len; ++j)
                    REAL(res)[i * len + j] = REAL(x)[j];
            break;

        default:
            assert(false);
        }

        return res;
    }

    case blt("islistfactor"): {
        if (nargs != 2)
            return nullptr;
        auto n = XLENGTH(args[0]);
        if (n == 0 || !isVectorList(args[0]))
            return R_FalseValue;
        int recursive = asLogical(args[1]);
        if (recursive)
            return nullptr;

        for (int i = 0; i < n; i++)
            if (!isFactor(VECTOR_ELT(args[0], i)))
                return R_FalseValue;

        return R_TrueValue;
    }

    case blt("bitwiseAnd"): {
        if (nargs != 2)
            return nullptr;
        return bitwiseOp(std::bit_and<int>(), args[0], args[1], false);
    }

    case blt("bitwiseOr"): {
        if (nargs != 2)
            return nullptr;
        return bitwiseOp(std::bit_or<int>(), args[0], args[1], false);
    }

    case blt("bitwiseXor"): {
        if (nargs != 2)
            return nullptr;
        return bitwiseOp(std::bit_xor<int>(), args[0], args[1], false);
    }

    case blt("bitwiseShiftL"): {
        if (nargs != 2)
            return nullptr;
        return bitwiseOp(bitShiftL(), args[0], args[1], false);
    }

    case blt("bitwiseShiftR"): {
        if (nargs != 2)
            return nullptr;
        return bitwiseOp(bitShiftR(), args[0], args[1], false);
    }
    }
    return nullptr;
}

bool supportsFastBuiltinCall(SEXP b) {
    switch (b->u.primsxp.offset) {
    case blt("nargs"):
    case blt("length"):
    case blt("c"):
    case blt("vector"):
    case blt("which"):
    case blt("abs"):
    case blt("min"):
    case blt("max"):
    case blt("as.character"):
    case blt("as.integer"):
    case blt("stdin"):
    case blt("stdout"):
    case blt("stderr"):
    case blt("is.logical"):
    case blt("is.symbol"):
    case blt("is.expression"):
    case blt("is.object"):
    case blt("is.numeric"):
    case blt("is.matrix"):
    case blt("is.array"):
    case blt("is.atomic"):
    case blt("is.call"):
    case blt("is.function"):
    case blt("is.na"):
    case blt("is.vector"):
    case blt("list"):
    case blt("rep.int"):
    case blt("islistfactor"):
    case blt("bitwiseAnd"):
    case blt("bitwiseOr"):
    case blt("bitwiseXor"):
    case blt("bitwiseShiftL"):
    case blt("bitwiseShiftR"):
        return true;
    default: {}
    }
    return false;
}

} // namespace rir
