#include "builtins.h"
#include "interp.h"
#include <algorithm>

namespace rir {

struct bitShiftL {
    int operator()(int lhs, int rhs) const { return lhs << rhs; }
};

struct bitShiftR {
    int operator()(int lhs, int rhs) const { return lhs >> rhs; }
};

R_xlen_t asVecSize(R_bcstack_t* stackCell) {
    switch (stackCell->tag) {
    case STACK_OBJ_INT:
        return (R_xlen_t)stackCell->u.ival;
    case STACK_OBJ_REAL: {
        double d = stackCell->u.dval;
        if (ISNAN(d))
            break;
        if (!R_FINITE(d))
            break;
        if (d > R_XLEN_T_MAX)
            break;
        return (R_xlen_t)d;
    }
    case STACK_OBJ_LOGICAL:
        break;
    case STACK_OBJ_SEXP: {
        SEXP sexp = stackObjToSexp(stackCell);
        if (isVectorAtomic(sexp) && XLENGTH(sexp) >= 1) {
            switch (TYPEOF(sexp)) {
            case INTSXP: {
                int res = INTEGER(sexp)[0];
                if (res == NA_INTEGER)
                    break;
                return (R_xlen_t)res;
            }
            case REALSXP: {
                double d = REAL(sexp)[0];
                if (ISNAN(d))
                    break;
                if (!R_FINITE(d))
                    break;
                if (d > R_XLEN_T_MAX)
                    break;
                return (R_xlen_t)d;
            }
            case STRSXP: {
                double d = asReal(sexp);
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
        break;
    }
    default:
        assert(false);
    }
    return -999; /* which gives error in the caller */
}

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

SEXP tryFastSpecialCall(const CallContext& call, InterpreterInstance* ctx) {
    SLOWASSERT(call.hasStackArgs() && !call.hasNames());
    return nullptr;
}

SEXP tryFastBuiltinCall(const CallContext& call, InterpreterInstance* ctx) {
    SLOWASSERT(call.hasStackArgs() && !call.hasNames());
    static constexpr size_t MAXARGS = 16;
    std::array<R_bcstack_t, MAXARGS> args;
    auto nargs = call.suppliedArgs;

    if (nargs > MAXARGS)
        return nullptr;

    for (size_t i = 0; i < call.suppliedArgs; ++i) {
        auto arg = *call.stackArg(i);
        if (arg.tag == STACK_OBJ_SEXP && TYPEOF(arg.u.sxpval) == PROMSXP)
            arg.u.sxpval = PRVALUE(arg.u.sxpval);
        if (arg.tag == STACK_OBJ_SEXP &&
            (arg.u.sxpval == R_UnboundValue || arg.u.sxpval == R_MissingArg ||
             ATTRIB(arg.u.sxpval) != R_NilValue))
            return nullptr;
        args[i] = arg;
    }

    switch (call.callee->u.primsxp.offset) {
    case 90: { // "c"
        if (nargs == 0)
            return R_NilValue;

        auto type = stackObjSexpType(&args[0]);
        if (type != REALSXP && type != LGLSXP && type != INTSXP)
            return nullptr;
        long total = stackObjLength(&args[0]);
        for (size_t i = 1; i < nargs; ++i) {
            auto thistype = stackObjSexpType(&args[i]);
            if (thistype != REALSXP && thistype != LGLSXP && thistype != INTSXP)
                return nullptr;

            if (thistype == INTSXP && type == LGLSXP)
                type = INTSXP;

            if (thistype == REALSXP && type != REALSXP)
                type = REALSXP;

            total += stackObjLength(&args[i]);
        }

        if (total == 0)
            return nullptr;

        long pos = 0;
        auto res = Rf_allocVector(type, total);
        for (size_t i = 0; i < nargs; ++i) {
            auto len = stackObjLength(&args[i]);
            for (long j = 0; j < len; ++j) {
                assert(pos < total);
                // We handle LGL and INT in the same case here. That is fine,
                // because they are essentially the same type.
                SLOWASSERT(NA_INTEGER == NA_LOGICAL);
                if (type == REALSXP) {
                    if (stackObjSexpType(&args[i]) == REALSXP) {
                        REAL(res)
                        [pos++] = args[i].tag == STACK_OBJ_REAL
                                      ? args[i].u.dval
                                      : REAL(args[i].u.sxpval)[j];
                    } else {
                        int argInt = (args[i].tag == STACK_OBJ_INT ||
                                      args[i].tag == STACK_OBJ_LOGICAL)
                                         ? args[i].u.ival
                                         : INTEGER(args[i].u.sxpval)[j];
                        if (argInt == NA_INTEGER) {
                            REAL(res)[pos++] = NA_REAL;
                        } else {
                            REAL(res)[pos++] = (double)argInt;
                        }
                    }
                } else {
                    INTEGER(res)
                    [pos++] = (args[i].tag == STACK_OBJ_INT ||
                               args[i].tag == STACK_OBJ_LOGICAL)
                                  ? args[i].u.ival
                                  : INTEGER(args[i].u.sxpval)[j];
                }
            }
        }
        return res;
    }

    case 109: { // "vector"
        if (nargs != 2)
            return nullptr;
        if (stackObjSexpType(&args[0]) != STRSXP)
            return nullptr;
        if (stackObjLength(&args[0]) != 1)
            return nullptr;
        // TODO: What about Rf_length check?
        auto length = asVecSize(&args[1]);
        if (length < 0)
            return nullptr;
        int type = str2type(CHAR(VECTOR_ELT(args[0].u.sxpval, 0)));

        switch (type) {
        case LGLSXP: {
            auto res = allocVector(type, length);
            Memzero(LOGICAL(res), length);
            return res;
        }
        case INTSXP: {
            auto res = allocVector(type, length);
            Memzero(INTEGER(res), length);
            return res;
        }
        case REALSXP: {
            auto res = allocVector(type, length);
            Memzero(REAL(res), length);
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
    }

    case 412: { // "list"
        // "lists" at the R level are VECSXP's in the implementation
        auto res = Rf_allocVector(VECSXP, nargs);
        for (size_t i = 0; i < nargs; ++i)
            SET_VECTOR_ELT(res, i, stackObjToSexp(&args[i]));
        return res;
    }

    case 399: { // "is.vector"
        if (nargs != 1)
            return nullptr;

        auto arg = &args[0];
        if (stackObjLength(arg) != 1)
            return nullptr;
        return stackObjSexpType(arg) == VECSXP ? R_TrueValue : R_FalseValue;
    }

    case 395: { // "is.na"
        if (nargs != 1)
            return nullptr;

        auto arg = &args[0];
        if (stackObjLength(arg) != 1)
            return nullptr;

        switch (stackObjSexpType(arg)) {
        case INTSXP:
            return tryStackObjToInteger(arg) == NA_INTEGER ? R_TrueValue
                                                           : R_FalseValue;
        case LGLSXP:
            return tryStackObjToLogical(arg) == NA_LOGICAL ? R_TrueValue
                                                           : R_FalseValue;
        case REALSXP:
            return ISNAN(tryStackObjToReal(arg)) ? R_TrueValue : R_FalseValue;
        default:
            return nullptr;
        }
        assert(false);
    }

    case 393: { // "is.function"
        if (nargs != 1)
            return nullptr;
        return (args[0].tag == STACK_OBJ_SEXP && isFunction(args[0].u.sxpval))
                   ? R_TrueValue
                   : R_FalseValue;
    }

    case 507: { // "islistfactor"
        if (nargs != 2)
            return nullptr;
        if (args[0].tag != STACK_OBJ_SEXP)
            return R_FalseValue;
        auto arg0 = args[0].u.sxpval;

        auto n = XLENGTH(arg0);
        if (n == 0 || !isVectorList(arg0))
            return R_FalseValue;

        // TODO: This replaces asLogical, is that OK?
        int recursive = tryStackObjToLogicalNa(&args[1]);
        if (recursive)
            return nullptr;

        for (int i = 0; i < n; i++)
            if (!isFactor(VECTOR_ELT(arg0, i)))
                return R_FalseValue;

        return R_TrueValue;
    }

    case 88: { // "length"
        if (nargs != 1)
            return nullptr;

        switch (stackObjSexpType(&args[0])) {
        case INTSXP:
        case REALSXP:
        case LGLSXP:
            return Rf_ScalarInteger(stackObjLength(&args[0]));
        default:
            return nullptr;
        }
        assert(false);
    }

    case 386: { // "is.numeric"
        if (nargs != 1)
            return nullptr;
        switch (args[0].tag) {
        case STACK_OBJ_INT:
        case STACK_OBJ_REAL:
            return R_TrueValue;
        case STACK_OBJ_LOGICAL:
            return R_FalseValue;
        case STACK_OBJ_SEXP:
            return (isNumeric(args[0].u.sxpval) && !isLogical(args[0].u.sxpval))
                       ? R_TrueValue
                       : R_FalseValue;
        default:
            assert(false);
        }
    }

    case 387: { // "is.matrix"
        if (nargs != 1)
            return nullptr;
        return (args[0].tag == STACK_OBJ_SEXP && isMatrix(args[0].u.sxpval))
                   ? R_TrueValue
                   : R_FalseValue;
    }

    case 388: { // "is.array"
        if (nargs != 1)
            return nullptr;
        return (args[0].tag == STACK_OBJ_SEXP && isArray(args[0].u.sxpval))
                   ? R_TrueValue
                   : R_FalseValue;
    }

    case 389: { // "is.atomic"
        if (nargs != 1)
            return nullptr;
        switch (stackObjSexpType(&args[0])) {
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

    case 384: { // "is.object"
        if (nargs != 1)
            return nullptr;
        return (args[0].tag == STACK_OBJ_SEXP && OBJECT(args[0].u.sxpval))
                   ? R_TrueValue
                   : R_FalseValue;
    }

    case 136: { // "which"
        if (nargs != 1)
            return nullptr;
        auto arg = &args[0];
        if (stackObjSexpType(arg) != LGLSXP)
            return nullptr;
        std::vector<size_t> which;
        switch (arg->tag) {
        case STACK_OBJ_LOGICAL:
            // TODO: Is R_NilValue NA?
            if ((bool)arg->u.ival)
                which.push_back(1);
            else
                return R_NilValue;
        case STACK_OBJ_SEXP: {
            auto argSexp = arg->u.sxpval;
            std::vector<size_t> which;
            for (long i = 0; i < XLENGTH(argSexp); ++i) {
                if (LOGICAL(argSexp)[i] == TRUE) {
                    which.push_back(i);
                }
            }
        }
        default:
            assert(false);
        }
        auto res = Rf_allocVector(INTSXP, which.size());
        size_t pos = 0;
        for (auto i : which)
            INTEGER(res)[pos++] = i + 1;
        return res;
    }

    case 407: { // "rep.int"
        if (nargs != 2)
            return nullptr;

        auto times = tryStackObjToInteger(&args[1]);
        if (times == NA_INTEGER)
            return nullptr;

        auto x = &args[0];
        if (stackObjSexpType(x) != INTSXP && stackObjSexpType(x) != REALSXP)
            return nullptr;

        auto len = stackObjLength(x);

        auto res = Rf_allocVector(stackObjSexpType(x), len * times);

        switch (stackObjSexpType(x)) {

        case INTSXP:
            for (long i = 0; i < times; ++i) {
                if (x->tag == STACK_OBJ_INT) {
                    SLOWASSERT(len == 1);
                    INTEGER(res)[i] = x->u.ival;
                } else {
                    SLOWASSERT(x->tag == STACK_OBJ_SEXP);
                    for (long j = 0; j < len; ++j)
                        INTEGER(res)[i * len + j] = INTEGER(x->u.sxpval)[j];
                }
            }
            break;

        case REALSXP:
            for (long i = 0; i < times; ++i) {
                if (x->tag == STACK_OBJ_REAL) {
                    SLOWASSERT(len == 1);
                    REAL(res)[i] = x->u.dval;
                } else {
                    SLOWASSERT(x->tag == STACK_OBJ_SEXP);
                    for (long j = 0; j < len; ++j)
                        REAL(res)[i * len + j] = REAL(x->u.sxpval)[j];
                }
            }
            break;

        default:
            assert(false);
        }

        return res;
    }

    case 301: { // "min"
        if (nargs != 2)
            return nullptr;

        auto a = &args[0];
        auto b = &args[1];

        if (stackObjLength(a) != 1 || stackObjLength(b) != 1)
            return nullptr;

        auto combination =
            (stackObjSexpType(&args[0]) << 8) + stackObjSexpType(&args[1]);

        switch (combination) {
        case (INTSXP << 8) + INTSXP:
            if (tryStackObjToInteger(a) == NA_INTEGER ||
                tryStackObjToInteger(b) == NA_INTEGER)
                return nullptr;
            return tryStackObjToInteger(a) < tryStackObjToInteger(b)
                       ? stackObjToSexp(a)
                       : stackObjToSexp(b);

        case (INTSXP << 8) + REALSXP:
            if (tryStackObjToInteger(a) == NA_INTEGER ||
                ISNAN(tryStackObjToReal(b)))
                return nullptr;
            return tryStackObjToInteger(a) < tryStackObjToReal(b)
                       ? stackObjToSexp(a)
                       : stackObjToSexp(b);

        case (REALSXP << 8) + INTSXP:
            if (tryStackObjToInteger(b) == NA_INTEGER ||
                ISNAN(tryStackObjToReal(a)))
                return nullptr;
            return tryStackObjToReal(a) < tryStackObjToInteger(b)
                       ? stackObjToSexp(a)
                       : stackObjToSexp(b);

        case (REALSXP << 8) + REALSXP:
            if (ISNAN(tryStackObjToReal(a)) || ISNAN(tryStackObjToReal(b)))
                return nullptr;
            return tryStackObjToReal(a) < tryStackObjToReal(b)
                       ? stackObjToSexp(a)
                       : stackObjToSexp(b);

        default:
            return nullptr;
        }
    }

    case 678: { // "bitwiseAnd"
        if (nargs != 2)
            return nullptr;
        return bitwiseOp(std::bit_and<int>(), stackObjToSexp(&args[0]),
                         stackObjToSexp(&args[1]), false);
    }

    case 680: { // "bitwiseOr"
        if (nargs != 2)
            return nullptr;
        return bitwiseOp(std::bit_or<int>(), stackObjToSexp(&args[0]),
                         stackObjToSexp(&args[1]), false);
    }

    case 681: { // "bitwiseXor"
        if (nargs != 2)
            return nullptr;
        return bitwiseOp(std::bit_xor<int>(), stackObjToSexp(&args[0]),
                         stackObjToSexp(&args[1]), false);
    }

    case 682: { // "bitwiseShiftL"
        if (nargs != 2)
            return nullptr;
        return bitwiseOp(bitShiftL(), stackObjToSexp(&args[0]),
                         stackObjToSexp(&args[1]), false);
    }

    case 683: { // "bitwiseShiftL"
        if (nargs != 2)
            return nullptr;
        return bitwiseOp(bitShiftR(), stackObjToSexp(&args[0]),
                         stackObjToSexp(&args[1]), false);
    }
    }
    return nullptr;
}

} // namespace rir
