#include "builtins.h"
#include "interp.h"

namespace rir {

R_xlen_t asVecSize(R_bcstack_t x) {
    switch (x.tag) {
    case STACK_OBJ_INT:
        return (R_xlen_t)x.u.ival;
    case STACK_OBJ_REAL: {
        double d = x.u.dval;
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
        SEXP sexp = stackObjToSexp(x);
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

R_bcstack_t tryFastSpecialCall(const CallContext& call,
                               InterpreterInstance* ctx) {
    SLOWASSERT(call.hasStackArgs() && !call.hasNames());
    return nullStackObj;
}

R_bcstack_t tryFastBuiltinCall(const CallContext& call,
                               InterpreterInstance* ctx) {
    SLOWASSERT(call.hasStackArgs() && !call.hasNames());

    static constexpr size_t MAXARGS = 16;
    std::array<R_bcstack_t, MAXARGS> args;
    auto nargs = call.suppliedArgs;

    if (nargs > MAXARGS)
        return nullStackObj;

    for (size_t i = 0; i < call.suppliedArgs; ++i) {
        auto arg = call.stackArg(i, ctx);
        if (arg.tag == STACK_OBJ_SEXP && TYPEOF(arg.u.sxpval) == PROMSXP)
            arg = sexpToStackObj(PRVALUE(arg.u.sxpval));
        if (arg.tag == STACK_OBJ_SEXP &&
            (arg.u.sxpval == R_UnboundValue || arg.u.sxpval == R_MissingArg ||
             ATTRIB(arg.u.sxpval) != R_NilValue))
            return nullStackObj;
        args[i] = arg;
    }

    switch (call.callee->u.primsxp.offset) {
    case 90: { // "c"
        if (nargs == 0)
            return sexpToStackObj(R_NilValue);

        auto type = stackObjSexpType(args[0]);
        if (type != REALSXP && type != LGLSXP && type != INTSXP)
            return nullStackObj;
        long total = stackObjLength(args[0]);
        for (size_t i = 1; i < nargs; ++i) {
            auto thistype = stackObjSexpType(args[i]);
            if (thistype != REALSXP && thistype != LGLSXP && thistype != INTSXP)
                return nullStackObj;

            if (thistype == INTSXP && type == LGLSXP)
                type = INTSXP;

            if (thistype == REALSXP && type != REALSXP)
                type = REALSXP;

            total += stackObjLength(args[i]);
        }

        if (total == 0)
            return nullStackObj;

        long pos = 0;
        auto res = Rf_allocVector(type, total);
        for (size_t i = 0; i < nargs; ++i) {
            auto len = stackObjLength(args[i]);
            for (long j = 0; j < len; ++j) {
                assert(pos < total);
                // We handle LGL and INT in the same case here. That is fine,
                // because they are essentially the same type.
                SLOWASSERT(NA_INTEGER == NA_LOGICAL);
                if (type == REALSXP) {
                    if (stackObjSexpType(args[i]) == REALSXP) {
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
        return sexpToStackObj(res);
    }

    case 109: { // "vector"
        if (nargs != 2)
            return nullStackObj;
        if (stackObjSexpType(args[0]) != STRSXP)
            return nullStackObj;
        if (stackObjLength(args[0]) != 1)
            return nullStackObj;
        // TODO: What about Rf_length check?
        auto length = asVecSize(args[1]);
        if (length < 0)
            return nullStackObj;
        int type = str2type(CHAR(VECTOR_ELT(args[0].u.sxpval, 0)));

        switch (type) {
        case LGLSXP: {
            if (length == 1) {
                return logicalStackObj(0);
            } else {
                auto res = allocVector(type, length);
                Memzero(LOGICAL(res), length);
                return sexpToStackObj(res);
            }
        }
        case INTSXP: {
            if (length == 1) {
                return intStackObj(0);
            } else {
                auto res = allocVector(type, length);
                Memzero(INTEGER(res), length);
                return sexpToStackObj(res);
            }
        }
        case REALSXP: {
            if (length == 1) {
                return realStackObj(0);
            } else {
                auto res = allocVector(type, length);
                Memzero(REAL(res), length);
                return sexpToStackObj(res);
            }
        }
        case CPLXSXP: {
            auto res = allocVector(type, length);
            Memzero(COMPLEX(res), length);
            return sexpToStackObj(res);
        }
        case RAWSXP: {
            auto res = allocVector(type, length);
            Memzero(RAW(res), length);
            return sexpToStackObj(res);
        }
        case STRSXP:
        case EXPRSXP:
        case VECSXP:
            return sexpToStackObj(allocVector(type, length));
        case LISTSXP:
            if (length > INT_MAX)
                return nullStackObj;
            return sexpToStackObj(allocList((int)length));
        default:
            return nullStackObj;
        }
        assert(false);
    }

    case 412: { // "list"
        // "lists" at the R level are VECSXP's in the implementation
        auto res = Rf_allocVector(VECSXP, nargs);
        for (size_t i = 0; i < nargs; ++i)
            SET_VECTOR_ELT(res, i, stackObjToSexp(args[i]));
        return sexpToStackObj(res);
    }

    case 399: { // "is.vector"
        if (nargs != 1)
            return nullStackObj;

        auto arg = args[0];
        if (stackObjLength(arg) != 1)
            return nullStackObj;
        return logicalStackObj(stackObjSexpType(arg) == VECSXP);
    }

    case 395: { // "is.na"
        if (nargs != 1)
            return nullStackObj;

        auto arg = args[0];
        if (stackObjLength(arg) != 1)
            return nullStackObj;

        switch (stackObjSexpType(arg)) {
        case INTSXP:
            return logicalStackObj(tryStackObjToInteger(arg) == NA_INTEGER);
        case LGLSXP:
            return logicalStackObj(tryStackObjToLogical(arg) == NA_LOGICAL);
        case REALSXP:
            return logicalStackObj(ISNAN(tryStackObjToReal(arg)));
        default:
            return nullStackObj;
        }
        assert(false);
    }

    case 393: { // "is.function"
        if (nargs != 1)
            return nullStackObj;
        return logicalStackObj(args[0].tag == STACK_OBJ_SEXP &&
                               isFunction(args[0].u.sxpval));
    }

    case 507: { // "islistfactor"
        if (nargs != 2)
            return nullStackObj;
        if (args[0].tag != STACK_OBJ_SEXP)
            return logicalStackObj(false);
        auto arg0 = args[0].u.sxpval;

        auto n = XLENGTH(arg0);
        if (n == 0 || !isVectorList(arg0))
            return logicalStackObj(false);

        // TODO: This replaces asLogical, is that OK?
        int recursive = tryStackObjToLogicalNa(args[1]);
        if (recursive)
            return nullStackObj;

        for (int i = 0; i < n; i++)
            if (!isFactor(VECTOR_ELT(arg0, i)))
                return logicalStackObj(false);

        return logicalStackObj(true);
    }

    case 88: { // "length"
        if (nargs != 1)
            return nullStackObj;

        switch (stackObjSexpType(args[0])) {
        case INTSXP:
        case REALSXP:
        case LGLSXP:
            return intStackObj(stackObjLength(args[0]));
        default:
            return nullStackObj;
        }
        assert(false);
    }

    case 386: { // "is.numeric"
        if (nargs != 1)
            return nullStackObj;
        switch (args[0].tag) {
        case STACK_OBJ_INT:
        case STACK_OBJ_REAL:
            return logicalStackObj(true);
        case STACK_OBJ_LOGICAL:
            return logicalStackObj(false);
        case STACK_OBJ_SEXP:
            return logicalStackObj(isNumeric(args[0].u.sxpval) &&
                                   !isLogical(args[0].u.sxpval));
        default:
            assert(false);
        }
    }

    case 387: { // "is.matrix"
        if (nargs != 1)
            return nullStackObj;
        return logicalStackObj(args[0].tag == STACK_OBJ_SEXP &&
                               isMatrix(args[0].u.sxpval));
    }

    case 388: { // "is.array"
        if (nargs != 1)
            return nullStackObj;
        return logicalStackObj(args[0].tag == STACK_OBJ_SEXP &&
                               isArray(args[0].u.sxpval));
    }

    case 389: { // "is.atomic"
        if (nargs != 1)
            return nullStackObj;
        switch (stackObjSexpType(args[0])) {
        case NILSXP:
        /* NULL is atomic (S compatibly), but not in isVectorAtomic(.) */
        case CHARSXP:
        case LGLSXP:
        case INTSXP:
        case REALSXP:
        case CPLXSXP:
        case STRSXP:
        case RAWSXP:
            return logicalStackObj(true);
        default:
            return logicalStackObj(false);
        }
        assert(false);
    }

    case 384: { // "is.object"
        if (nargs != 1)
            return nullStackObj;
        return logicalStackObj(args[0].tag == STACK_OBJ_SEXP &&
                               OBJECT(args[0].u.sxpval));
    }

    case 136: { // "which"
        if (nargs != 1)
            return nullStackObj;
        auto arg = args[0];
        if (stackObjSexpType(arg) != LGLSXP)
            return nullStackObj;
        switch (arg.tag) {
        case STACK_OBJ_LOGICAL:
            // TODO: Is R_NilValue NA?
            return (bool)arg.u.ival ? intStackObj(1)
                                    : sexpToStackObj(R_NilValue);
        case STACK_OBJ_SEXP: {
            auto argSexp = arg.u.sxpval;
            std::vector<size_t> which;
            for (long i = 0; i < XLENGTH(argSexp); ++i) {
                if (LOGICAL(argSexp)[i] == TRUE) {
                    which.push_back(i);
                }
            }
            auto res = Rf_allocVector(INTSXP, which.size());
            size_t pos = 0;
            for (auto i : which)
                INTEGER(res)[pos++] = i + 1;
            return sexpToStackObj(res);
        }
        default:
            assert(false);
        }
    }

    case 407: { // "rep.int"
        if (nargs != 2)
            return nullStackObj;

        auto times = tryStackObjToInteger(args[1]);
        if (times == NA_INTEGER)
            return nullStackObj;

        auto x = args[0];
        if (stackObjSexpType(x) != INTSXP && stackObjSexpType(x) != REALSXP)
            return nullStackObj;

        auto len = stackObjLength(x);

        auto res = Rf_allocVector(stackObjSexpType(x), len * times);

        switch (stackObjSexpType(x)) {

        case INTSXP:
            for (long i = 0; i < times; ++i) {
                if (x.tag == STACK_OBJ_INT) {
                    SLOWASSERT(len == 1);
                    INTEGER(res)[i] = x.u.ival;
                } else {
                    SLOWASSERT(x.tag == STACK_OBJ_SEXP);
                    for (long j = 0; j < len; ++j)
                        INTEGER(res)[i * len + j] = INTEGER(x.u.sxpval)[j];
                }
            }
            break;

        case REALSXP:
            for (long i = 0; i < times; ++i) {
                if (x.tag == STACK_OBJ_REAL) {
                    SLOWASSERT(len == 1);
                    REAL(res)[i] = x.u.dval;
                } else {
                    SLOWASSERT(x.tag == STACK_OBJ_SEXP);
                    for (long j = 0; j < len; ++j)
                        REAL(res)[i * len + j] = REAL(x.u.sxpval)[j];
                }
            }
            break;

        default:
            assert(false);
        }

        return sexpToStackObj(res);
    }

    case 301: { // "min"
        if (nargs != 2)
            return nullStackObj;

        auto a = args[0];
        auto b = args[1];

        if (stackObjLength(a) != 1 || stackObjLength(b) != 1)
            return nullStackObj;

        auto combination =
            (stackObjSexpType(args[0]) << 8) + stackObjSexpType(args[1]);

        switch (combination) {
        case (INTSXP << 8) + INTSXP:
            if (tryStackObjToInteger(a) == NA_INTEGER ||
                tryStackObjToInteger(b) == NA_INTEGER)
                return nullStackObj;
            return tryStackObjToInteger(a) < tryStackObjToInteger(b) ? a : b;

        case (INTSXP << 8) + REALSXP:
            if (tryStackObjToInteger(a) == NA_INTEGER ||
                ISNAN(tryStackObjToReal(b)))
                return nullStackObj;
            return tryStackObjToInteger(a) < tryStackObjToReal(b) ? a : b;

        case (REALSXP << 8) + INTSXP:
            if (tryStackObjToInteger(b) == NA_INTEGER ||
                ISNAN(tryStackObjToReal(a)))
                return nullStackObj;
            return tryStackObjToReal(a) < tryStackObjToInteger(b) ? a : b;

        case (REALSXP << 8) + REALSXP:
            if (ISNAN(tryStackObjToReal(a)) || ISNAN(tryStackObjToReal(b)))
                return nullStackObj;
            return tryStackObjToReal(a) < tryStackObjToReal(b) ? a : b;

        default:
            return nullStackObj;
        }
    }
    }

    return nullStackObj;
}
} // namespace rir
