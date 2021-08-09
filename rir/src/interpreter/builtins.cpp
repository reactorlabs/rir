#include "builtins.h"
#include "R/BuiltinIds.h"
#include "R/Funtab.h"
#include "compiler/util/safe_builtins_list.h"
#include "interp.h"
#include "runtime/LazyArglist.h"
#include <algorithm>
#include <stdlib.h>

extern "C" {
extern Rboolean R_Visible;
SEXP R_subset3_dflt(SEXP, SEXP, SEXP);
int R_DispatchOrEvalSP(SEXP call, SEXP op, const char* generic, SEXP args,
                       SEXP rho, SEXP* ans);
}

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

enum class IsVectorCheck {
    unsupported,
    any,
    numeric,
    integer,
    logical,
    character,
};

static IsVectorCheck whichIsVectorCheck(SEXP str) {
    if (!isString(str) || LENGTH(str) != 1)
        return IsVectorCheck::unsupported;
    auto stype = CHAR(STRING_ELT(str, 0));
    if (std::string("any") == stype) {
        return IsVectorCheck::any;
    } else if (std::string("numeric") == stype) {
        return IsVectorCheck::numeric;
    } else if (std::string("integer") == stype) {
        return IsVectorCheck::integer;
    } else if (std::string("logical") == stype) {
        return IsVectorCheck::logical;
    } else if (std::string("double") == stype) {
        return IsVectorCheck::numeric;
    } else if (std::string("character") == stype) {
        return IsVectorCheck::character;
    }
    return IsVectorCheck::unsupported;
}

SEXP tryFastSpecialCall(CallContext& call, InterpreterInstance* ctx) {
    auto nargs = call.passedArgs;
    switch (call.callee->u.primsxp.offset) {
    case blt("substitute"): {
        if (nargs != 1 || call.hasNames())
            return nullptr;
        return Rf_substitute(call.stackArg(0), call.callerEnv);
    }
    case blt("$"): {
        auto x = call.stackArg(0);
        auto s = call.stackArg(1);
        if (TYPEOF(s) != PROMSXP)
            return nullptr;
        s = PREXPR(s);
        if (auto c = Code::check(s))
            s = c->trivialExpr;
        if (TYPEOF(s) == SYMSXP)
            s = PRINTNAME(s);
        else if (TYPEOF(s) == STRSXP && XLENGTH(s) > 0)
            s = STRING_ELT(s, 0);

        if (nargs == 2 && s && TYPEOF(s) == CHARSXP) {
            if (TYPEOF(x) == PROMSXP)
                x = evaluatePromise(x, ctx, nullptr, true);

            if (isObject(x)) {
                ENSURE_NAMEDMAX(x);
                SEXP ss = PROTECT(allocVector(STRSXP, 1));
                SET_STRING_ELT(ss, 0, s);
                auto args = CONS_NR(x, CONS_NR(ss, R_NilValue));
                PROTECT(args);
                SEXP ans;
                if (R_DispatchOrEvalSP(call.ast, call.callee, "$", args,
                                       materializeCallerEnv(call, ctx), &ans)) {
                    UNPROTECT(2); /* args */
                    if (NAMED(ans))
                        ENSURE_NAMEDMAX(ans);
                    return (ans);
                }
                UNPROTECT(2);
            }
            return R_subset3_dflt(x, s, call.ast);
        }
        return nullptr;
    }
    case blt("forceAndCall"): {

        if (call.passedArgs < 2)
            Rf_error("attempt to apply non-function");

        auto nArg = call.stackArg(0);
        if (TYPEOF(nArg) == PROMSXP)
            nArg = evaluatePromise(nArg, ctx);
        int n = asInteger(nArg);
        if (n == NA_INTEGER)
            n = 0;

        auto fun = call.stackArg(1);
        if (TYPEOF(fun) == PROMSXP)
            fun = evaluatePromise(fun, ctx);
        assert(call.stackArgs);

        // Remove the first arg and create a new CallContext for the actual
        // call.
        auto ast = LCONS(CADDR(call.ast), CDDDR(call.ast));
        PROTECT(ast);
        Context innerCtxt;
        auto nargs = call.suppliedArgs - 2;
        if (TYPEOF(fun) == CLOSXP) {
            for (size_t i = 0; i < nargs && i < Context::NUM_TYPED_ARGS; ++i) {
                if (call.givenContext.isEager(i + 2))
                    innerCtxt.setEager(i);
                if (call.givenContext.isSimpleInt(i + 2))
                    innerCtxt.setSimpleInt(i);
                if (call.givenContext.isSimpleReal(i + 2))
                    innerCtxt.setSimpleReal(i);
                if (call.givenContext.isNotObj(i + 2))
                    innerCtxt.setNotObj(i);
                if (call.givenContext.isNonRefl(i + 2))
                    innerCtxt.setNonRefl(i);
            }
            if (call.givenContext.includes(Assumption::NoExplicitlyMissingArgs))
                innerCtxt.add(Assumption::NoExplicitlyMissingArgs);
            if (call.givenContext.includes(Assumption::CorrectOrderOfArguments))
                innerCtxt.add(Assumption::CorrectOrderOfArguments);
            if (auto dt = DispatchTable::check(BODY(fun))) {
                auto f = dt->baseline();
                if (f->nargs() >= nargs) {
                    innerCtxt.add(Assumption::NotTooManyArguments);
                    innerCtxt.numMissing(f->nargs() - nargs);
                }
            }
        }
        CallContext innerCall(ArglistOrder::NOT_REORDERED, call.caller, fun,
                              nargs, ast, call.stackArgs + 2,
                              call.names ? call.names + 2 : nullptr,
                              call.callerEnv, R_NilValue, innerCtxt, ctx);

        if (TYPEOF(fun) == BUILTINSXP || TYPEOF(fun) == CLOSXP) {
            for (int i = 0; i < n; i++) {
                auto p = innerCall.stackArg(i);
                if (TYPEOF(p) == PROMSXP)
                    evaluatePromise(p);
                else if (p == R_MissingArg)
                    errorcall(innerCall.ast, ("argument %d is empty"), i);
            }
        } else if (TYPEOF(fun) != SPECIALSXP) {
            Rf_error("attempt to apply non-function");
        }
        auto res = doCall(innerCall, ctx);
        UNPROTECT(1);
        R_Visible = (Rboolean) true;
        return res;
    }
    default:
        break;
    }
    return nullptr;
}

static constexpr size_t MAXARGS = 8;

bool supportsFastBuiltinCall2(SEXP b, size_t nargs) {
    if (nargs > 5)
        return false;

    // This is a blocklist of builtins which tamper with the argslist in some
    // bad way. This can be changing contents and assume they are protected, or
    // leaking cons cells of the arglist (e.g. through the gengc_next pointers).
    switch (b->u.primsxp.offset) {
    // misc
    case blt("registerNamespace"):
    case blt("...length"):
    case blt("...elt"):
    case blt("strsplit"):
    case blt("eval"):
    // Injects args
    case blt("standardGeneric"):
    // because of fixup_NaRm
    case blt("range"):
    case blt("sum"):
    case blt("min"):
    case blt("max"):
    case blt("prod"):
    case blt("mean"):
    case blt("any"):
    case blt("all"):
    // because of other SETCDR on the argslist
    case blt("match.call"):
    case blt(".subset"):
    case blt(".subset2"):
    case blt("$<-"):
    case blt("NextMethod"):
    case blt("options"):
    case blt("&"):
    case blt("|"):
    case blt("attach"):
    case blt("psort"):
    // case blt("invisible"):
    // because of longjmp
    case blt("warning"):
    case blt("stop"):
    case blt(".dfltStop"):
    case blt(".signalCondition"):
    // SETCAR
    case blt("%*%"):
    case blt("match"):
    case blt("crossprod"):
    case blt("tcrossprod"):
    case blt("comment<-"):
    case blt("oldClass<-"):
    case blt("names<-"):
    case blt("dimnames<-"):
    case blt("dim<-"):
    case blt("levels<-"):
    case blt("makeLazy"):
    case blt("args"):
    case blt("as.function.default"):
    case blt("as.call"):
    case blt("do.call"):
    case blt("call"):
    case blt("class<-"):
    case blt("debug"):
    case blt("undebug"):
    case blt("isdebugged"):
    case blt("debugonce"):
    case blt("dump"):
    case blt("browser"):
    case blt("unclass"):
    case blt("save"):
    case blt("saveToConn"):
    case blt("[<-"):
    case blt("[[<-"):
    // SET_TAG
    case blt("cbind"):
    case blt("rbind"):
        return false;
    default: {}
    }
    return true;
}


static bool doesNotAccessEnv(SEXP b) {
    return pir::SafeBuiltinsList::nonObject(b->u.primsxp.offset);
}

SEXP tryFastBuiltinCall2(CallContext& call, InterpreterInstance* ctx,
                         size_t nargs, SEXP (&args)[MAXARGS]) {
    assert(nargs <= 5);

    {
        SEXP arglist;
        CCODE f = getBuiltin(call.callee);
        SEXP res = nullptr;
        auto env = doesNotAccessEnv(call.callee) ? R_BaseEnv : materializeCallerEnv(call, ctx);
        switch (call.passedArgs) {
        case 0: {
            return f(call.ast, call.callee, R_NilValue, env);
        }
        case 1: {
            FAKE_ARGS1(arglist, args[0]);
            res = f(call.ast, call.callee, arglist, env);
            CHECK_FAKE_ARGS1();
            break;
        }
        case 2: {
            FAKE_ARGS2(arglist, args[0], args[1]);
            res = f(call.ast, call.callee, arglist, env);
            CHECK_FAKE_ARGS2();
            break;
        }
        case 3: {
            FAKE_ARGS3(arglist, args[0], args[1], args[2]);
            res = f(call.ast, call.callee, arglist, env);
            CHECK_FAKE_ARGS3();
            break;
        }
        case 4: {
            FAKE_ARGS4(arglist, args[0], args[1], args[2], args[3]);
            res = f(call.ast, call.callee, arglist, env);
            CHECK_FAKE_ARGS4();
            break;
        }
        case 5: {
            FAKE_ARGS5(arglist, args[0], args[1], args[2], args[3], args[4]);
            res = f(call.ast, call.callee, arglist, env);
            CHECK_FAKE_ARGS5();
            break;
        }
        }
        return res;
    }
    return nullptr;
}

SEXP tryFastBuiltinCall1(const CallContext& call, InterpreterInstance* ctx,
                         size_t nargs, bool hasAttrib, SEXP (&args)[MAXARGS]) {
    switch (call.callee->u.primsxp.offset) {
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

    case blt("baseenv"): {
        return R_BaseEnv;
    }
    }

    if (hasAttrib)
        return nullptr;

    switch (call.callee->u.primsxp.offset) {
    case blt("nargs"): {
        if (nargs != 0)
            return nullptr;

        int nargs = -1;
        for (RCNTXT* cptr = (RCNTXT*)R_GlobalContext; cptr != NULL;
             cptr = cptr->nextcontext) {
            if ((cptr->callflag & CTXT_FUNCTION) &&
                cptr->cloenv == call.callerEnv) {
                if (auto l = LazyArglist::check(cptr->promargs)) {
                    nargs = l->nargs();
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

        size_t res;
        switch (TYPEOF(args[0])) {
        case INTSXP:
        case REALSXP:
        case LGLSXP:
        case STRSXP:
            res = XLENGTH(args[0]);
            break;
        default:
            res = Rf_xlength(args[0]);
            break;
        }
        if (res >= INT_MAX)
            return Rf_ScalarReal(res);
        return Rf_ScalarInteger(res);
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
        SEXP s;
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
                py[i] = hypot(px[i].r, px[i].i);
            return y;
        }
        return nullptr;
    }

    case blt("min"):
    case blt("max"): {
        if (nargs != 2)
            return nullptr;

        SEXP a = args[0];
        SEXP b = args[1];

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

    case blt("all"): {
        for (size_t i = 0; i < nargs; ++i) {
            auto a = args[0];
            if (IS_SIMPLE_SCALAR(a, LGLSXP)) {
                if (LOGICAL(a)[0] == 0)
                    return R_FalseValue;
                else if (LOGICAL(a)[0] == 1)
                    continue;
            }
            return nullptr;
        }
        return R_TrueValue;
    }

    case blt("any"): {
        for (size_t i = 0; i < nargs; ++i) {
            auto a = args[0];
            if (IS_SIMPLE_SCALAR(a, LGLSXP)) {
                if (LOGICAL(a)[0] == 1)
                    return R_TrueValue;
                else if (LOGICAL(a)[0] == 0)
                    continue;
            }
            return nullptr;
        }
        return R_FalseValue;
    }

    case blt("as.character"): {
        if (nargs != 1)
            return nullptr;
        if (TYPEOF(args[0]) == STRSXP) {
            SEXP r = args[0];
            return r;
        }
        if (TYPEOF(args[0]) == SYMSXP) {
            return ScalarString(PRINTNAME(args[0]));
        }
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
                SEXP r = stringCache[i];
                return r;
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
        if (TYPEOF(args[0]) == INTSXP) {
            SEXP r = args[0];
            return r;
        }
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
        bool res = false;
        if (nargs < 1 || nargs > 2)
            return nullptr;

        auto arg = args[0];
        if (nargs == 1) {
            res = isVector(arg);
        } else {
            auto which = whichIsVectorCheck(args[1]);
            switch (which) {
            case IsVectorCheck::any:
                res = isVector(arg);
                break;
            case IsVectorCheck::numeric:
                res = (isNumeric(arg) && !isLogical(arg));
                break;
            case IsVectorCheck::integer:
                res = isInteger(arg);
                break;
            case IsVectorCheck::logical:
                res = isLogical(arg);
                break;
            case IsVectorCheck::character:
                res = isString(arg);
                break;
            case IsVectorCheck::unsupported:
                return nullptr;
            }
        }

        if (!res)
            return R_FalseValue;

        auto a = ATTRIB(arg);
        while (a != R_NilValue) {
            if (TAG(a) != R_NamesSymbol)
                return R_FalseValue;
            a = CDR(a);
        }
        return R_TrueValue;
    }

    case blt("as.logical"): {
        if (nargs != 1)
            return nullptr;
        auto arg = args[0];
        if (TYPEOF(arg) != LGLSXP)
            return nullptr;
        if (ATTRIB(arg) == R_NilValue)
            return arg;
        arg = MAYBE_REFERENCED(arg) ? duplicate(arg) : arg;
        CLEAR_ATTRIB(arg);
        return arg;
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

    case blt("row"): {
        if (nargs != 1)
            return nullptr;
        SEXP dim = args[0];
        int nprot = 0;
        if (!isInteger(dim)) {
            PROTECT(dim = coerceVector(dim, INTSXP));
            nprot++;
        }
        if (LENGTH(dim) != 2)
            Rf_error("a matrix-like object is required as argument to 'row'");

        int nr = INTEGER(dim)[0], nc = INTEGER(dim)[1];
        if (nprot)
            UNPROTECT(nprot);

        SEXP ans = allocMatrix(INTSXP, nr, nc);

        R_xlen_t NR = nr;
        for (int i = 0; i < nr; i++)
            for (int j = 0; j < nc; j++)
                INTEGER(ans)[i + j * NR] = i + 1;
        return ans;
    }

    case blt("col"): {
        if (nargs != 1)
            return nullptr;
        SEXP dim = args[0];
        int nprot = 0;
        if (!isInteger(dim)) {
            PROTECT(dim = coerceVector(dim, INTSXP));
            nprot++;
        }
        if (LENGTH(dim) != 2)
            Rf_error("a matrix-like object is required as argument to 'col'");

        int nr = INTEGER(dim)[0], nc = INTEGER(dim)[1];
        if (nprot)
            UNPROTECT(nprot);

        SEXP ans = allocMatrix(INTSXP, nr, nc);

        R_xlen_t NR = nr;
        for (int i = 0; i < nr; i++)
            for (int j = 0; j < nc; j++)
                INTEGER(ans)[i + j * NR] = j + 1;
        return ans;
    }
    case blt("dim"): {
        if (nargs != 1)
            return nullptr;
        return getAttrib(args[0], R_DimNamesSymbol);
    }
    }
    return nullptr;
}

bool supportsFastBuiltinCall(SEXP b, size_t nargs) {
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
    case blt("all"):
    case blt("any"):
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
    case blt("as.logical"):
    case blt("list"):
    case blt("rep.int"):
    case blt("islistfactor"):
    case blt("bitwiseAnd"):
    case blt("bitwiseOr"):
    case blt("bitwiseXor"):
    case blt("bitwiseShiftL"):
    case blt("bitwiseShiftR"):
    case blt("baseenv"):
    case blt("col"):
    case blt("row"):
    case blt("dim"):
    case blt("$"):
        return true;
    default: {}
    }
    return false;
}

SEXP tryFastBuiltinCall(CallContext& call, InterpreterInstance* ctx) {
    SLOWASSERT(!call.hasNames());

    SEXP args[MAXARGS];
    auto nargs = call.suppliedArgs;

    if (nargs > MAXARGS)
        return nullptr;

    bool hasAttrib = false;
    for (size_t i = 0; i < call.suppliedArgs; ++i) {
        auto arg = call.stackArg(i);
        if (TYPEOF(arg) == PROMSXP)
            arg = evaluatePromise(arg);
        if (arg == R_UnboundValue || arg == R_MissingArg)
            return nullptr;
        if (ATTRIB(arg) != R_NilValue)
            hasAttrib = true;
        args[i] = arg;
    }

    auto res = tryFastBuiltinCall1(call, ctx, nargs, hasAttrib, args);
    if (res)
        return res;

    if (hasAttrib)
        return nullptr;

    if (!supportsFastBuiltinCall2(call.callee, nargs))
        return nullptr;

    return tryFastBuiltinCall2(call, ctx, nargs, args);
}

SEXP vapply(SEXP X, SEXP XX, SEXP FUN, SEXP value, SEXP useNames_, SEXP env) {
    R_xlen_t i, n;
    int commonLen;
    int useNames,
        rnk_v = -1; // = array_rank(value) := length(dim(value))
    bool array_value;
    SEXPTYPE commonType;
    PROTECT_INDEX index = 0; /* initialize to avoid a warning */

    if (!isVector(value))
        Rf_error(("'FUN.VALUE' must be a vector"));
    useNames = asLogical(useNames_);
    if (useNames == NA_LOGICAL)
        Rf_error(("invalid '%s' value"), "USE.NAMES");

    n = xlength(XX);
    if (n == NA_INTEGER)
        Rf_error(("invalid length"));
    bool realIndx = n > INT_MAX;

    commonLen = Rf_length(value);
    if (commonLen > 1 && n > INT_MAX)
        Rf_error(("long vectors are not supported for matrix/array results"));
    commonType = TYPEOF(value);
    // check once here
    if (commonType != CPLXSXP && commonType != REALSXP &&
        commonType != INTSXP && commonType != LGLSXP && commonType != RAWSXP &&
        commonType != STRSXP && commonType != VECSXP)
        Rf_error(("type '%s' is not supported"), type2char(commonType));
    auto dim_v = getAttrib(value, R_DimSymbol);
    array_value = (TYPEOF(dim_v) == INTSXP && LENGTH(dim_v) >= 1);
    auto ans = PROTECT(allocVector(commonType, n * commonLen));
    SEXP rowNames = nullptr;
    SEXP names = nullptr;
    if (useNames) {
        names = PROTECT(getAttrib(XX, R_NamesSymbol));
        if (isNull(names) && TYPEOF(XX) == STRSXP) {
            UNPROTECT(1);
            PROTECT(names = XX);
        }
        PROTECT_WITH_INDEX(rowNames =
                               getAttrib(value, array_value ? R_DimNamesSymbol
                                                            : R_NamesSymbol),
                           &index);
    }
    /* The R level code has ensured that XX is a vector.
       If it is atomic we can speed things up slightly by
       using the evaluated version.
    */
    {
        SEXP ind;
        /* Build call: FUN(XX[[<ind>]], ...) */

        SEXP isym = install("i");
        PROTECT(ind = allocVector(realIndx ? REALSXP : INTSXP, 1));
        defineVar(isym, ind, env);
        INCREMENT_NAMED(ind);

        /* Notice that it is OK to have one arg to LCONS do memory
           allocation and not PROTECT the result (LCONS does memory
           protection of its args internally), but not both of them,
           since the computation of one may destroy the other */
        auto tmp =
            PROTECT(LCONS(R_Bracket2Symbol, LCONS(X, LCONS(isym, R_NilValue))));
        auto R_fcall =
            PROTECT(LCONS(FUN, LCONS(tmp, LCONS(R_DotsSymbol, R_NilValue))));

        int common_len_offset = 0;
        for (i = 0; i < n; i++) {
            SEXP val;
            SEXPTYPE valType;
            PROTECT_INDEX indx;
            if (realIndx)
                REAL(ind)[0] = (double)(i + 1);
            else
                INTEGER(ind)[0] = (int)(i + 1);
            val = R_forceAndCall(R_fcall, 1, env);
            if (MAYBE_REFERENCED(val))
                val = lazy_duplicate(
                    val); // Need to duplicate? Copying again anyway
            PROTECT_WITH_INDEX(val, &indx);
            if (Rf_length(val) != commonLen)
                Rf_error(("values must be length %d,\n but "
                          "FUN(X[[%d]]) result is length %d"),
                         commonLen, i + 1, Rf_length(val));
            valType = TYPEOF(val);
            if (valType != commonType) {
                bool okay = false;
                switch (commonType) {
                case CPLXSXP:
                    okay = (valType == REALSXP) || (valType == INTSXP) ||
                           (valType == LGLSXP);
                    break;
                case REALSXP:
                    okay = (valType == INTSXP) || (valType == LGLSXP);
                    break;
                case INTSXP:
                    okay = (valType == LGLSXP);
                    break;
                }
                if (!okay)
                    Rf_error(("values must be type '%s',\n but "
                              "FUN(X[[%d]]) result is type '%s'"),
                             type2char(commonType), i + 1, type2char(valType));
                REPROTECT(val = coerceVector(val, commonType), indx);
            }
            /* Take row names from the first result only */
            if (i == 0 && useNames && isNull(rowNames))
                REPROTECT(rowNames =
                              getAttrib(val, array_value ? R_DimNamesSymbol
                                                         : R_NamesSymbol),
                          index);
            // two cases - only for efficiency
            if (commonLen == 1) { // common case
                switch (commonType) {
                case CPLXSXP:
                    COMPLEX(ans)[i] = COMPLEX(val)[0];
                    break;
                case REALSXP:
                    REAL(ans)[i] = REAL(val)[0];
                    break;
                case INTSXP:
                    INTEGER(ans)[i] = INTEGER(val)[0];
                    break;
                case LGLSXP:
                    LOGICAL(ans)[i] = LOGICAL(val)[0];
                    break;
                case RAWSXP:
                    RAW(ans)[i] = RAW(val)[0];
                    break;
                case STRSXP:
                    SET_STRING_ELT(ans, i, STRING_ELT(val, 0));
                    break;
                case VECSXP:
                    SET_VECTOR_ELT(ans, i, VECTOR_ELT(val, 0));
                    break;
                }
            } else { // commonLen > 1 (typically, or == 0) :
                switch (commonType) {
                case REALSXP:
                    memcpy(REAL(ans) + common_len_offset, REAL(val),
                           commonLen * sizeof(double));
                    break;
                case INTSXP:
                    memcpy(INTEGER(ans) + common_len_offset, INTEGER(val),
                           commonLen * sizeof(int));
                    break;
                case LGLSXP:
                    memcpy(LOGICAL(ans) + common_len_offset, LOGICAL(val),
                           commonLen * sizeof(int));
                    break;
                case RAWSXP:
                    memcpy(RAW(ans) + common_len_offset, RAW(val),
                           commonLen * sizeof(Rbyte));
                    break;
                case CPLXSXP:
                    memcpy(COMPLEX(ans) + common_len_offset, COMPLEX(val),
                           commonLen * sizeof(Rcomplex));
                    break;
                case STRSXP:
                    for (int j = 0; j < commonLen; j++)
                        SET_STRING_ELT(ans, common_len_offset + j,
                                       STRING_ELT(val, j));
                    break;
                case VECSXP:
                    for (int j = 0; j < commonLen; j++)
                        SET_VECTOR_ELT(ans, common_len_offset + j,
                                       VECTOR_ELT(val, j));
                    break;
                }
                common_len_offset += commonLen;
            }
            UNPROTECT(1);
        }
        UNPROTECT(3);
    }

    if (commonLen != 1) {
        SEXP dim;
        rnk_v = array_value ? LENGTH(dim_v) : 1;
        PROTECT(dim = allocVector(INTSXP, rnk_v + 1));
        if (array_value)
            for (int j = 0; j < rnk_v; j++)
                INTEGER(dim)[j] = INTEGER(dim_v)[j];
        else
            INTEGER(dim)[0] = commonLen;
        INTEGER(dim)[rnk_v] = (int)n; // checked above
        setAttrib(ans, R_DimSymbol, dim);
        UNPROTECT(1);
    }

    if (useNames) {
        if (commonLen == 1) {
            if (!isNull(names))
                setAttrib(ans, R_NamesSymbol, names);
        } else {
            if (!isNull(names) || !isNull(rowNames)) {
                SEXP dimnames;
                PROTECT(dimnames = allocVector(VECSXP, rnk_v + 1));
                if (array_value && !isNull(rowNames)) {
                    if (TYPEOF(rowNames) != VECSXP || LENGTH(rowNames) != rnk_v)
                        // should never happen ..
                        Rf_error(("dimnames(<value>) is neither NULL "
                                  "nor list of length %d"),
                                 rnk_v);
                    for (int j = 0; j < rnk_v; j++)
                        SET_VECTOR_ELT(dimnames, j, VECTOR_ELT(rowNames, j));
                } else
                    SET_VECTOR_ELT(dimnames, 0, rowNames);

                SET_VECTOR_ELT(dimnames, rnk_v, names);
                setAttrib(ans, R_DimNamesSymbol, dimnames);
                UNPROTECT(1);
            }
        }
    }
    UNPROTECT(
        useNames ? 3 : 1); /* X, XX, value, ans, and maybe names and rowNames */

    return ans;
}

} // namespace rir
