#include "safe_builtins_list.h"

#include "R/BuiltinIds.h"
#include "R/Funtab.h"
#include "R/Symbols.h"

#include <algorithm>
#include <string>

namespace rir {
namespace pir {

bool SafeBuiltinsList::always(int builtin) {
    switch (builtin) {
    case blt("diag"):
    case blt("backsolve"):
    case blt("max.col"):
    case blt("row"):
    case blt("col"):
    case blt("all.names"):
    case blt("list"):
    case blt("formals"):
    case blt("body"):
    case blt("bodyCode"):

    case blt("matrix"):

        // do_bitwise
    case blt("bitwiseAnd"):
    case blt("bitwiseNot"):
    case blt("bitwiseOr"):
    case blt("bitwiseXor"):
    case blt("bitwiseShiftL"):
    case blt("bitwiseShiftR"):

        // do_randomN
    case blt("rchisq"):
    case blt("rexp"):
    case blt("rgeom"):
    case blt("rpois"):
    case blt("rt"):
    case blt("rsignrank"):
    case blt("rbeta"):
    case blt("rbinom"):
    case blt("rcauchy"):
    case blt("rf"):
    case blt("rgamma"):
    case blt("rlnorm"):
    case blt("rlogis"):
    case blt("rnbinom"):
    case blt("rnbinom_mu"):
    case blt("rnchisq"):
    case blt("rnorm"):
    case blt("runif"):
    case blt("rweibull"):
    case blt("rwilcox"):
    case blt("rhyper"):

        // coerce.c
    case blt("as.function.default"):
    case blt("typeof"):
    case blt("is.vector"):
    case blt("is.null"):
    case blt("is.logical"):
    case blt("is.integer"):
    case blt("is.double"):
    case blt("is.complex"):
    case blt("is.character"):
    case blt("is.symbol"):
    case blt("is.name"):
    case blt("is.environment"):
    case blt("is.list"):
    case blt("is.pairlist"):
    case blt("is.expression"):
    case blt("is.raw"):
    case blt("is.object"):
    case blt("isS4"):

    case blt("which"):

    case blt("cat"):
    case blt("stdout"):
    case blt("stderr"):
    case blt("("):
    case blt("Sys.time"):

    case blt("strsplit"):

    case blt("seq_len"):
    case blt("rep_len"):
        return true;
    default: {}
    };
    return false;
}
bool SafeBuiltinsList::always(SEXP builtin) {
    return always(getBuiltinNr(builtin));
}

bool SafeBuiltinsList::returnsObj(int builtin) {
    switch (builtin) {
    case blt("stdout"):
    case blt("stderr"):
        return true;
    default: {}
    };

    return false;
}

bool SafeBuiltinsList::nonObject(int builtin) {
    if (always(builtin))
        return true;

    switch (builtin) {
        // TODO: this should be always safe: but something breaks if it is
        // moved. Need to investigate what!
    case blt("is.atomic"):

        // Those are not always safe, due to coerceVector: which can be
        // overwritten by objects
    case blt("vector"):
    case blt("complex"):
    case blt("array"):
    case blt("new.env"):
    case blt("match"):

    case blt("dim"):
    case blt("names"):

    case blt("$"):
    case blt("c"):
    case blt("["):
    case blt("[["):
    case blt("+"):
    case blt("-"):
    case blt("*"):
    case blt("/"):
    case blt("^"):
    case blt("%%"):
    case blt("%/%"):
    case blt("%*%"):
    case blt("=="):
    case blt("!="):
    case blt("<"):
    case blt("<="):
    case blt(">="):
    case blt(">"):
    case blt("&"):
    case blt("|"):
    case blt("!"):
    case blt("&&"):
    case blt("||"):
    case blt(":"):
    case blt("~"):
    case blt("crossprod"):
    case blt("tcrossprod"):
        // Would be safe if not a vector of objects
        // blt("lengths"):
    case blt("length"):
    case blt("round"):
    case blt("signif"):
    case blt("log"):
    case blt("log10"):
    case blt("log2"):
    case blt("abs"):
    case blt("floor"):
    case blt("ceiling"):
    case blt("sqrt"):
    case blt("sign"):
    case blt("trunc"):
    case blt("exp"):
    case blt("expm1"):
    case blt("log1p"):
    case blt("cos"):
    case blt("sin"):
    case blt("tan"):
    case blt("acos"):
    case blt("asin"):
    case blt("atan"):
    case blt("cosh"):
    case blt("sinh"):
    case blt("tanh"):
    case blt("acosh"):
    case blt("asinh"):
    case blt("atanh"):
    case blt("lgamma"):
    case blt("gamma"):
    case blt("digamma"):
    case blt("trigamma"):
    case blt("cospi"):
    case blt("sinpi"):
    case blt("tanpi"):
    case blt("atan2"):
    case blt("lbeta"):
    case blt("beta"):
    case blt("lchoose"):
    case blt("choose"):
    case blt("dchisq"):
    case blt("pchisq"):
    case blt("qchisq"):
    case blt("dexp"):
    case blt("pexp"):
    case blt("qexp"):
    case blt("dgeom"):
    case blt("pgeom"):
    case blt("qgeom"):
    case blt("dpois"):
    case blt("ppois"):
    case blt("qpois"):
    case blt("dt"):
    case blt("pt"):
    case blt("qt"):
    case blt("dsignrank"):
    case blt("psignrank"):
    case blt("qsignrank"):
    case blt("besselJ"):
    case blt("besselY"):
    case blt("psigamma"):
    case blt("Re"):
    case blt("Im"):
    case blt("Mod"):
    case blt("Arg"):
    case blt("Conj"):
    case blt("dbeta"):
    case blt("pbeta"):
    case blt("qbeta"):
    case blt("dbinom"):
    case blt("pbinom"):
    case blt("qbinom"):
    case blt("dcauchy"):
    case blt("pcauchy"):
    case blt("qcauchy"):
    case blt("df"):
    case blt("pf"):
    case blt("qf"):
    case blt("dgamma"):
    case blt("pgamma"):
    case blt("qgamma"):
    case blt("dlnorm"):
    case blt("plnorm"):
    case blt("qlnorm"):
    case blt("dlogis"):
    case blt("plogis"):
    case blt("qlogis"):
    case blt("dnbinom"):
    case blt("pnbinom"):
    case blt("qnbinom"):
    case blt("dnorm"):
    case blt("pnorm"):
    case blt("qnorm"):
    case blt("dunif"):
    case blt("punif"):
    case blt("qunif"):
    case blt("dweibull"):
    case blt("pweibull"):
    case blt("qweibull"):
    case blt("dnchisq"):
    case blt("pnchisq"):
    case blt("qnchisq"):
    case blt("dnt"):
    case blt("pnt"):
    case blt("qnt"):
    case blt("dwilcox"):
    case blt("pwilcox"):
    case blt("qwilcox"):
    case blt("besselI"):
    case blt("besselK"):
    case blt("dnbinom_mu"):
    case blt("pnbinom_mu"):
    case blt("qnbinom_mu"):
    case blt("dhyper"):
    case blt("phyper"):
    case blt("qhyper"):
    case blt("dnbeta"):
    case blt("pnbeta"):
    case blt("qnbeta"):
    case blt("dnf"):
    case blt("pnf"):
    case blt("qnf"):
    case blt("dtukey"):
    case blt("ptukey"):
    case blt("qtukey"):
    case blt("sum"):
    case blt("min"):
    case blt("max"):
    case blt("prod"):
    case blt("mean"):
    case blt("range"):
    case blt("as.character"):
    case blt("as.integer"):
    case blt("as.double"):
    case blt("as.numeric"):
    case blt("as.complex"):
    case blt("as.logical"):
    case blt("as.raw"):
    case blt("as.vector"):

    case blt("is.numeric"):
    case blt("is.matrix"):
    case blt("is.array"):
    case blt("is.recursive"):
    case blt("is.call"):
    case blt("is.language"):
    case blt("is.function"):
    case blt("is.na"):
    case blt("is.nan"):
    case blt("is.finite"):
    case blt("is.infinite"):

    case blt("cumsum"):
    case blt("colSums"):

    case blt("paste"):
    case blt("nchar"):
    case blt("pmatch"):

    case blt("seq.int"):
    case blt("rep.int"):

    case blt("inherits"):
    case blt("anyNA"):
        return true;
    default: {}
    };

    return false;
}

bool SafeBuiltinsList::nonObject(SEXP builtin) {
    return nonObject(getBuiltinNr(builtin));
}

bool SafeBuiltinsList::idempotent(int builtin) {
    switch (builtin) {
    case blt("diag"):
    case blt("backsolve"):
    case blt("max.col"):
    case blt("row"):
    case blt("col"):
    case blt("all.names"):
    case blt("list"):
    case blt("formals"):
    case blt("body"):
    case blt("bodyCode"):

    case blt("matrix"):

        // do_bitwise
    case blt("bitwiseAnd"):
    case blt("bitwiseNot"):
    case blt("bitwiseOr"):
    case blt("bitwiseXor"):
    case blt("bitwiseShiftL"):
    case blt("bitwiseShiftR"):

        // coerce.c
    case blt("as.function.default"):
    case blt("typeof"):
    case blt("is.vector"):
    case blt("is.null"):
    case blt("is.logical"):
    case blt("is.integer"):
    case blt("is.double"):
    case blt("is.complex"):
    case blt("is.character"):
    case blt("is.symbol"):
    case blt("is.name"):
    case blt("is.environment"):
    case blt("is.list"):
    case blt("is.pairlist"):
    case blt("is.expression"):
    case blt("is.raw"):
    case blt("is.object"):
    case blt("isS4"):

    case blt("which"):

    case blt("("):

    case blt("seq_len"):
    case blt("rep_len"):
        return true;
    default: {}
    };

    return false;
}
bool SafeBuiltinsList::idempotent(SEXP builtin) {
    return idempotent(getBuiltinNr(builtin));
}

bool SafeBuiltinsList::nonObjectIdempotent(int builtin) {
    if (always(builtin))
        return true;

    switch (builtin) {
        // TODO: this should be always safe, but something breaks if it is
        // moved. Need to investigate what!
    case blt("is.atomic"):

    case blt("dim"):
    case blt("names"):

    case blt("["):
    case blt("[["):
    case blt("+"):
    case blt("-"):
    case blt("*"):
    case blt("/"):
    case blt("^"):

    case blt("%%"):
    case blt("%/%"):
    case blt("%*%"):
    case blt("=="):
    case blt("!="):
    case blt("<"):

    case blt("<="):
    case blt(">="):
    case blt(">"):
    case blt("&"):
    case blt("|"):
    case blt("!"):
    case blt("&&"):

    case blt("||"):
    case blt(":"):
    case blt("~"):
    case blt("crossprod"):
    case blt("tcrossprod"):

        // Would be safe if not a vector of objects
        // blt("lengths"):
    case blt("length"):
    case blt("round"):
    case blt("signif"):
    case blt("log"):
    case blt("log10"):

    case blt("log2"):
    case blt("abs"):
    case blt("floor"):
    case blt("ceiling"):
    case blt("sqrt"):

    case blt("sign"):
    case blt("trunc"):
    case blt("exp"):
    case blt("expm1"):
    case blt("log1p"):

    case blt("cos"):
    case blt("sin"):
    case blt("tan"):
    case blt("acos"):
    case blt("asin"):

    case blt("atan"):
    case blt("cosh"):
    case blt("sinh"):
    case blt("tanh"):
    case blt("acosh"):

    case blt("asinh"):
    case blt("atanh"):
    case blt("lgamma"):
    case blt("gamma"):
    case blt("digamma"):

    case blt("trigamma"):
    case blt("cospi"):
    case blt("sinpi"):
    case blt("tanpi"):
    case blt("atan2"):

    case blt("lbeta"):
    case blt("beta"):
    case blt("lchoose"):
    case blt("choose"):
    case blt("dchisq"):

    case blt("pchisq"):
    case blt("qchisq"):
    case blt("dexp"):
    case blt("pexp"):
    case blt("qexp"):

    case blt("dgeom"):
    case blt("pgeom"):
    case blt("qgeom"):
    case blt("dpois"):
    case blt("ppois"):

    case blt("qpois"):
    case blt("dt"):
    case blt("pt"):
    case blt("qt"):
    case blt("dsignrank"):

    case blt("psignrank"):
    case blt("qsignrank"):
    case blt("besselJ"):
    case blt("besselY"):

    case blt("psigamma"):
    case blt("Re"):
    case blt("Im"):
    case blt("Mod"):
    case blt("Arg"):

    case blt("Conj"):
    case blt("dbeta"):
    case blt("pbeta"):
    case blt("qbeta"):
    case blt("dbinom"):

    case blt("pbinom"):
    case blt("qbinom"):
    case blt("dcauchy"):
    case blt("pcauchy"):

    case blt("qcauchy"):
    case blt("df"):
    case blt("pf"):
    case blt("qf"):
    case blt("dgamma"):

    case blt("pgamma"):
    case blt("qgamma"):
    case blt("dlnorm"):
    case blt("plnorm"):

    case blt("qlnorm"):
    case blt("dlogis"):
    case blt("plogis"):
    case blt("qlogis"):

    case blt("dnbinom"):
    case blt("pnbinom"):
    case blt("qnbinom"):
    case blt("dnorm"):

    case blt("pnorm"):
    case blt("qnorm"):
    case blt("dunif"):
    case blt("punif"):
    case blt("qunif"):

    case blt("dweibull"):
    case blt("pweibull"):
    case blt("qweibull"):
    case blt("dnchisq"):

    case blt("pnchisq"):
    case blt("qnchisq"):
    case blt("dnt"):
    case blt("pnt"):
    case blt("qnt"):

    case blt("dwilcox"):
    case blt("pwilcox"):
    case blt("qwilcox"):
    case blt("besselI"):

    case blt("besselK"):
    case blt("dnbinom_mu"):
    case blt("pnbinom_mu"):
    case blt("qnbinom_mu"):

    case blt("dhyper"):
    case blt("phyper"):
    case blt("qhyper"):
    case blt("dnbeta"):

    case blt("pnbeta"):
    case blt("qnbeta"):
    case blt("dnf"):
    case blt("pnf"):
    case blt("qnf"):

    case blt("dtukey"):
    case blt("ptukey"):
    case blt("qtukey"):
    case blt("sum"):
    case blt("min"):

    case blt("max"):
    case blt("prod"):
    case blt("mean"):
    case blt("range"):
    case blt("as.character"):

    case blt("as.integer"):
    case blt("as.double"):
    case blt("as.numeric"):

    case blt("as.complex"):
    case blt("as.logical"):
    case blt("as.raw"):
    case blt("as.vector"):

    case blt("is.numeric"):
    case blt("is.matrix"):
    case blt("is.array"):

    case blt("is.recursive"):
    case blt("is.call"):
    case blt("is.language"):

    case blt("is.function"):
    case blt("is.na"):
    case blt("is.nan"):
    case blt("is.finite"):

    case blt("is.infinite"):

    case blt("cumsum"):
    case blt("colSums"):

    case blt("match"):

    case blt("seq.int"):
    case blt("rep.int"):

    case blt("inherits"):
    case blt("anyNA"):
        return true;
    default: {}
    }
    return false;
}

bool SafeBuiltinsList::nonObjectIdempotent(SEXP builtin) {
    return nonObjectIdempotent(getBuiltinNr(builtin));
}

#define UNSAFE_BUILTINS_FOR_INLINE(V)                                          \
    V(exists)                                                                  \
    V(parent.env)                                                              \
    V(parent.env<-)                                                            \
    V(sys.nframe)                                                              \
    V(lockBinding)                                                             \
    V(lockEnvironment)                                                         \
    V(unlockBinding)                                                           \
    V(bindingIsLocked)                                                         \
    V(as.environment)                                                          \
    V(on.exit)                                                                 \
    V(environment)                                                             \
    V(nargs)                                                                   \
    V(sys.parent)                                                              \
    V(sys.function)                                                            \
    V(sys.frame)                                                               \
    V(sys.call)                                                                \
    V(parent.frame)                                                            \
    V(UseMethod)                                                               \
    V(eval)                                                                    \
    V(topenv)                                                                  \
    V(pos.to.env)                                                              \
    V(standardGeneric)

bool SafeBuiltinsList::forInline(int builtin) {
    switch (builtin) {
#define V(name) case blt(#name):
        UNSAFE_BUILTINS_FOR_INLINE(V)
#undef V
        return false;
    default: {}
    };
    return true;
}

bool SafeBuiltinsList::forInlineByName(SEXP name) {
    static SEXP unsafeBuiltins[] = {
#define V(name) Rf_install(#name),
        UNSAFE_BUILTINS_FOR_INLINE(V)
#undef V
    };

    return std::find(std::begin(unsafeBuiltins), std::end(unsafeBuiltins),
                     name) == std::end(unsafeBuiltins);
}

bool SafeBuiltinsList::assumeStableInBaseEnv(SEXP name) {
    return R_BindingIsLocked(name, R_BaseEnv) &&
           !R_BindingIsActive(name, R_BaseEnv);
}

} // namespace pir
} // namespace rir
