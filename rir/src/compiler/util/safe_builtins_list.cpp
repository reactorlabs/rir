#include "safe_builtins_list.h"

#include "R/Funtab.h"
#include "R/Symbols.h"

namespace rir {
namespace pir {

bool SafeBuiltinsList::always(int builtin) {
    static int safeBuiltins[] = {
        findBuiltin("vector"),
        findBuiltin("vector"),
        findBuiltin("complex"),
        findBuiltin("matrix"),
        findBuiltin("array"),
        findBuiltin("diag"),
        findBuiltin("backsolve"),
        findBuiltin("max.col"),
        findBuiltin("row"),
        findBuiltin("col"),
        findBuiltin("all.names"),
        findBuiltin("list"),
        findBuiltin("formals"),
        findBuiltin("body"),
        findBuiltin("bodyCode"),

        // do_bitwise
        findBuiltin("bitwiseAnd"),
        findBuiltin("bitwiseAnd"),
        findBuiltin("bitwiseNot"),
        findBuiltin("bitwiseOr"),
        findBuiltin("bitwiseXor"),
        findBuiltin("bitwiseShiftL"),
        findBuiltin("bitwiseShiftR"),

        // do_randomN
        findBuiltin("rchisq"),
        findBuiltin("rexp"),
        findBuiltin("rgeom"),
        findBuiltin("rpois"),
        findBuiltin("rt"),
        findBuiltin("rsignrank"),
        findBuiltin("rbeta"),
        findBuiltin("rbinom"),
        findBuiltin("rcauchy"),
        findBuiltin("rf"),
        findBuiltin("rgamma"),
        findBuiltin("rlnorm"),
        findBuiltin("rlogis"),
        findBuiltin("rnbinom"),
        findBuiltin("rnbinom_mu"),
        findBuiltin("rnchisq"),
        findBuiltin("rnorm"),
        findBuiltin("runif"),
        findBuiltin("rweibull"),
        findBuiltin("rwilcox"),
        findBuiltin("rhyper"),

        // coerce.c
        findBuiltin("as.function.default"),
        findBuiltin("typeof"),
        findBuiltin("is.vector"),
    };

    for (auto i : safeBuiltins)
        if (i == builtin)
            return true;
    return false;
}
bool SafeBuiltinsList::always(SEXP builtin) {
    return always(getBuiltinNr(builtin));
}

bool SafeBuiltinsList::nonObject(int builtin) {
    if (always(builtin))
        return true;

    static int safeBuiltins[] = {
        findBuiltin("c"),
        findBuiltin("["),
        findBuiltin("[["),
        findBuiltin("+"),
        findBuiltin("-"),
        findBuiltin("*"),
        findBuiltin("/"),
        findBuiltin("^"),
        findBuiltin("%%"),
        findBuiltin("%/%"),
        findBuiltin("%*%"),
        findBuiltin("=="),
        findBuiltin("!="),
        findBuiltin("<"),
        findBuiltin("<="),
        findBuiltin(">="),
        findBuiltin(">"),
        findBuiltin("&"),
        findBuiltin("|"),
        findBuiltin("!"),
        findBuiltin("&&"),
        findBuiltin("||"),
        findBuiltin(":"),
        findBuiltin("~"),
        findBuiltin("crossprod"),
        findBuiltin("tcrossprod"),
        findBuiltin("lengths"),
        findBuiltin("round"),
        findBuiltin("signif"),
        findBuiltin("log"),
        findBuiltin("log10"),
        findBuiltin("log2"),
        findBuiltin("abs"),
        findBuiltin("floor"),
        findBuiltin("ceiling"),
        findBuiltin("sqrt"),
        findBuiltin("sign"),
        findBuiltin("trunc"),
        findBuiltin("exp"),
        findBuiltin("expm1"),
        findBuiltin("log1p"),
        findBuiltin("cos"),
        findBuiltin("sin"),
        findBuiltin("tan"),
        findBuiltin("acos"),
        findBuiltin("asin"),
        findBuiltin("atan"),
        findBuiltin("cosh"),
        findBuiltin("sinh"),
        findBuiltin("tanh"),
        findBuiltin("acosh"),
        findBuiltin("asinh"),
        findBuiltin("atanh"),
        findBuiltin("lgamma"),
        findBuiltin("gamma"),
        findBuiltin("digamma"),
        findBuiltin("trigamma"),
        findBuiltin("cospi"),
        findBuiltin("sinpi"),
        findBuiltin("tanpi"),
        findBuiltin("atan2"),
        findBuiltin("lbeta"),
        findBuiltin("beta"),
        findBuiltin("lchoose"),
        findBuiltin("choose"),
        findBuiltin("dchisq"),
        findBuiltin("pchisq"),
        findBuiltin("qchisq"),
        findBuiltin("dexp"),
        findBuiltin("pexp"),
        findBuiltin("qexp"),
        findBuiltin("dgeom"),
        findBuiltin("pgeom"),
        findBuiltin("qgeom"),
        findBuiltin("dpois"),
        findBuiltin("ppois"),
        findBuiltin("qpois"),
        findBuiltin("dt"),
        findBuiltin("pt"),
        findBuiltin("qt"),
        findBuiltin("dsignrank"),
        findBuiltin("psignrank"),
        findBuiltin("qsignrank"),
        findBuiltin("besselJ"),
        findBuiltin("besselY"),
        findBuiltin("psigamma"),
        findBuiltin("Re"),
        findBuiltin("Im"),
        findBuiltin("Mod"),
        findBuiltin("Arg"),
        findBuiltin("Conj"),
        findBuiltin("dbeta"),
        findBuiltin("pbeta"),
        findBuiltin("qbeta"),
        findBuiltin("dbinom"),
        findBuiltin("pbinom"),
        findBuiltin("qbinom"),
        findBuiltin("dcauchy"),
        findBuiltin("pcauchy"),
        findBuiltin("qcauchy"),
        findBuiltin("df"),
        findBuiltin("pf"),
        findBuiltin("qf"),
        findBuiltin("dgamma"),
        findBuiltin("pgamma"),
        findBuiltin("qgamma"),
        findBuiltin("dlnorm"),
        findBuiltin("plnorm"),
        findBuiltin("qlnorm"),
        findBuiltin("dlogis"),
        findBuiltin("plogis"),
        findBuiltin("qlogis"),
        findBuiltin("dnbinom"),
        findBuiltin("pnbinom"),
        findBuiltin("qnbinom"),
        findBuiltin("dnorm"),
        findBuiltin("pnorm"),
        findBuiltin("qnorm"),
        findBuiltin("dunif"),
        findBuiltin("punif"),
        findBuiltin("qunif"),
        findBuiltin("dweibull"),
        findBuiltin("pweibull"),
        findBuiltin("qweibull"),
        findBuiltin("dnchisq"),
        findBuiltin("pnchisq"),
        findBuiltin("qnchisq"),
        findBuiltin("dnt"),
        findBuiltin("pnt"),
        findBuiltin("qnt"),
        findBuiltin("dwilcox"),
        findBuiltin("pwilcox"),
        findBuiltin("qwilcox"),
        findBuiltin("besselI"),
        findBuiltin("besselK"),
        findBuiltin("dnbinom_mu"),
        findBuiltin("pnbinom_mu"),
        findBuiltin("qnbinom_mu"),
        findBuiltin("dhyper"),
        findBuiltin("phyper"),
        findBuiltin("qhyper"),
        findBuiltin("dnbeta"),
        findBuiltin("pnbeta"),
        findBuiltin("qnbeta"),
        findBuiltin("dnf"),
        findBuiltin("pnf"),
        findBuiltin("qnf"),
        findBuiltin("dtukey"),
        findBuiltin("ptukey"),
        findBuiltin("qtukey"),
        findBuiltin("sum"),
        findBuiltin("min"),
        findBuiltin("max"),
        findBuiltin("prod"),
        findBuiltin("mean"),
        findBuiltin("range"),
        findBuiltin("as.character"),
        findBuiltin("as.integer"),
        findBuiltin("as.double"),
        findBuiltin("as.numeric"),
        findBuiltin("as.complex"),
        findBuiltin("as.logical"),
        findBuiltin("as.raw"),
        findBuiltin("is.null"),
        findBuiltin("is.logical"),
        findBuiltin("is.integer"),
        findBuiltin("is.double"),
        findBuiltin("is.complex"),
        findBuiltin("is.character"),
        findBuiltin("is.symbol"),
        findBuiltin("is.name"),
        findBuiltin("is.environment"),
        findBuiltin("is.list"),
        findBuiltin("is.pairlist"),
        findBuiltin("is.expression"),
        findBuiltin("is.raw"),
        findBuiltin("is.object"),
        findBuiltin("isS4"),
        findBuiltin("is.numeric"),
        findBuiltin("is.matrix"),
        findBuiltin("is.array"),
        findBuiltin("is.atomic"),
        findBuiltin("is.recursive"),
        findBuiltin("is.call"),
        findBuiltin("is.language"),
        findBuiltin("is.function"),
        findBuiltin("is.single"),
        findBuiltin("is.na"),
        findBuiltin("is.nan"),
        findBuiltin("is.finite"),
        findBuiltin("is.infinite"),
    };

    for (auto i : safeBuiltins)
        if (i == builtin)
            return true;
    return false;
}

bool SafeBuiltinsList::nonObject(SEXP builtin) {
    return nonObject(getBuiltinNr(builtin));
}

bool SafeBuiltinsList::forInline(int builtin) {
    static int unsafeBuiltins[] = {
        findBuiltin("sys.frame"),
        findBuiltin("sys.call"),
        findBuiltin("UseMethod"),
        findBuiltin("standardGeneric"),
    };

    for (auto i : unsafeBuiltins)
        if (i == builtin)
            return false;
    return true;
}

bool SafeBuiltinsList::forInlineByName(SEXP name) {
    SEXP unsafeBuiltins[] = {
        rir::symbol::sysframe,
        rir::symbol::syscall,
        rir::symbol::UseMethod,
        rir::symbol::standardGeneric,
    };

    for (auto i : unsafeBuiltins)
        if (i == name)
            return false;
    return true;
}

} // namespace pir
} // namespace rir
