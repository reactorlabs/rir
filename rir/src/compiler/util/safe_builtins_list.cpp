#include "safe_builtins_list.h"

#include "R/BuiltinIds.h"
#include "R/Funtab.h"
#include "R/Symbols.h"

#include <string>

namespace rir {
namespace pir {

bool SafeBuiltinsList::always(int builtin) {
    static int safeBuiltins[] = {
        blt("diag"),
        blt("backsolve"),
        blt("max.col"),
        blt("row"),
        blt("col"),
        blt("all.names"),
        blt("list"),
        blt("formals"),
        blt("body"),
        blt("bodyCode"),

        blt("matrix"),

        // do_bitwise
        blt("bitwiseAnd"),
        blt("bitwiseNot"),
        blt("bitwiseOr"),
        blt("bitwiseXor"),
        blt("bitwiseShiftL"),
        blt("bitwiseShiftR"),

        // do_randomN
        blt("rchisq"),
        blt("rexp"),
        blt("rgeom"),
        blt("rpois"),
        blt("rt"),
        blt("rsignrank"),
        blt("rbeta"),
        blt("rbinom"),
        blt("rcauchy"),
        blt("rf"),
        blt("rgamma"),
        blt("rlnorm"),
        blt("rlogis"),
        blt("rnbinom"),
        blt("rnbinom_mu"),
        blt("rnchisq"),
        blt("rnorm"),
        blt("runif"),
        blt("rweibull"),
        blt("rwilcox"),
        blt("rhyper"),

        // coerce.c
        blt("as.function.default"),
        blt("typeof"),
        blt("is.vector"),
        blt("is.null"),
        blt("is.logical"),
        blt("is.integer"),
        blt("is.double"),
        blt("is.complex"),
        blt("is.character"),
        blt("is.symbol"),
        blt("is.name"),
        blt("is.environment"),
        blt("is.list"),
        blt("is.pairlist"),
        blt("is.expression"),
        blt("is.raw"),
        blt("is.object"),
        blt("isS4"),

        blt("which"),

        blt("stdout"),
        blt("stderr"),
        blt("("),
        blt("Sys.time"),

        blt("strsplit"),

        blt("seq_len"),
        blt("rep_len"),
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
        // TODO: this should be always safe, but something breaks if it is
        // moved. Need to investigate what!
        blt("is.atomic"),

        // Those are not always safe, due to coerceVector, which can be
        // overwritten by objects
        blt("vector"),
        blt("complex"),
        blt("array"),
        blt("new.env"),

        blt("dim"),
        blt("names"),

        blt("c"),
        blt("["),
        blt("[["),
        blt("+"),
        blt("-"),
        blt("*"),
        blt("/"),
        blt("^"),
        blt("%%"),
        blt("%/%"),
        blt("%*%"),
        blt("=="),
        blt("!="),
        blt("<"),
        blt("<="),
        blt(">="),
        blt(">"),
        blt("&"),
        blt("|"),
        blt("!"),
        blt("&&"),
        blt("||"),
        blt(":"),
        blt("~"),
        blt("crossprod"),
        blt("tcrossprod"),
        // Would be safe if not a vector of objects
        // blt("lengths"),
        blt("length"),
        blt("round"),
        blt("signif"),
        blt("log"),
        blt("log10"),
        blt("log2"),
        blt("abs"),
        blt("floor"),
        blt("ceiling"),
        blt("sqrt"),
        blt("sign"),
        blt("trunc"),
        blt("exp"),
        blt("expm1"),
        blt("log1p"),
        blt("cos"),
        blt("sin"),
        blt("tan"),
        blt("acos"),
        blt("asin"),
        blt("atan"),
        blt("cosh"),
        blt("sinh"),
        blt("tanh"),
        blt("acosh"),
        blt("asinh"),
        blt("atanh"),
        blt("lgamma"),
        blt("gamma"),
        blt("digamma"),
        blt("trigamma"),
        blt("cospi"),
        blt("sinpi"),
        blt("tanpi"),
        blt("atan2"),
        blt("lbeta"),
        blt("beta"),
        blt("lchoose"),
        blt("choose"),
        blt("dchisq"),
        blt("pchisq"),
        blt("qchisq"),
        blt("dexp"),
        blt("pexp"),
        blt("qexp"),
        blt("dgeom"),
        blt("pgeom"),
        blt("qgeom"),
        blt("dpois"),
        blt("ppois"),
        blt("qpois"),
        blt("dt"),
        blt("pt"),
        blt("qt"),
        blt("dsignrank"),
        blt("psignrank"),
        blt("qsignrank"),
        blt("besselJ"),
        blt("besselY"),
        blt("psigamma"),
        blt("Re"),
        blt("Im"),
        blt("Mod"),
        blt("Arg"),
        blt("Conj"),
        blt("dbeta"),
        blt("pbeta"),
        blt("qbeta"),
        blt("dbinom"),
        blt("pbinom"),
        blt("qbinom"),
        blt("dcauchy"),
        blt("pcauchy"),
        blt("qcauchy"),
        blt("df"),
        blt("pf"),
        blt("qf"),
        blt("dgamma"),
        blt("pgamma"),
        blt("qgamma"),
        blt("dlnorm"),
        blt("plnorm"),
        blt("qlnorm"),
        blt("dlogis"),
        blt("plogis"),
        blt("qlogis"),
        blt("dnbinom"),
        blt("pnbinom"),
        blt("qnbinom"),
        blt("dnorm"),
        blt("pnorm"),
        blt("qnorm"),
        blt("dunif"),
        blt("punif"),
        blt("qunif"),
        blt("dweibull"),
        blt("pweibull"),
        blt("qweibull"),
        blt("dnchisq"),
        blt("pnchisq"),
        blt("qnchisq"),
        blt("dnt"),
        blt("pnt"),
        blt("qnt"),
        blt("dwilcox"),
        blt("pwilcox"),
        blt("qwilcox"),
        blt("besselI"),
        blt("besselK"),
        blt("dnbinom_mu"),
        blt("pnbinom_mu"),
        blt("qnbinom_mu"),
        blt("dhyper"),
        blt("phyper"),
        blt("qhyper"),
        blt("dnbeta"),
        blt("pnbeta"),
        blt("qnbeta"),
        blt("dnf"),
        blt("pnf"),
        blt("qnf"),
        blt("dtukey"),
        blt("ptukey"),
        blt("qtukey"),
        blt("sum"),
        blt("min"),
        blt("max"),
        blt("prod"),
        blt("mean"),
        blt("range"),
        blt("as.character"),
        blt("as.integer"),
        blt("as.double"),
        blt("as.numeric"),
        blt("as.complex"),
        blt("as.logical"),
        blt("as.raw"),
        blt("as.vector"),

        blt("is.numeric"),
        blt("is.matrix"),
        blt("is.array"),
        blt("is.recursive"),
        blt("is.call"),
        blt("is.language"),
        blt("is.function"),
        blt("is.single"),
        blt("is.na"),
        blt("is.nan"),
        blt("is.finite"),
        blt("is.infinite"),

        blt("cumsum"),
        blt("colSums"),

        blt("cat"),
        blt("paste"),
        blt("nchar"),
        blt("match"),

        blt("seq.int"),
        blt("rep.int"),

        blt("inherits"),
        blt("anyNA")
    };

    for (auto i : safeBuiltins)
        if (i == builtin)
            return true;
    return false;
}

bool SafeBuiltinsList::nonObject(SEXP builtin) {
    return nonObject(getBuiltinNr(builtin));
}

#define UNSAFE_BUILTINS_FOR_INLINE(V)                                          \
    V(exists)                                                                  \
    V(parent.env)                                                              \
    V(parent.env<-)                                                            \
    V(sys.nframe)                                                              \
    V(lockBinding)                                                             \
    V(lockEnvironment)                                                         \
    V(unlockBinding)                                                           \
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
    V(standardGeneric)

bool SafeBuiltinsList::forInline(int builtin) {
    static int unsafeBuiltins[] = {
#define V(name) blt(#name),
        UNSAFE_BUILTINS_FOR_INLINE(V)
#undef V
    };

    for (auto i : unsafeBuiltins)
        if (i == builtin)
            return false;
    return true;
}

bool SafeBuiltinsList::forInlineByName(SEXP name) {
    SEXP unsafeBuiltins[] = {
#define V(name) Rf_install(#name),
        UNSAFE_BUILTINS_FOR_INLINE(V)
#undef V
    };

    for (auto i : unsafeBuiltins)
        if (i == name)
            return false;
    return true;
}

bool SafeBuiltinsList::assumeStableInBaseEnv(SEXP name) {
    return R_BindingIsLocked(name, R_BaseEnv) &&
           !R_BindingIsActive(name, R_BaseEnv);
}

} // namespace pir
} // namespace rir
