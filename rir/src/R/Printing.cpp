#include "Printing.h"

#include "R/Funtab.h"
#include <iostream>
#include <string>

namespace rir {

std::string Print::dumpSexp(SEXP src, size_t length) {
    if (src == R_UnboundValue) {
        return "R_UnboundValue";
    } else if (src == R_MissingArg) {
        return "R_MissingArg";
    }

    static auto deparseBlt = getBuiltinFun("deparse");
    static auto deparse = getBuiltin(deparseBlt);

    auto args = CONS_NR(
        src, CONS_NR(Rf_ScalarInteger(length),
                     CONS_NR(R_MissingArg,
                             CONS_NR(R_MissingArg, CONS_NR(Rf_ScalarInteger(1),
                                                           R_NilValue)))));
    PROTECT(args);
    auto res = deparse(R_NilValue, deparseBlt, args, R_BaseEnv);
    auto str = std::string(CHAR(STRING_ELT(res, 0)));
    UNPROTECT(1);

    return str;
}

} // namespace rir
