#include "Symbols.h"

namespace rir {
namespace symbol {
namespace {

// Copy-paste from names.c
// Bypass Rf_install to avoid having this in the symbol table - there shouldn't
// be any way for users to get to this symbol
SEXP mkSymMarker(SEXP pname) {
    PROTECT(pname);
    SEXP ans = allocSExp(SYMSXP);
    SET_SYMVALUE(ans, ans);
    SET_ATTRIB(ans, R_NilValue);
    SET_PRINTNAME(ans, pname);
    UNPROTECT(1);
    return ans;
}

} // namespace

#define V(name, txt) SEXP name = Rf_install(txt);
SYMBOLS(V)
#undef V

SEXP expandDotsTrigger = mkSymMarker(Rf_mkChar(".expandDotsTrigger."));

} // namespace symbol
} // namespace rir
