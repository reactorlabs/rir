#include "R/Printing.h"
#include "R/r.h"

#include <assert.h>

namespace rir {

// Print the representation of s to std::cout
void rirPrint(SEXP s) {
    assert(TYPEOF(s) == EXTERNALSXP);
    Rprintf(Print::dumpSexp(s).c_str());
    Rprintf("\n");
}

} // namespace rir
