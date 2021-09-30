#include "interpreter/interp_incl.h"
#include "runtime/ArglistOrder.h"
#include "runtime/Code.h"
#include "runtime/DispatchTable.h"
#include "runtime/Function.h"
#include "runtime/LazyArglist.h"
#include "runtime/LazyEnvironment.h"
#include "runtime/PirTypeFeedback.h"

#include <R/r.h>
#include <assert.h>

namespace rir {

void rirPrint(SEXP s) {
    // Print the representation of s to std::cout
    assert(TYPEOF(s) == EXTERNALSXP);

    // TODO: print more info / contents
    if (Code::check(s)) {
        Rprintf("<rir::Code: %p>\n", s);
    } else if (Function::check(s)) {
        Rprintf("<rir::Function: %p>\n", s);
    } else if (DispatchTable::check(s)) {
        Rprintf("<rir::DispatchTable: %p>\n", s);
    } else if (ArglistOrder::check(s)) {
        Rprintf("<rir::ArglistOrder: %p>\n", s);
    } else if (LazyArglist::check(s)) {
        Rprintf("<rir::LazyArglist: %p>\n", s);
    } else if (LazyEnvironment::check(s)) {
        Rprintf("<rir::LazyEnvironment: %p>\n", s);
    } else if (PirTypeFeedback::check(s)) {
        Rprintf("<rir::PirTypeFeedback: %p>\n", s);
    } else {
        assert(false && "missing RirRuntimeObject printing");
    }
}

}; // namespace rir
