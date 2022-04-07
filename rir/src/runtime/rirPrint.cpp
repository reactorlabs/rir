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
    if (auto p = Code::check(s)) {
        Rprintf("<rir::Code container:%p payload:%p>\n", s, p);
    } else if (auto p = Function::check(s)) {
        Rprintf("<rir::Function container:%p payload:%p>\n", s, p);
    } else if (auto p = DispatchTable::check(s)) {
        Rprintf("<rir::DispatchTable container:%p payload:%p>\n", s, p);
    } else if (auto p = ArglistOrder::check(s)) {
        Rprintf("<rir::ArglistOrder container:%p payload:%p>\n", s, p);
    } else if (auto p = LazyArglist::check(s)) {
        Rprintf("<rir::LazyArglist container:%p payload:%p>\n", s, p);
    } else if (auto p = LazyEnvironment::check(s)) {
        Rprintf("<rir::LazyEnvironment container:%p payload:%p>\n", s, p);
    } else if (auto p = PirTypeFeedback::check(s)) {
        Rprintf("<rir::PirTypeFeedback container:%p payload:%p>\n", s, p);
    } else {
        assert(false && "missing RirRuntimeObject printing");
    }
}

} // namespace rir
