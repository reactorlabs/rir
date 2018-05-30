#include "pir/pir_impl.h"
#include "util/builder.h"
#include "R/Protect.h"

namespace rir {
namespace pir {

void sameBBredundnacy(pir::Closure* function) {
    Protect p;
    Builder builder(function, Env::notClosed());
    SEXP value = p(Rf_ScalarInteger(1));
    builder(new LdConst(value));
    builder(new LdConst(value));
}

}
}
