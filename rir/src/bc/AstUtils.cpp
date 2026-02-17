#include "AstUtils.h"
#include "R/RList.h"
#include "R/Symbols.h"

namespace rir {

bool containsLoop(SEXP exp) {
    if (TYPEOF(exp) != LANGSXP)
        return false;

    auto fun = CAR(exp);
    auto args_ = CDR(exp);

    if (TYPEOF(fun) != SYMSXP) {
        return false;
    } else if (fun == symbol::Repeat) {
        return true;
    } else if (fun == symbol::While) {
        return true;
    } else if (fun == symbol::For) {
        return true;
    }

    RList args(args_);
    bool res = false;
    for (RListIter e = args.begin(); e != args.end(); ++e) {
        res = res || containsLoop(*e);
    }
    return res;
}

} // namespace rir
