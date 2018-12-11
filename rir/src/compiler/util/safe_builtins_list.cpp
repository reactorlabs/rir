#include "safe_builtins_list.h"

#include "R/Funtab.h"

namespace rir {
namespace pir {

bool CallSafeBuiltinsList::contains(SEXP builtin) {
    static int safeBuiltins[] = {
        findBuiltin("vector"),
        findBuiltin("c"),
    };

    for (auto i : safeBuiltins)
        if (i == getBuiltinNr(builtin))
            return true;
    return false;
}

} // namespace pir
} // namespace rir
