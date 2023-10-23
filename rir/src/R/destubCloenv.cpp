//
// Created by Jakob Hain on 10/23/23.
//

#include "destubCloenv.h"
#include "R/Symbols.h"
#include "R/r.h"

namespace rir {

SEXP destubCloenv(SEXP closure) {
    return CLOENV(closure) == symbol::closureEnvStub
               ? R_GlobalEnv
               : CLOENV(closure);
}

} // namespace rir