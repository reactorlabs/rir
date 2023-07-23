#pragma once

#include "R/r.h"
#include "hash/UUID.h"

namespace rir {

/// Create a UUID from only the AST part of a SEXP.
UUID hashAst(SEXP s);

} // namespace rir
