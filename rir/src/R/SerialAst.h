#pragma once

#include "R/r.h"

#include <string>

namespace rir {

/// Serialize only the AST part of an S-expression
void serializeAst(R_outpstream_t out, SEXP s);

} // namespace rir
