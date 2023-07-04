#pragma once

#include "R/r.h"
#include "hash/UUID.h"

namespace rir {

/// Serialize only the AST part of an S-expression
void serializeAst(UUIDHasher& bb, SEXP s);
/// Serialize only the AST part of an S-expression
UUID serializeAst(SEXP s);

} // namespace rir
