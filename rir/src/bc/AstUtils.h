#ifndef RIR_AST_UTILS_H
#define RIR_AST_UTILS_H

#include "R/r.h"

namespace rir {

bool containsLoop(SEXP exp);

// Count AST nodes (proxy for lines of code). Stops counting at limit to
// avoid traversing large functions unnecessarily.
int countNodes(SEXP exp, int limit);

} // namespace rir

#endif
