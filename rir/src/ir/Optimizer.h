#ifndef RIR_OPTIMIZER_H
#define RIR_OPTIMIZER_H

#include "ir/CodeEditor.h"

namespace rir {

class Optimizer {
  public:
    static bool cleanupRIR(CodeEditor&, int steam = 10);
    static bool tryOptimize(SEXP what, bool verbose = false);
    static SEXP reoptimizeFunction(SEXP);
};

} // namespace rir

#endif
