#ifndef RIR_OPTIMIZER_H
#define RIR_OPTIMIZER_H

#include "ir/CodeEditor.h"

namespace rir {

class Optimizer {
  public:
    static bool optimize(CodeEditor&, int steam = 10);
    static SEXP reoptimizeFunction(SEXP);
};
}

#endif
