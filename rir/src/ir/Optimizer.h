#ifndef RIR_OPTIMIZER_H
#define RIR_OPTIMIZER_H

#include "ir/CodeEditor.h"

namespace rir {

class Optimizer {
  public:
    static void optimize(CodeEditor&, int steam = 3);
    static void inliner(CodeEditor&);
    static SEXP reoptimizeFunction(SEXP);
};
}

#endif
