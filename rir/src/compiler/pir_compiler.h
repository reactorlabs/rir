#ifndef PIR_COMPILER_H
#define PIR_COMPILER_H

#include "R/RList.h"
#include "pir/module.h"
#include "runtime/Function.h"

namespace rir {

class PirCompiler {
  public:
    pir::Module* m;
    pir::Module* compileFunction(SEXP, bool verbose = false);
};
}

#endif
