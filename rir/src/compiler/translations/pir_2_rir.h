#pragma once

#include "../pir/module.h"
#include "runtime/Function.h"

#include <unordered_set>

namespace rir {
namespace pir {

class Pir2RirCompiler {
  public:
    uint verbose = 0;
    bool dryRun = false;

    void compile(Closure* cls, SEXP origin);

    bool shouldPrintCSSA() { return verbose & 0X10000; }
    bool shouldPrintAllocations() { return verbose & 0X100000; }
    bool shouldPrintLiveness() { return verbose & 0X1000000; }
    bool shouldPrintRIRAfterPIR() { return verbose & 0X10000000; }
    bool isVerbose() { return verbose; }

  private:
    std::unordered_set<Closure*> done;
};

} // namespace pir
} // namespace rir
