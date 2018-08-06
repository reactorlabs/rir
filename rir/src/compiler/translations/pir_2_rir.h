#pragma once

#include "../pir/module.h"
#include "../pir/pir.h"
#include "runtime/Function.h"

#include <unordered_set>

namespace rir {
namespace pir {

class Pir2RirCompiler {
  public:
    uint32_t verbose = 0;
    bool dryRun = false;

    void compile(Closure* cls, SEXP origin);

    bool shouldPrintCSSA() { return verbose & PRINT_CSSA_MASK; }
    bool shouldPrintAllocations() { return verbose & PRINT_LIVENESS_MASK; }
    bool shouldPrintLiveness() { return verbose & PRINT_STACK_MASK; }
    bool shouldPrintRIRAfterPIR() { return verbose & PRINT_FINAL_RIR_MASK; }
    bool isVerbose() { return verbose; }

  private:
    std::unordered_set<Closure*> done;
};

} // namespace pir
} // namespace rir
