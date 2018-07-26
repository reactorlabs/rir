#pragma once

#include "../pir/module.h"
#include "runtime/Function.h"

#include <unordered_set>

namespace rir {
namespace pir {

class Pir2RirCompiler {
  public:
    bool verbose = false;
    bool dryRun = false;

    void compile(Closure* cls, SEXP origin);

  private:
    std::unordered_set<Closure*> done;
};

} // namespace pir
} // namespace rir
