#pragma once

#include "../debugging.h"
#include "../pir/module.h"
#include "../pir/pir.h"
#include "runtime/Function.h"

#include <unordered_set>

namespace rir {
namespace pir {

class Pir2RirCompiler {
  public:
    Pir2RirCompiler(const DebugOptions& debug) : debug(debug) {}

    const DebugOptions debug;

    void compile(Closure* cls, SEXP origin);

  private:
    std::unordered_set<Closure*> done;
};

} // namespace pir
} // namespace rir
