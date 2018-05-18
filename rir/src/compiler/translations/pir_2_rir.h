#pragma once

#include "../pir/module.h"
#include "runtime/Function.h"

namespace rir {
namespace pir {

class Pir2RirCompiler {
  public:
    bool verbose = false;

    rir::Function* compile(Closure* cls, SEXP origin);
};

} // namespace pir
} // namespace rir
