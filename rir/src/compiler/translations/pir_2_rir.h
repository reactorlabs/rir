#pragma once

#include "../pir/module.h"
#include "runtime/Function.h"

#include <unordered_map>

namespace rir {
namespace pir {

class Pir2RirCompiler {
  public:
    rir::Function* operator()(Module* fun);
};

} // namespace pir
} // namespace rir
