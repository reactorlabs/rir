#pragma once

#include "../pir/module.h"
#include "runtime/Function.h"

#include <unordered_map>

namespace rir {
namespace pir {

class Pir2Rir {
  public:
    rir::Function* operator()(Function* fun);
};

} // namespace pir
} // namespace rir
