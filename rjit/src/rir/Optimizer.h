#ifndef RIR_OPTIMIZER_H
#define RIR_OPTIMIZER_H

#include "../Symbols.h"
#include "BC.h"
#include "CodeStream.h"
#include <cassert>

namespace rjit {
namespace rir {

class Optimizer {
  public:
    static void optimize(Code* fun) {}
};

} // rir
} // rjit

#endif
