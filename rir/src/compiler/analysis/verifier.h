#ifndef COMPILER_PIR_VERIFIER_H
#define COMPILER_PIR_VERIFIER_H

#include "../pir/pir.h"

namespace rir {
namespace pir {

class Verify {
  public:
    static bool apply(ClosureVersion*, bool slow = false);
};
}
}

#endif
