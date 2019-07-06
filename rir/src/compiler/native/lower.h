#ifndef PIR_NATIVE_LOWER_H
#define PIR_NATIVE_LOWER_H

#include "compiler/pir/pir.h"
#include "runtime/Code.h"

#include "jit/jit-plus.h"
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace rir {
namespace pir {

class Lower {
  public:
    void* tryCompile(ClosureVersion* cls, Code* code,
                     const std::unordered_map<Promise*, unsigned>&,
                     const std::unordered_set<Instruction*>& needsEnsureNamed);
};
}
}

#endif
