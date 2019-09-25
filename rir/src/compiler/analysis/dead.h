#ifndef PIR_DEAD_H
#define PIR_DEAD_H

#include "compiler/pir/pir.h"
#include "compiler/pir/tag.h"
#include <unordered_set>

namespace rir {
namespace pir {

class DeadInstructions {
    std::unordered_set<Instruction*> used_;

  public:
    enum DeadInstructionsMode {
        CountAll,
        IgnoreUpdatePromise,
    };

    DeadInstructions(Code*, DeadInstructionsMode mode = CountAll);
    bool used(Value* v);
    bool used(Instruction* v);
    bool unused(Value* v);
    bool unused(Instruction* v);
};

} // namespace pir
} // namespace rir

#endif
