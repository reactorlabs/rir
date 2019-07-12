#ifndef PIR_DEAD_H
#define PIR_DEAD_H

#include "compiler/pir/pir.h"
#include <unordered_set>

namespace rir {
namespace pir {

class DeadInstructions {
    std::unordered_set<Instruction*> used_;

  public:
    explicit DeadInstructions(Code*);
    bool used(Value* v);
    bool used(Instruction* v);
    bool unused(Value* v);
    bool unused(Instruction* v);
};

} // namespace pir
} // namespace rir

#endif
