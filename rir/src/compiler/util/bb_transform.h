#ifndef BB_TRANSFORM_H
#define BB_TRANSFORM_H

#include "../pir/bb.h"
#include "../pir/pir.h"
#include "compiler/analysis/cfg.h"

namespace rir {

enum class Opcode : uint8_t;

namespace pir {

class FrameState;
class Checkpoint;
class Assume;

class BBTransform {
  public:
    static BB* clone(BB* src, Code* target, ClosureVersion* targetClosure);
    static BB* splitEdge(size_t next_id, BB* from, BB* to, Code* target);
    static BB* split(size_t next_id, BB* src, BB::Instrs::iterator,
                     Code* target);
    static std::pair<Value*, BB*> forInline(BB* inlinee, BB* cont,
                                            Value* context);
    static BB* lowerExpect(Code* closure, BB* src,
                           BB::Instrs::iterator position, Assume* assume,
                           bool condition, BB* deoptBlock,
                           const std::string& debugMesage,
                           bool triggerAnyway = false);
    static void insertAssume(Instruction* condition, Checkpoint* cp, BB* bb,
                             BB::Instrs::iterator& position,
                             bool assumePositive, rir::Code* srcCode,
                             Opcode* origin = nullptr);
    static void insertAssume(Instruction* condition, Checkpoint* cp,
                             bool assumePositive, rir::Code* srcCode,
                             Opcode* origin = nullptr);

    static void mergeRedundantBBs(Code* closure);

    // Renumber in dominance order. This ensures that controlflow always goes
    // from smaller id to bigger id, except for back-edges.
    static void renumber(Code* fun);

    // Remove dead instructions (instead of waiting until the cleanup pass).
    // Note that a phi is dead if it is not used by any instruction, or it is
    // used only by dead phis.
    static void removeDeadInstrs(Code* fun, uint8_t maxBurstSize);

    static void removeDeadBlocks(Code* fun,
                                 const std::unordered_set<BB*>& dead);
};

} // namespace pir
} // namespace rir

#endif
