#ifndef BB_TRANSFORM_H
#define BB_TRANSFORM_H

#include "../pir/bb.h"
#include "../pir/pir.h"
#include "../pir/values.h"
#include "compiler/analysis/cfg.h"
#include "runtime/TypeFeedback.h"

namespace rir {

enum class Opcode : uint8_t;
struct Code;

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
    static Value* forInline(BB* inlinee, BB* cont, Value* context,
                            Checkpoint* entryCp);
    static BB* lowerExpect(Module* m, Code* closure, BB* src,
                           BB::Instrs::iterator position, Assume* assume,
                           bool condition, BB* deoptBlock,
                           const std::string& debugMesage,
                           bool triggerAnyway = false);
    static void insertAssume(Instruction* condition, bool assumePositive,
                             Checkpoint* cp, const FeedbackOrigin& origin,
                             DeoptReason::Reason reason, BB* bb,
                             BB::Instrs::iterator& position);
    static void insertAssume(Instruction* condition, bool assumePositive,
                             Checkpoint* cp, const FeedbackOrigin& origin,
                             DeoptReason::Reason reason);

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
