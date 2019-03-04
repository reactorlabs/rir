#ifndef BB_TRANSFORM_H
#define BB_TRANSFORM_H

#include "../pir/bb.h"
#include "../pir/pir.h"
#include "../util/cfg.h"

namespace rir {
namespace pir {

class FrameState;
class Checkpoint;
class BBTransform {
  public:
    static BB* clone(BB* src, Code* target, ClosureVersion* targetClosure);
    static BB* splitEdge(size_t next_id, BB* from, BB* to, Code* target);
    static BB* split(size_t next_id, BB* src, BB::Instrs::iterator,
                     Code* target);
    static std::pair<Value*, BB*> forInline(BB* inlinee, BB* cont);
    static BB* lowerExpect(Code* closure, BB* src,
                           BB::Instrs::iterator position, Value* condition,
                           bool expected, BB* deoptBlock,
                           const std::string& debugMesage);
    static void insertAssume(Value* condition, Checkpoint* cp, BB* bb,
                             BB::Instrs::iterator& position,
                             bool assumePositive);
    static void insertAssume(Value* condition, Checkpoint* cp,
                             bool assumePositive);
};

} // namespace pir
} // namespace rir

#endif
