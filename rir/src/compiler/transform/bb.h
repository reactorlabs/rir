#ifndef BB_TRANSFORM_H
#define BB_TRANSFORM_H

#include "../pir/bb.h"
#include "../pir/pir.h"

namespace rir {
namespace pir {

class FrameState;
class BBTransform {
  public:
    static BB* clone(BB* src, Code* target);
    static BB* split(size_t next_id, BB* src, BB::Instrs::iterator,
                     Code* target);
    static Value* forInline(BB* inlinee, BB* cont);
    static BB* addConditionalDeopt(Closure* closure, BB* src,
                                   BB::Instrs::iterator position,
                                   Instruction* condition,
                                   FrameState* frameState);
    static BB* addCheckpoint(Closure* closure, BB* src,
                             BB::Instrs::iterator position);
};
} // namespace pir
} // namespace rir

#endif
