#ifndef BB_TRANSFORM_H
#define BB_TRANSFORM_H

#include "../pir/bb.h"
#include "../pir/pir.h"

namespace rir {
namespace pir {

class Safepoint;
class BBTransform {
  public:
    static BB* clone(BB* src, Code* target);
    static BB* split(size_t next_id, BB* src, BB::Instrs::iterator,
                     Code* target);
    static Value* forInline(BB* inlinee, BB* cont);
    static void addConditionalDeopt(Closure* closure, BB* src,
                                    BB::Instrs::iterator position,
                                    Instruction* condition,
                                    Safepoint* safepoint);
};
} // namespace pir
} // namespace rir

#endif
