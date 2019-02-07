#ifndef BB_TRANSFORM_H
#define BB_TRANSFORM_H

#include "../pir/bb.h"
#include "../pir/pir.h"
#include "../util/cfg.h"

namespace rir {
namespace pir {

class BBTransform {
  public:
    static BB* clone(BB* src, Code* target, ClosureVersion* targetClosure);
    static BB* split(BB* src, BB::Instrs::iterator);
    static Value* addDeopt(BB* src, BB::Instrs::iterator,
                           const Value* framestate);
    static Value* forInline(BB* inlinee, BB* cont);
    static BB* lowerExpect(Code* closure, BB* src,
                           BB::Instrs::iterator position, Value* condition,
                           bool expected, BB* deoptBlock,
                           const std::string& debugMesage);
    static void removeBBs(Code* code, const std::unordered_set<BB*>& toDelete);
    static Value* insertAssume(BB* src, Instruction* condition,
                               Value* checkpoint,
                               BB::Instrs::iterator& position, bool assumeType);
};
} // namespace pir
} // namespace rir

#endif
