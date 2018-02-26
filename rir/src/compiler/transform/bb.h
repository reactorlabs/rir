#ifndef BB_TRANSFORM_H
#define BB_TRANSFORM_H

#include "../pir/bb.h"
#include "../pir/pir.h"

namespace rir {
namespace pir {

class BBTransform {
  public:
    static BB* clone(size_t* id_counter, BB* src, Code* target);
    static BB* clone(BB* src, Code* target) {
        size_t c = 1;
        return clone(&c, src, target);
    }
    static BB* split(size_t next_id, BB* src, BB::Instrs::iterator,
                     Code* target);
    static Value* forInline(BB* inlinee, BB* cont);
};
}
}

#endif
