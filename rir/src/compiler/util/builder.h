#ifndef COMPILER_BUILDER_H
#define COMPILER_BUILDER_H

#include "../pir/bb.h"
#include "../pir/pir.h"
#include "../pir/tag.h"

namespace rir {
namespace pir {

class Builder {
  public:
    Closure* function;
    Code* code;
    Value* env;
    BB* bb;
    Builder(Closure* fun, Promise* prom);
    Builder(Closure* fun, Value* enclos);

    Value* buildDefaultEnv(Closure* fun);

    template <class T>
    T* operator()(T* i) {
        assert(i->tag != Tag::_UNUSED_);
        bb->append(i);
        return i;
    }

    BB* createBB();

    void next(BB* b) {
        bb->next0 = b;
        bb = b;
    }
};

} // namespace pir
} // namespace rir

#endif
