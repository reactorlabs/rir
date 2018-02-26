#ifndef COMPILER_BUILDER_H
#define COMPILER_BUILDER_H

#include "../pir/bb.h"
#include "../pir/pir.h"

namespace rir {
namespace pir {

class Builder {
  public:
    Function* function;
    Code* code;
    Value* env;
    BB* bb;
    Builder(Function* fun, Promise* prom);
    Builder(Function* fun, Env* enclos);

    Value* buildDefaultEnv(Function* fun);

    template <class T>
    T* operator()(T* i) {
        bb->append(i);
        i->bb_ = bb;
        return i;
    }

    BB* createBB();

    void next(BB* b) {
        bb->next0 = b;
        bb = b;
    }
};
}
}

#endif
