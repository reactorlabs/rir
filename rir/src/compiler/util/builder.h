#ifndef COMPILER_BUILDER_H
#define COMPILER_BUILDER_H

#include "../pir/bb.h"
#include "../pir/pir.h"
#include "../pir/tag.h"
#include "ir/BC_inc.h"

#include <deque>
#include <functional>

namespace rir {
namespace pir {

class Safepoint;
struct RirStack;

class Builder {
  public:
    Closure* function;
    Code* code;
    Value* env;

    Builder(Closure* fun, Promise* prom);
    Builder(Closure* fun, Value* enclos);

    Value* buildDefaultEnv(Closure* fun);

    template <class T>
    T* add(T* i) {
        assert(i->tag != Tag::_UNUSED_);
        bb->append(i);
        return i;
    }
    template <class T>
    T* operator()(T* i) {
        return add(i);
    }

    void markDone(BB*);
    bool isDone(BB*);

    BB* createBB();
    void createNextBB();
    void enterBB(BB* bb);
    void setNext(BB* bb1);
    void setBranch(BB* bb1, BB* bb2);

    void deoptUnless(Value* condition, rir::Code* srcCode, Opcode* pos,
                     const RirStack& stack);

    // Use with care, let the builder keep track of BB. Prefer the highlevel
    // api above.
    BB* getCurrentBB() { return bb; }
    // Use with even more care, no checks
    void reenterBB(BB* bb) { this->bb = bb; }

  private:
    std::vector<bool> done;
    BB* bb = nullptr;
};

} // namespace pir
} // namespace rir

#endif
