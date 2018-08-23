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

class Builder {
  public:
    Closure* function;
    Code* code;
    Value* env;

    Builder(Closure* fun, Promise* prom);
    Builder(Closure* fun, Value* enclos);

    Value* buildDefaultEnv(Closure* fun);

    template <class T>
    T* operator()(T* i) {
        assert(i->tag != Tag::_UNUSED_);
        bb->append(i);
        return i;
    }

    void markDone(BB*);
    bool isDone(BB*);

    BB* createBB();
    void createNextBB();
    void enterBB(BB* bb);
    void setNextBB(BB* bb1, BB* bb2 = nullptr);

    typedef std::function<void()> BBCompile;
    void ifThenElse(BBCompile ifblock, BBCompile thenblock);

    void deopt(rir::Code* srcCode, Opcode* pos,
               const std::deque<Value*>& stack);

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
