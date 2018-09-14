#ifndef COMPILER_BUILDER_H
#define COMPILER_BUILDER_H

#include "../pir/bb.h"
#include "../pir/pir.h"
#include "../pir/tag.h"
#include "ir/BC_inc.h"

#include <deque>
#include <functional>

namespace rir {
struct Code;

namespace pir {

class Safepoint;
class CallInstruction;
struct RirStack;

class Builder {
  public:
    Closure* function;
    Code* code;
    Value* env;

    Builder(Closure* fun, Promise* prom);
    Builder(Closure* fun, Value* enclos);

    Value* buildDefaultEnv(Closure* fun);

    void add(Instruction* i);
    template <class T>
    T* operator()(T* i) {
        add(i);
        return i;
    }

    BB* createBB();
    void createNextBB();
    void enterBB(BB* bb);
    void setNext(BB* bb1);
    void setBranch(BB* bb1, BB* bb2);

    Safepoint* registerSafepoint(rir::Code* srcCode, Opcode* pos,
                                 const RirStack& stack);
    void conditionalDeopt(Value* condition, rir::Code* srcCode, Opcode* pos,
                          const RirStack& stack, bool deoptOnFalseBranch);

    // Use with care, let the builder keep track of BB. Prefer the highlevel
    // api above.
    BB* getCurrentBB() const { return bb; }
    // Use with even more care, no checks
    void reenterBB(BB* bb) { this->bb = bb; }

  private:
    void markDone(BB*);
    bool isDone(BB*) const;

    std::vector<bool> done;
    BB* bb = nullptr;
};

} // namespace pir
} // namespace rir

#endif
