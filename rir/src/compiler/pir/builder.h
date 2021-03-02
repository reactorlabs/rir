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

class FrameState;
class Checkpoint;
class CallInstruction;
struct RirStack;

class Builder {
  public:
    ClosureVersion* function;
    Code* code;
    Value* env;

    Builder(ClosureVersion* fun, Promise* prom);
    Builder(ClosureVersion* fun, Value* enclos);

    Value* buildDefaultEnv(ClosureVersion* fun);

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

    FrameState* registerFrameState(rir::Code* srcCode, Opcode* pos,
                                   const RirStack& stack, bool inPromise);
    Checkpoint* emitCheckpoint(rir::Code* srcCode, Opcode* pos,
                               const RirStack& stack, bool inPromise);
    Checkpoint* emitCheckpoint(FrameState* fs);

    // Use with care, let the builder keep track of BB. Prefer the highlevel
    // api above.
    BB* getCurrentBB() const { return bb; }
    // Use with even more care, no checks
    void reenterBB(BB* bb) { this->bb = bb; }
    void clearCurrentBB() { this->bb = nullptr; }

  private:
    void markDone(BB*);
    bool isDone(BB*) const;

    std::vector<bool> done;
    BB* bb = nullptr;
};

} // namespace pir
} // namespace rir

#endif
