#ifndef MACHINE_STATE_H
#define MACHINE_STATE_H

#include "../../pir/bb.h"
#include "../../pir/instruction.h"
#include "../../util/builder.h"
#include "runtime/Function.h"
#include <queue>

namespace rir {
namespace pir {

class Rir2Pir;

class StackMachine {
  public:
    StackMachine(rir::Function* srcFunction, rir::Code* srcCode)
        : srcFunction(srcFunction), srcCode(srcCode), pc(srcCode->code()) {}
    StackMachine(rir::Function* srcFunction, rir::Code* srcCode, Opcode* pc)
        : srcFunction(srcFunction), srcCode(srcCode), pc(pc) {}

    Opcode* getPC();
    void setPC(Opcode*);
    bool atEnd() { return pc == srcCode->endCode(); }

    pir::BB* getEntry();
    void setEntry(pir::BB*);

    bool doMerge(Opcode*, Builder&, StackMachine*);

    typedef std::pair<BB*, Value*> ReturnSite;

    bool tryRunCurrentBC(const Rir2Pir& cmp, Builder&)
        __attribute__((warn_unused_result));

    void clear();
    bool empty();
    size_t stack_size();
    Value* front();
    Value* top();
    void push(Value*);
    Value* pop();
    BC getCurrentBC();
    void advancePC();
    unsigned getSrcIdx();
    std::deque<Value*> stack;

  private:
    rir::Function* srcFunction;
    rir::Code* srcCode;
    Opcode* pc;
    pir::BB* entry = nullptr;
    Value* at(size_t);
    void set(size_t n, Value* v);
};

} // namespace pir
} // namespace rir

#endif
