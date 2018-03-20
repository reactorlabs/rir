#ifndef MACHINE_STATE_H
#define MACHINE_STATE_H

#include "instruction.h"
#include "bb.h"
#include "runtime/Function.h"
#include "../util/builder.h"
#include <queue>

namespace rir {
namespace pir {

typedef std::pair<BB*, Value*> ReturnSite;

class StackMachine {
    public:
      StackMachine() : stack(), currentBC(), pc(), entry() {}

      Opcode* getPC();
      void setPC(Opcode*);
      pir::BB* getEntry();
      void setEntry(pir::BB*);
      bool doMerge(Opcode*, Builder*, StackMachine*);

      void runCurrentBC(Builder*, rir::Function*, std::vector<ReturnSite>*);
      void clear();
      bool empty();
      size_t stack_size();
      Value* front();
      Value* top();
      void push(Value*);
      Value* pop();
      BC getCurrentBC();
      
    private:
      std::deque<Value*> stack;
      BC currentBC;
      Opcode* pc;
      pir::BB* entry = nullptr;
      Value* at(size_t);
      void set(size_t n, Value* v);
      void advancePC();
      void decodeCurrentBytecode();
      void consumeBC(Opcode* end);
};
}
}
#endif