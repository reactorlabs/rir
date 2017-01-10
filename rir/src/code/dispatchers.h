#pragma once

#include "framework.h"

namespace rir {

class InstructionDispatcher : public Dispatcher {
  public:
    /** Receiver implements the visitor pattern over instructions.

      It contains a virtial any() method used for generic instruction and for
      each opcode virtual method of the opcode's name that by default calls to
      any().

      TODO this hierarchy can be done better.
      */
    class Receiver {
      public:
        virtual void any(CodeEditor::Iterator ins) {}

        virtual void label(CodeEditor::Iterator ins) { any(ins); }

#define DEF_INSTR(NAME, ...)                                                   \
    virtual void NAME(CodeEditor::Iterator ins) { any(ins); }
#include "ir/insns.h"

        virtual ~Receiver() {}
    };

    /** The dispatcher must be initialized with the appropriate receiver.

      NOTE: alternatively the dispatcher base class must be templated with the
      receiver, which would then become argument to doDispatch().
     */
    InstructionDispatcher(Receiver& receiver) : receiver_(receiver) {}

  private:
    /** Dispatches on the given instruction.

      If the instruction's opcode is not handler by the receiver, an assertion
      fails as the contract of instruction visitor is that it understand *all*
      instructions.
     */
    void doDispatch(CodeEditor::Iterator ins) override {
        BC cur = *ins;
        switch (cur.bc) {
#define DEF_INSTR(NAME, ...)                                                   \
    case Opcode::NAME:                                                         \
        receiver_.NAME(ins);                                                   \
        break;
#include "ir/insns.h"
        case Opcode::label:
            receiver_.label(ins);
            break;
        default:
            assert(false and "Invalid instruction opcode");
        }
    }

    Receiver& receiver_;
};
}
