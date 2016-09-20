#pragma once

#include "ir/BC.h"
#include "ir/CodeEditor.h"

namespace rir {


/** Dispatcher prototype.

  Dispatchers are used to determine the current situation and execute the appropriate code. Each dispatcher must override the doDispatch() method. If the dispatch fails, the dispatcher is supposed to call the fail() method, so that the dispatch() can return proper result.

  Note that failing the dispatch does not necessarily mean an error, it merely signifies that the dispatcher did not recognize the situation, or that the code dispatched to wanted to override the failure.
  */

class Dispatcher {
public:
    ~Dispatcher() {
    }

    /** Dispatches on the given cursor, and returns true if the dispatch was successful, false if not.
     */
    bool dispatch(CodeEditor::Cursor & ins) {
        success_ = true;
        doDispatch(ins);
        return success_;
    }
protected:

    Dispatcher() = default;

    /** Called by actual dispatchers when they want to notify the dispatching that it has failed.

      When this method is called from a dispatched routine, the dispatch() method will then return false.
     */
    void fail() {
        success_ = false;
    }

private:

    /** Actual dispatch code.

      Must be implemented in children.
     */
    virtual void doDispatch(CodeEditor::Cursor & ins) = 0;

    bool success_;
};

class ControlFlowDispatcher : public Dispatcher {
public:
    class Receiver {
    public:
        virtual void jump(CodeEditor::Cursor target) = 0;

        virtual void conditionalJump(CodeEditor::Cursor target) = 0;

        virtual void terminator(CodeEditor::Cursor at) = 0;

        virtual void label(CodeEditor::Cursor at) = 0;

        virtual ~Receiver() {
        }
    };

    ControlFlowDispatcher(Receiver & receiver):
        receiver_(receiver) {
   }

private:
    void doDispatch(CodeEditor::Cursor & ins) override {
        BC cur = ins.bc();

        if (cur.bc == BC_t::br_) {
            receiver_.jump(ins.editorX().label(cur.immediate.offset));
        } else if (cur.isJmp()) {
            receiver_.conditionalJump(
                ins.editorX().label(cur.immediate.offset));
        } else if (cur.bc == BC_t::ret_ || cur.bc == BC_t::return_) {
            receiver_.terminator(ins);
        } else {
            fail();
        }
    }

    Receiver & receiver_;
};

}



