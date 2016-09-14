#pragma once

#include <deque>

#include "framework.h"
#include "State.h"

namespace rir {


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
        switch (cur.bc) {
            case BC_t::brtrue_:
            case BC_t::brfalse_:
                receiver_.conditionalJump(ins.editorX().label(cur.immediate.offset));
                break;
            case BC_t::br_:
                receiver_.jump(ins.editorX().label(cur.immediate.offset));
                break;
            case BC_t::ret_:
            case BC_t::return_:
                receiver_.terminator(ins);
                break;
            default:
                fail();
        }
    }

    Receiver & receiver_;
};


// TODO for simplicity, should we make this an analysis?
class ForwardDriver : public Driver {
public:

protected:

    void clear() {
        delete currentState_;
        delete initialState_;
        delete finalState_;
        initialState_ = nullptr;
        currentState_ = nullptr;
        finalState_ = nullptr;
        for (State * s : mergePoints_)
            delete s;
        mergePoints_.clear();
    }

    void run(CodeEditor & code, State * initialState, Dispatcher & dispatcher) {
        clear();
        mergePoints_.resize(code.numLabels());
        // set current state to initial and push the first instruction in the queue
        initialState_ = initialState;
        currentState_ = initialState->clone();
        q_.push_front(code.getCursor());
        // execute the driver
        doRun(code, dispatcher);
    }


    ForwardDriver():
        cfReceiver_(*this),
        cfDispatcher_(cfReceiver_) {
    }

    State * initialState_ = nullptr;
    State * currentState_ = nullptr;
    State * finalState_ = nullptr;
    CodeEditor::Cursor currentIns_;

    std::vector<State *> mergePoints_;

private:

    class ControlFlowReceiver : public ControlFlowDispatcher::Receiver {
    public:
        void jump(CodeEditor::Cursor target);

        void conditionalJump(CodeEditor::Cursor target);

        void terminator(CodeEditor::Cursor at);

        void label(CodeEditor::Cursor at);

        ControlFlowReceiver(ForwardDriver & driver):
            driver_(driver) {
        }

    private:
        ForwardDriver & driver_;
    };

    void doRun(CodeEditor & code, Dispatcher & dispatcher) {
        while (not q_.empty()) {
            currentIns_ = q_.front();
            q_.pop_front();
            stopCurrentSequence_ = false;
            while (true) {
                BC cur = currentIns_.bc();
                // if current instruction is label, deal with state merging
                if (cur.bc == BC_t::label) {
                    // if state not stored, store copy of incomming
                    State * & stored = mergePoints_[cur.immediate.offset];
                    if (stored == nullptr) {
                        assert(currentState_ != nullptr);
                        stored = currentState_->clone();
                    } else {
                        // if incomming not present, copy stored
                        if (currentState_ == nullptr) {
                            currentState_ = stored->clone();
                        // otherwise merge incomming with stored
                        } else if (stored->mergeWith(currentState_)) {
                            delete currentState_;
                            currentState_ = stored->clone();
                        // and terminate current branch if there is no need to continue
                        } else {
                            delete currentState_;
                            currentState_ = nullptr;
                            break;
                        }
                    }
                }
                // user dispatch method
                dispatcher.dispatch(currentIns_);
                // now dispatch on the control flow
                cfDispatcher_.dispatch(currentIns_);
                // terminate current sequence if requested
                if (stopCurrentSequence_)
                    break;
                // move to next instruction
                currentIns_.advance();
            }
        }
    }

    bool shouldJump(size_t label) {
        State * & stored = mergePoints_[label];
        if (stored == nullptr) {
            stored = currentState_->clone();
            return true;
        } else {
            return stored->mergeWith(currentState_);
        }
    }

    std::deque<CodeEditor::Cursor> q_;
    bool stopCurrentSequence_;

    ControlFlowReceiver cfReceiver_;
    ControlFlowDispatcher cfDispatcher_;

};

inline void ForwardDriver::ControlFlowReceiver::jump(CodeEditor::Cursor target) {
    if (driver_.shouldJump(target.bc().immediate.offset)) {
        driver_.q_.push_front(target);
        delete driver_.currentState_;
        driver_.currentState_ = nullptr;
        driver_.stopCurrentSequence_ = true;
    }
}

inline void ForwardDriver::ControlFlowReceiver::conditionalJump(CodeEditor::Cursor target) {
    if (driver_.shouldJump(target.bc().immediate.offset)) {
        driver_.q_.push_front(target);
    }
    driver_.q_.push_front(driver_.currentIns_.next());
    driver_.stopCurrentSequence_ = true;
}

inline void ForwardDriver::ControlFlowReceiver::terminator(CodeEditor::Cursor at) {
    if (driver_.finalState_ == nullptr) {
        driver_.finalState_ = driver_.currentState_;
    } else {
        driver_.finalState_->mergeWith(driver_.currentState_);
        delete driver_.currentState_;
    }
    driver_.stopCurrentSequence_ = true;
    driver_.currentState_ = nullptr;
}

inline void ForwardDriver::ControlFlowReceiver::label(CodeEditor::Cursor at) {
    // do nothing and be happy
}

}
