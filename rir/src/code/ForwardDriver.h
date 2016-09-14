#pragma once

#include <deque>

#include "framework.h"
#include "State.h"

namespace rir {


class ControlFlowDispatcher : public Dispatcher {
public:
    class Receiver {
    public:
        virtual void jump(Cursor target) = 0;

        virtual void conditionalJump(Cursor target) = 0;

        virtual void terminator(Cursor at) = 0;

        virtual void label(Cursor at) = 0;

        virtual ~Receiver() {
        }
    };

    ControlFlowDispatcher(Receiver & receiver):
        receiver_(receiver) {
   }

private:
    void doDispatch(Cursor & ins) override {
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

    void run(Code & code, State * initialState, Dispatcher & dispatcher) {
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
    Cursor currentIns_;

    std::vector<State *> mergePoints_;

private:

    class ControlFlowReceiver : public ControlFlowDispatcher::Receiver {
    public:
        void jump(Cursor target);

        void conditionalJump(Cursor target);

        void terminator(Cursor at);

        void label(Cursor at);

        ControlFlowReceiver(ForwardDriver & driver):
            driver_(driver) {
        }

    private:
        ForwardDriver & driver_;
    };

    void doRun(Code & code, Dispatcher & dispatcher) {
        while (not q_.empty()) {
            currentIns_ = q_.front();
            q_.pop_front();
            stopCurrentSequence_ = false;
            while (true) {
                BC cur = currentIns_.bc();
                // if current instruction is label, get set incomming state to stored
                if (cur.bc == BC_t::label) {
                    State * & stored = mergePoints_[cur.immediate.offset];
                    if (stored == nullptr) {
                        assert(currentState_ != nullptr);
                        stored = currentState_->clone();
                    } else {
                        delete currentState_; // just to be sure
                        currentState_ = stored->clone();
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

    std::deque<Cursor> q_;
    bool stopCurrentSequence_;

    ControlFlowReceiver cfReceiver_;
    ControlFlowDispatcher cfDispatcher_;

};

inline void ForwardDriver::ControlFlowReceiver::jump(Cursor target) {
    if (driver_.shouldJump(target.bc().immediate.offset)) {
        driver_.q_.push_front(target);
        delete driver_.currentState_;
        driver_.currentState_ = nullptr;
        driver_.stopCurrentSequence_ = true;
    }
}

inline void ForwardDriver::ControlFlowReceiver::conditionalJump(Cursor target) {
    if (driver_.shouldJump(target.bc().immediate.offset)) {
        driver_.q_.push_front(target);
    }
    driver_.q_.push_front(driver_.currentIns_.next());
    driver_.stopCurrentSequence_ = true;
}

inline void ForwardDriver::ControlFlowReceiver::terminator(Cursor at) {
    if (driver_.finalState_ == nullptr) {
        driver_.finalState_ = driver_.currentState_;
    } else {
        driver_.finalState_->mergeWith(driver_.currentState_);
        delete driver_.currentState_;
    }
    driver_.stopCurrentSequence_ = true;
    driver_.currentState_ = nullptr;
}

inline void ForwardDriver::ControlFlowReceiver::label(Cursor at) {
    // do nothing and be happy
}

}
