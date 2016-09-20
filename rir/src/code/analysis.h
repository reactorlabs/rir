#pragma once

#include "State.h"
#include "framework.h"

namespace rir {

/** Base class for all analyses.

  In the future, this is where the API for analysis scheduling, retrieval and invalidation will live.
 */
class Analysis {
public:

    /** Analyzes the given code.

      Internally sets the state and then calls the doAnalyze() virtual method that should implement the actual analysis.
     */
    void analyze(CodeEditor & code) {
        if (code_ != nullptr)
            invalidate();
        code_ = & code;
        doAnalyze();
    }

    /** Invalidates the data computed by the analysis.

      This is to be overwritten in child classes to proviode the appropriate cleanup functionality.
     */
    virtual void invalidate() {
        code_ = nullptr;
    }

    /** Returns true if the analysis contains valid data.
     */
    bool good() const {
        return code_ != nullptr;
    }

    /** Override this for pretty printing.
     */
    virtual void print() = 0;

protected:

    /** Override this method to provide the actual analysis.

      This should mostly mean invoking the appropriate driver and collecting the state.
     */

    virtual void doAnalyze() = 0;

    CodeEditor * code_ = nullptr;


};


/** Forward driver analysis updates the basic analysis API with forward driver.

  ForwardAnalysis does not deal with visibility of any results, that is the domain of its descelndants.
 */
template<typename ASTATE>
class ForwardAnalysis : public Analysis {
public:

    void invalidate() override {
        Analysis::invalidate();
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

    void print() override {

    }

protected:
    ForwardAnalysis():
        cfReceiver_(*this),
        cfDispatcher_(cfReceiver_) {
    }

    ASTATE & current() {
        return * reinterpret_cast<ASTATE *>(currentState_);
    }

    virtual Dispatcher & dispatcher() = 0;

    virtual ASTATE * initialState() {
        return new ASTATE();
    }

    void doAnalyze() override {
        mergePoints_.resize(code_->numLabels());
        initialState_ = initialState();
        currentState_ = initialState_->clone();
        q_.push_front(code_->getCursor());
        Dispatcher & d = dispatcher();
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
                d.dispatch(currentIns_);
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

        ControlFlowReceiver(ForwardAnalysis & driver):
            a_(driver) {
        }

    private:
        ForwardAnalysis & a_;
    };

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

template<typename ASTATE>
void ForwardAnalysis<ASTATE>::ControlFlowReceiver::jump(CodeEditor::Cursor target) {
    Label l = target.bc().immediate.offset;
    if (a_.shouldJump(l)) {
        a_.q_.push_front(target);
    }
    delete a_.currentState_;
    a_.currentState_ = nullptr;
    a_.stopCurrentSequence_ = true;
}

template<typename ASTATE>
void ForwardAnalysis<ASTATE>::ControlFlowReceiver::conditionalJump(CodeEditor::Cursor target) {
    Label l = target.bc().immediate.offset;
    if (a_.shouldJump(l)) {
        a_.q_.push_front(target);
    }
}

template<typename ASTATE>
void ForwardAnalysis<ASTATE>::ControlFlowReceiver::terminator(CodeEditor::Cursor at) {
    if (a_.finalState_ == nullptr) {
        a_.finalState_ = a_.currentState_;
    } else {
        a_.finalState_->mergeWith(a_.currentState_);
        delete a_.currentState_;
    }
    a_.stopCurrentSequence_ = true;
    a_.currentState_ = nullptr;
}

template<typename ASTATE>
void ForwardAnalysis<ASTATE>::ControlFlowReceiver::label(CodeEditor::Cursor at) {
    // do nothing and be happy
}





template<typename ASTATE>
class ForwardAnalysisFinal : public ForwardAnalysis<ASTATE> {
    using ForwardAnalysis<ASTATE>::finalState_;
public:
    ASTATE const & finalState() {
        return * reinterpret_cast<ASTATE *>(finalState_);
    }
};

template<typename ASTATE>
class ForwardAnalysisIns : public ForwardAnalysisFinal<ASTATE> {
    using ForwardAnalysis<ASTATE>::currentIns_;
    using ForwardAnalysis<ASTATE>::currentState_;
    using ForwardAnalysis<ASTATE>::initialState_;
    using ForwardAnalysis<ASTATE>::dispatcher;
    using ForwardAnalysis<ASTATE>::mergePoints_;
protected:
    using ForwardAnalysis<ASTATE>::current;
    using ForwardAnalysis<ASTATE>::code_;
public:

    ASTATE const & operator [] (CodeEditor::Cursor const & ins) {
        assert(& ins.editorX() == code_ and "you can only use cursors from the same editor");
        if (ins != currentIns_)
            seek(ins);
        return current();
    }

protected:

    void doAnalyze() override {
        ForwardAnalysis<ASTATE>::doAnalyze();
        initializeCache();
    }

    void initializeCache() {
        currentState_ = initialState_->clone();
        currentIns_ = code_->getCursor();
    }

    void advance() {
        dispatcher().dispatch(currentIns_);
        currentIns_.advance();
        // if the cached instruction is label, dispose of the state and create a copy of the fixpoint
        if (currentIns_.bc().bc == BC_t::label) {
            delete currentState_;
            currentState_ = mergePoints_[currentIns_.bc().immediate.offset]->clone();
        }
    }

    void seek(CodeEditor::Cursor ins) {
        CodeEditor::Cursor end = ins.editorX().getCursorAtEnd();
        while (currentIns_ != end) {
            if (currentIns_ == ins)
                return;
            // advance the state using dispatcher
            advance();
        }
        // if we haven't found it, let's start over
        initializeCache();
        while (currentIns_!= end) {
            if (currentIns_ == ins)
                return;
            advance();
        }
        assert(false and "not reachable");
    }



};



}
