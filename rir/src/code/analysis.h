#pragma once

#include "ForwardDriver.h"
#include "State.h"

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
class ForwardAnalysis : public Analysis, public ForwardDriver {
public:

    void invalidate() override {
        Analysis::invalidate();
        clear();
    }

    void print() override {

    }

protected:
    ForwardAnalysis() = default;

    ASTATE & current() {
        return * reinterpret_cast<ASTATE *>(currentState_);
    }

    virtual Dispatcher & dispatcher() = 0;

    virtual ASTATE * initialState() {
        return new ASTATE();
    }

    void doAnalyze() override {
        run(* code_, initialState(), dispatcher());
    }


};


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
    using ForwardAnalysis<ASTATE>::code_;
    using ForwardAnalysis<ASTATE>::dispatcher;
    using ForwardAnalysis<ASTATE>::mergePoints_;
protected:
    using ForwardAnalysis<ASTATE>::current;
public:

    ASTATE const & operator [] (Cursor const & ins) {
        assert(& ins.editor() == code_ and "you can only use cursors from the same editor");
        if (ins != currentIns_)
            seek(ins);
        return current();
    }

protected:
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
