#pragma once

#include "ForwardDriver.h"
#include "State.h"

namespace rir {

/** Base class for all analyses.
 */
class Analysis {
public:
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

    virtual void print() {

    }

protected:

    virtual void doAnalyze() = 0;

    CodeEditor * code_ = nullptr;


};

template<typename ASTATE>
class ForwardAnalysis : public Analysis, public ForwardDriver {
public:

    void invalidate() override {
        Analysis::invalidate();
        clear();
    }

    ASTATE const & finalState() {
        return * finalState_;
    }

    ASTATE const & operator [] (Cursor const & ins) {
        assert(& ins.editor() == code_ and "you can only use cursors from the same editor");
        if (ins != currentIns_)
            seek(ins);
        return current();
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

    void doAnalyze() {
        run(* code_, initialState(), dispatcher());
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
