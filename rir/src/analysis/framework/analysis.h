#pragma once

#include "State.h"
#include "framework.h"

#include "R/Funtab.h"
namespace rir {

/** Base class for all analyses.

  In the future, this is where the API for analysis scheduling, retrieval and
  invalidation will live.
 */
class Analysis {
  public:
    /** Analyzes the given code.

      Internally sets the state and then calls the doAnalyze() virtual method
      that should implement the actual analysis.
     */
    void analyze(CodeEditor& code) {
        if (code_ != nullptr)
            invalidate();
        code_ = &code;
        doAnalyze();
    }

    /** Invalidates the data computed by the analysis.

      This is to be overwritten in child classes to proviode the appropriate
      cleanup functionality.
     */
    virtual void invalidate() { code_ = nullptr; }

    /** Returns true if the analysis contains valid data.
     */
    bool good() const { return code_ != nullptr; }

    /** Override this for pretty printing.
     */
    virtual void print() = 0;

  protected:
    /** Override this method to provide the actual analysis.

      This should mostly mean invoking the appropriate driver and collecting the
      state.
     */

    virtual void doAnalyze() = 0;

    CodeEditor* code_ = nullptr;
};

/** Forward driver analysis updates the basic analysis API with forward driver.

  ForwardAnalysis does not deal with visibility of any results, that is the
  domain of its descelndants.
 */
template <typename ASTATE>
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
        for (State* s : mergePoints_)
            delete s;
        mergePoints_.clear();
    }

    void print() override {}

  protected:
    ForwardAnalysis() {}

    ASTATE& current() { return *reinterpret_cast<ASTATE*>(currentState_); }

    virtual Dispatcher& dispatcher() = 0;

    virtual ASTATE* initialState() { return new ASTATE(); }

    void doAnalyze() override {
        mergePoints_.resize(code_->numLabels());
        initialState_ = initialState();
        currentState_ = initialState_->clone();
        q_.push_front(code_->begin());
        Dispatcher& d = dispatcher();
        while (not q_.empty()) {
            currentIns_ = q_.front();
            q_.pop_front();
            while (true) {
                BC cur = *currentIns_;

                // if current instruction is label, deal with state merging
                if (cur.is(Opcode::label)) {
                    // if state not stored, store copy of incomming
                    State*& stored = mergePoints_[cur.immediate.offset];
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
                            // and terminate current branch if there is no need
                            // to continue
                        } else {
                            delete currentState_;
                            currentState_ = nullptr;
                            break;
                        }
                    }
                }
                // user dispatch method
                d.dispatch(currentIns_);

                if (cur.is(Opcode::br_)) {
                    LabelT l = cur.immediate.offset;
                    if (shouldJump(l)) {
                        q_.push_front(code_->target(cur));
                    }
                    delete currentState_;
                    currentState_ = nullptr;
                    break;
                } else if (cur.isJmp()) {
                    LabelT l = cur.immediate.offset;
                    if (shouldJump(l)) {
                        q_.push_front(code_->target(cur));
                    }
                } else if (cur.isReturn()) {
                    if (finalState_ == nullptr) {
                        finalState_ = currentState_;
                    } else {
                        finalState_->mergeWith(currentState_);
                        delete currentState_;
                    }
                    currentState_ = nullptr;
                    break;
                }

                // move to next instruction
                ++currentIns_;
            }
        }
    }

    State* initialState_ = nullptr;
    State* currentState_ = nullptr;
    State* finalState_ = nullptr;
    CodeEditor::Iterator currentIns_;
    std::vector<State*> mergePoints_;

  private:
    bool shouldJump(size_t label) {
        State*& stored = mergePoints_[label];
        if (stored == nullptr) {
            stored = currentState_->clone();
            return true;
        } else {
            return stored->mergeWith(currentState_);
        }
    }

    std::deque<CodeEditor::Iterator> q_;
};

template <typename ASTATE>
class ForwardAnalysisFinal : public ForwardAnalysis<ASTATE> {
    using ForwardAnalysis<ASTATE>::finalState_;

  public:
    ASTATE const& finalState() {
        return *reinterpret_cast<ASTATE*>(finalState_);
    }
};

template <typename ASTATE>
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
    ASTATE const& operator[](CodeEditor::Iterator ins) {
        if (ins != currentIns_)
            seek(ins);
        return current();
    }
    ASTATE const& operator[](CodeEditor::Cursor cur) {
        auto ins = cur.asItr();
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
        currentIns_ = code_->begin();
    }

    void advance() {
        dispatcher().dispatch(currentIns_);
        ++currentIns_;
        // if the cached instruction is label, dispose of the state and create a
        // copy of the fixpoint
        if ((*currentIns_).is(Opcode::label)) {
            auto fixpoint = mergePoints_[(*currentIns_).immediate.offset];
            // if we reach dead code there is no merge state available
            if (fixpoint) {
                delete currentState_;
                currentState_ = fixpoint->clone();
            }
        }
    }

    void seek(CodeEditor::Iterator ins) {
        while (currentIns_ != code_->end()) {
            if (currentIns_ == ins)
                return;
            if ((*currentIns_).isReturn())
                break;
            // advance the state using dispatcher
            advance();
        }
        // if we haven't found it, let's start over
        initializeCache();
        while (currentIns_ != code_->end()) {
            if (currentIns_ == ins)
                return;
            advance();
        }
        assert(false and "not reachable");
    }
};

static inline bool isSafeBuiltin(int i) {
    // We have reason to believe that those would not run arbitrary
    // code and not mess with the env

    // builtins for `is.*` where primval(op) not within [100,200[
    // (those do not dispatch)
    if ((i >= 362 && i < 376) || (i >= 379 && i <= 389))
        return true;

    switch (i) {
    case 62:  // identical
    case 88:  // c
    case 91:  // class
    case 107: // vector
    case 397: // rep.int
    case 555: // inherits
        return true;
    }
    return false;
}
}
