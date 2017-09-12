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
    virtual ~Analysis() {}

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
    ~ForwardAnalysis() { invalidate(); }

    void invalidate() override {
        Analysis::invalidate();
        delete currentState_;
        delete initialState_;
        delete finalState_;
        initialState_ = nullptr;
        currentState_ = nullptr;
        finalState_ = nullptr;
        for (auto& s : mergePoints_)
            delete s.second;
        mergePoints_.clear();
    }

    void print() override {}

  protected:
    ForwardAnalysis() {}

    ASTATE& current() { return *reinterpret_cast<ASTATE*>(currentState_); }

    virtual Dispatcher& dispatcher() = 0;

    virtual ASTATE* initialState() { return new ASTATE(); }

    void doAnalyze() override {
        initialState_ = initialState();
        currentState_ = initialState_->clone();
        q_.push_front(code_->begin());
        Dispatcher& d = dispatcher();
        while (not q_.empty()) {
            currentIns_ = q_.front();
            q_.pop_front();
            while (true) {
                // if current instruction is label, deal with state merging
                if (code_->isLabel(currentIns_)) {
                    // if state not stored, store copy of incoming
                    State*& stored = mergePoints_[currentIns_];
                    if (stored == nullptr) {
                        assert(currentState_ != nullptr);
                        stored = currentState_->clone();
                    } else {
                        // if incoming not present, copy stored
                        if (currentState_ == nullptr) {
                            currentState_ = stored->clone();
                            // otherwise merge incoming with stored
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

                if (code_->isJmp(currentIns_)) {
                    auto l = code_->target(currentIns_);
                    if (shouldJump(l)) {
                        q_.push_front(l);
                    }
                    if (code_->isUncondJmp(currentIns_)) {
                        delete currentState_;
                        currentState_ = nullptr;
                        break;
                    }
                } else if (code_->isExitPoint(currentIns_)) {
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
    std::unordered_map<CodeEditor::Iterator, State*> mergePoints_;

  private:
    bool shouldJump(CodeEditor::Iterator label) {
        State*& stored = mergePoints_[label];
        if (stored == nullptr) {
            stored = currentState_->clone();
            return true;
        }
        return stored->mergeWith(currentState_);
    }

    std::deque<CodeEditor::Iterator> q_;
};

template <typename ASTATE>
class ForwardAnalysisFinal : public ForwardAnalysis<ASTATE> {
    using ForwardAnalysis<ASTATE>::finalState_;

  public:
    ~ForwardAnalysisFinal() = default;

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
    ~ForwardAnalysisIns() = default;

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
        delete currentState_;
        currentState_ = initialState_->clone();
        currentIns_ = code_->begin();
    }

    void advance() {
        dispatcher().dispatch(currentIns_);
        ++currentIns_;
        // if the cached instruction is label, dispose of the state and create a
        // copy of the fixpoint
        if (code_->isLabel(currentIns_)) {
            auto fixpoint = mergePoints_[currentIns_];
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
            if (code_->isExitPoint(currentIns_))
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


/** Backward analysis driver
 */
template <typename ASTATE>
class BackwardAnalysis : public Analysis {
  public:
    ~BackwardAnalysis() { invalidate(); }

    void invalidate() override {
        Analysis::invalidate();
        delete currentState_;
        delete initialState_;
        delete finalState_;
        currentState_ = nullptr;
        initialState_ = nullptr;
        finalState_ = nullptr;
        for (auto& s : mergePoints_)
            delete s.second;
        mergePoints_.clear();
        jumpOrigins_.clear();
    }

    void print() override {}

  protected:
    BackwardAnalysis() {}

    ASTATE& current() {
        return *reinterpret_cast<ASTATE*>(currentState_);
    }

    virtual Dispatcher& dispatcher() = 0;

    virtual ASTATE* initialState() {
        return new ASTATE();
    }

    void doAnalyze() override {

        // First, forward pass to find jump origins for labels
        // and to add exit points to the working list
        for (auto it = code_->begin(); it != code_->end(); ++it) {
            if (code_->isJmp(it)) {
                jumpOrigins_[code_->target(it)].push_back(it);
            }
            if (code_->isExitPoint(it)) {
                q_.push_front(it);
            }
        }

        initialState_ = initialState();
        Dispatcher& d = dispatcher();

        while (not q_.empty()) {
            currentIns_ = q_.front();
            q_.pop_front();

            while (true) {
                if (code_->isExitPoint(currentIns_)) {
                    // need initial state
                    assert(currentState_ == nullptr);
                    currentState_ = initialState_->clone();
                } else if (isMergePoint(currentIns_)) {
                    // if state not stored, store copy of incoming
                    State*& stored = mergePoints_[currentIns_];  // first call to [] initializes to nullptr
                    if (!stored) {
                        assert(currentState_ != nullptr);
                        stored = currentState_->clone();
                    } else {
                        // if incoming not present, copy stored
                        if (currentState_ == nullptr) {
                            currentState_ = stored->clone();
                        // otherwise merge incoming with stored
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

                if (code_->isEntryPoint(currentIns_)) {
                    // backward analysis ends here
                    if (finalState_ == nullptr) {
                        finalState_ = currentState_;
                    } else {
                        finalState_->mergeWith(currentState_);
                        delete currentState_;
                    }
                    currentState_ = nullptr;
                    break;
                }

                if (code_->isLabel(currentIns_)) {
                    // merge all origins for this label
                    for (auto origin : jumpOrigins_[currentIns_]) {
                        if (shouldFollowJumpFrom(origin)) {
                            q_.push_front(origin);
                        }
                    }
                    // if previous instruction doesn't lead here
                    if (code_->isExitPoint(currentIns_ - 1) ||
                            code_->next(currentIns_ - 1).count(currentIns_) == 0) {
                        delete currentState_;
                        currentState_ = nullptr;
                        break;
                    }
                }

                // move to the previous instruction
                --currentIns_;
            }
        }
    }

    bool isMergePoint(CodeEditor::Iterator ins) const {
        return code_->isJmp(ins);
    }

    State* initialState_ = nullptr;
    State* currentState_ = nullptr;
    State* finalState_ = nullptr;
    CodeEditor::Iterator currentIns_;

    std::unordered_map<CodeEditor::Iterator, State*> mergePoints_;

  private:
    bool shouldFollowJumpFrom(CodeEditor::Iterator ins) {
        State*& stored = mergePoints_[ins];
        if (!stored) {
            stored = currentState_->clone();
            return true;
        }
        return stored->mergeWith(currentState_);
    }

    std::unordered_map<CodeEditor::Iterator, std::vector<CodeEditor::Iterator>> jumpOrigins_;
    std::deque<CodeEditor::Iterator> q_;
};

template<typename ASTATE>
class BackwardAnalysisFinal : public BackwardAnalysis<ASTATE> {
    using BackwardAnalysis<ASTATE>::finalState_;
public:
    ~BackwardAnalysisFinal() = default;

    ASTATE const& finalState() {
        return *reinterpret_cast<ASTATE*>(finalState_);
    }
};

template<typename ASTATE>
class BackwardAnalysisIns : public BackwardAnalysisFinal<ASTATE> {
    using BackwardAnalysis<ASTATE>::currentIns_;
    using BackwardAnalysis<ASTATE>::currentState_;
    using BackwardAnalysis<ASTATE>::initialState_;
    using BackwardAnalysis<ASTATE>::dispatcher;
    using BackwardAnalysis<ASTATE>::mergePoints_;

protected:
    using BackwardAnalysis<ASTATE>::current;
    using BackwardAnalysis<ASTATE>::code_;

public:
    ~BackwardAnalysisIns() = default;

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
        BackwardAnalysis<ASTATE>::doAnalyze();
        initializeCache();
    }

    void initializeCache() {
        delete currentState_;  // possibly non-null?
        currentState_ = initialState_->clone();
        currentIns_ = code_->rbegin();
        dispatcher().dispatch(currentIns_);
    }

    void advance() {
        --currentIns_;
        // if entry point for the analysis, initial state is needed
        if (code_->isExitPoint(currentIns_)) {
            delete currentState_;
            currentState_ = initialState_->clone();
        // here we have stored fixpoint, so use it
        } else if (this->isMergePoint(currentIns_)) {
            auto fixpoint = mergePoints_[currentIns_];
            if (fixpoint) {
                delete currentState_;
                currentState_ = fixpoint->clone();
            }
        }
        dispatcher().dispatch(currentIns_);
    }

    void seek(CodeEditor::Iterator ins) {
        while (currentIns_ != code_->rend()) {
            if (currentIns_ == ins)
                return;
            // advance the state using dispatcher
            advance();
        }
        // if we haven't found it, let's start over
        initializeCache();
        while (currentIns_ != code_->rend()) {
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
