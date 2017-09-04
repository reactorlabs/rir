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
                    // if state not stored, store copy of incoming
                    State*& stored = mergePoints_[cur.immediate.offset];
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


/** Backward analysis driver
 */
template <typename ASTATE>
class BackwardAnalysis : public Analysis {
  public:
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
        computeBackwardCF();

        initialState_ = initialState();
        Dispatcher& d = dispatcher();

        while (not q_.empty()) {
            currentIns_ = q_.front();
            q_.pop_front();

            while (true) {
                BC cur = *currentIns_;

                // start of new path
                if (cur.isReturn()) {
                    assert(currentState_ == nullptr);
                    currentState_ = initialState_->clone();
                }

                // merging
                else if (isBasicBlockEnd(currentIns_)) {

                    // if state not stored, store copy of incoming
                    State * & stored = mergePoints_[currentIns_];  // first call to [] initializes to nullptr
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

                // endpoint is begining
                if (currentIns_ == code_->begin()) {
                    if (finalState_ == nullptr) {
                        finalState_ = currentState_;
                    } else {
                        finalState_->mergeWith(currentState_);
                        delete currentState_;
                    }
                    currentState_ = nullptr;
                    break;
                }

                // add jumps, terminate finished branches
                if (isBasicBlockStart(currentIns_)) {

                    // possibly multiple paths lead to this instruction
                    if (cur.is(Opcode::label)) {

                        // merge all origin points to this label and continue to prev
                        for (auto& origin : jumpOrigins_[cur.immediate.offset]) {
                            if (shouldFollowJumpFrom(origin)) {
                                q_.push_front(origin);
                            }
                        }

                        // if prev is unconditional jump elswhere or return, terminate branch
                        BC prev = *(currentIns_ - 1);
                        if ((prev.is(Opcode::br_) && code_->target(prev) != currentIns_) ||
                                prev.isReturn()) {
                            // all origins already in q_
                            delete currentState_;
                            currentState_ = nullptr;
                            break;
                        }
                    }

                    // else: had to come from previous instruction
                    // (unless prev is unconditional jump, then this would be dead code!!)
                    // -> merge postponed to previous instruction

                }

                // FIXME: sequences JUMP + LABEL cause merge twice... !

                // move to the previous instruction
                --currentIns_;
            }
        }
    }

    State* initialState_ = nullptr;  // multiple initial states for all returns?? (but they are all the same)
    State* currentState_ = nullptr;
    State* finalState_ = nullptr;
    CodeEditor::Iterator currentIns_;

    std::unordered_map<CodeEditor::Iterator, State*> mergePoints_;


private:
    bool shouldFollowJumpFrom(CodeEditor::Iterator ins) {
        State*& stored = mergePoints_[ins];  // first call to [] initializes to nullptr
        if (stored == nullptr) {
            stored = currentState_->clone();
            return true;
        } else {
            return stored->mergeWith(currentState_);
        }
    }

    bool isBasicBlockStart(CodeEditor::Iterator ins) const {
        // Used to decide if we should merge states

        BC cur = *ins;

        // labels are starts
        if (cur.is(Opcode::label)) return true;

        // first instruction handled separately
        if (ins == code_->begin()) return false;

        // jump plus whatever means whatever is start
        BC prev = *(ins - 1);
        if (prev.isJmp()) return true;

        return false;
    }

    bool isBasicBlockEnd(CodeEditor::Iterator ins) const {
        // Used to decide if we should merge states

        BC cur = *ins;

        // jumps are ends
        if (cur.isJmp()) return true;

        // last instruction and returns handled separately, i.e. no merging for them
        if (ins == code_->end() || cur.isReturn()) return false;

        // whatever plus label means whatever is end
        BC next = *(ins + 1);
        if (next.is(Opcode::label)) return true;

        return false;
    }

    void computeBackwardCF() {
        // find for all labels the jumps that target them
        // also add all exit points to the working list

        jumpOrigins_.resize(code_->numLabels());

        int mpCount = 0;

        bool dead = false;
        auto it = code_->begin(), end = code_->end();
        while (it != end) {
            BC ins = *it;

            if (ins.is(Opcode::label)) dead = false;

            if (!dead) {
                // remember jump targets
                if (ins.isJmp()) {
                    JmpT l = ins.immediate.offset;
                    jumpOrigins_[l].push_back(it);
                }

                // count merge points
                if (isBasicBlockEnd(it)) {
                    ++mpCount;
                }

                // add entry point to working list
                if (ins.isReturn()) {
                    q_.push_front(it);
                    dead = true;
                }
            }

            ++it;
        }

        mergePoints_.reserve(mpCount);
    }

    std::vector<std::vector<CodeEditor::Iterator>> jumpOrigins_;
    std::deque<CodeEditor::Iterator> q_;
};

template<typename ASTATE>
class BackwardAnalysisFinal : public BackwardAnalysis<ASTATE> {
    using BackwardAnalysis<ASTATE>::finalState_;
public:
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
//        currentState_ = initialState_->clone();
//        currentIns_ = code_->begin();
      //  assert(false && "Not yet implemented.");
    }

    void advance() {
//        dispatcher().dispatch(currentIns_);
//        ++currentIns_;
//        // if the cached instruction is label, dispose of the state and create a copy of the fixpoint
//        if ((*currentIns_).is(BC_t::label)) {
//            auto fixpoint = mergePoints_[(*currentIns_).immediate.offset];
//            // if we reach dead code there is no merge state available
//            if (fixpoint) {
//                delete currentState_;
//                currentState_ = fixpoint->clone();
//            }
//        }
        assert(false && "Not yet implemented.");
    }

    void seek(CodeEditor::Iterator ins) {
//        while (currentIns_ != code_->end()) {
//            if (currentIns_ == ins)
//                return;
//            if ((*currentIns_).isReturn())
//                break;
//            // advance the state using dispatcher
//            advance();
//        }
//        // if we haven't found it, let's start over
//        initializeCache();
//        while (currentIns_ != code_->end()) {
//            if (currentIns_ == ins)
//                return;
//            advance();
//        }
//        assert(false and "not reachable");
        assert(false && "Not yet implemented.");
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
