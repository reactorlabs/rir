#ifndef RIR_OPTIMIZER_CP_H
#define RIR_OPTIMIZER_CP_H

#include "analysis_framework/analysis.h"
#include "analysis_framework/dispatchers.h"
#include "interpreter/interp_context.h"

namespace rir {

/** Constant Propagation Abstract Value.
 */
class CP_Value {
  public:
    static CP_Value const& top() {
        static CP_Value value(top_);
        return value;
    }

    static CP_Value const& bottom() {
        static CP_Value value(bottom_);
        return value;
    }

    static CP_Value const& Absent() { return top(); }

    CP_Value(SEXP from) : value_(from) {}

    CP_Value(CP_Value const& other) = default;

    CP_Value& operator=(SEXP what) {
        value_ = what;
        return *this;
    }

    CP_Value& operator=(CP_Value const& other) = default;

    bool operator==(CP_Value const& other) const {
        return value_ == other.value_;
    }

    bool operator!=(CP_Value const& other) const {
        return value_ != other.value_;
    }

    SEXP value() const {
        assert(value_ != bottom_ and value_ != top_);
        return value_;
    }

    bool isConst() const { return value_ != bottom_ and value_ != top_; }

    bool mergeWith(CP_Value const& other) {
        if (value_ == top_)
            return false;
        if (*this == other)
            return false;
        if (value_ == bottom_)
            value_ = other.value_;
        else
            value_ = top_;
        return true;
    }

    void print() const {
        if (value_ == top_)
            Rprintf("T");
        else if (value_ == bottom_)
            Rprintf("B");
        else
            Rf_PrintValue(value_);
    }

  protected:
    static SEXP const bottom_;
    static SEXP const top_;

    SEXP value_;
};

class ConstantPropagation
    : public ForwardAnalysisIns<AbstractState<SEXP, CP_Value>>,
      public InstructionDispatcher::Receiver {
  public:
    typedef CP_Value Value;
    ConstantPropagation() : dispatcher_(*this) {}

  protected:
    /** Set incomming arguments to top.
     */
    AbstractState<SEXP, CP_Value>* initialState() override {
        auto* result = new AbstractState<SEXP, CP_Value>();
        for (auto x : code_->arguments()) {
            (*result)[x.first] = CP_Value::top();
        }
        return result;
    }

    virtual Dispatcher& dispatcher() override { return dispatcher_; }

    void push_(CodeEditor::Iterator ins) override {
        BC bc = *ins;
        current().push(bc.immediateConst());
    }

    void ldvar_(CodeEditor::Iterator ins) override {
        BC bc = *ins;
        current().push(current().env().find(bc.immediateConst()));
    }

    void stvar_(CodeEditor::Iterator ins) override {
        BC bc = *ins;
        current()[bc.immediateConst()] = current().pop();
    }

    void label(CodeEditor::Iterator ins) override {}

    /** All other instructions, don't care for now.
     */
    void any(CodeEditor::Iterator ins) override {
        BC bc = *ins;
        // pop as many as we need, push as many tops as we need
        current().pop(bc.popCount());
        for (size_t i = 0, e = bc.pushCount(); i != e; ++i)
            current().push(Value::top());
    }

    InstructionDispatcher dispatcher_;
};
}
#endif
