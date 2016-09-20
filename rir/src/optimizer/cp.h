#ifndef RIR_OPTIMIZER_CP_H
#define RIR_OPTIMIZER_CP_H

#include "code/analysis.h"
#include "code/InstructionVisitor.h"
#include "interpreter/interp_context.h"

namespace rir {

/** Constant Propagation Abstract Value.
 */
class CP_Value {
  public:
    static CP_Value const & top() {
        static CP_Value value(top_);
        return value;
    }

    static CP_Value const & bottom() {
        static CP_Value value(bottom_);
        return value;
    }

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

class ConstantPropagation : public ForwardAnalysisIns<AbstractState<CP_Value>>,
                            public InstructionVisitor::Receiver {
  public:
    typedef CP_Value Value;
    ConstantPropagation() :
        dispatcher_(*this) {
    }

  protected:

    /** Set incomming arguments to top.
     */
    AbstractState<CP_Value> * initialState() override {
        auto * result = new AbstractState<CP_Value>();
        for (SEXP x : code_->arguments()) {
            (*result)[x] = CP_Value::top();
        }
        return result;
    }

    virtual Dispatcher & dispatcher() override {
        return dispatcher_;
    }

    void push_(CodeEditor::Cursor ins) override {
        current().push(ins.bc().immediateConst());
    }

    void ldvar_(CodeEditor::Cursor ins) override {
        current().push(current().env().find(ins.bc().immediateConst()));
    }

    void stvar_(CodeEditor::Cursor ins) override {
        current()[ins.bc().immediateConst()] = current().pop();
    }

    void label(CodeEditor::Cursor ins) override {
    }

    /** All other instructions, don't care for now.
     */
    void any(CodeEditor::Cursor ins) override {
        // pop as many as we need, push as many tops as we need
        current().pop(ins.bc().popCount());
        for (size_t i = 0, e = ins.bc().pushCount(); i != e; ++i)
            current().push(Value::top());
    }

    InstructionVisitor dispatcher_;

};
}
#endif
