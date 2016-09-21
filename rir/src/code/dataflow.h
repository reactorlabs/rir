#ifndef RIR_OPTIMIZER_DATAFLOW_H
#define RIR_OPTIMIZER_DATAFLOW_H

#include "ir/CodeEditor.h"
#include "code/analysis.h"
#include "code/dispatchers.h"
#include "interpreter/interp_context.h"

#include <unordered_set>

namespace rir {

class StackV {
  public:
    StackV(){};
    StackV(CodeEditor::Iterator c) { defs.insert(c); };

    bool operator==(StackV const& other) const {
        return defs == other.defs && uses == other.uses;
    }

    bool operator!=(StackV const& other) const {
        return defs != other.defs && uses != other.uses;
    }

    bool mergeWith(StackV const& other) {
        auto ds = defs.size();
        auto us = uses.size();

        defs.insert(other.defs.begin(), other.defs.end());
        uses.insert(other.uses.begin(), other.uses.end());

        return defs.size() != ds || uses.size() != us;
    }

    void used(CodeEditor::Iterator c) { uses.insert(c); }

    std::unordered_set<CodeEditor::Iterator> defs;
    std::unordered_set<CodeEditor::Iterator> uses;
};

class DataflowAnalysis : public ForwardAnalysisIns<AbstractStack<StackV>>,
                         public InstructionDispatcher::Receiver {
  public:
    DataflowAnalysis() : dispatcher_(*this) {}

  protected:
    virtual Dispatcher& dispatcher() override { return dispatcher_; }

    void dup_(CodeEditor::Iterator ins) override {
        auto v = current().pop();
        v.used(ins);
        current().push(StackV(ins));
        current().push(v);
    }

    void label(CodeEditor::Iterator ins) override {}

    /** All other instructions, don't care for now.
     */
    void any(CodeEditor::Iterator ins) override {
        BC bc = *ins;
        // pop as many as we need, push as many tops as we need
        current().pop(bc.popCount());
        for (size_t i = 0, e = bc.pushCount(); i != e; ++i)
            current().push(StackV(ins));
    }

    void return_(CodeEditor::Iterator ins) override {
        // Return is also a leave instruction
        current().pop(current().depth());
    }

    InstructionDispatcher dispatcher_;
};
}
#endif
