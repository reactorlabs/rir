#ifndef RIR_OPTIMIZER_DATAFLOW_H
#define RIR_OPTIMIZER_DATAFLOW_H

#include "ir/CodeEditor.h"
#include "code/analysis.h"
#include "code/InstructionVisitor.h"
#include "interpreter/interp_context.h"

#include <unordered_set>

namespace rir {

class StackV {
  public:
    StackV(){};
    StackV(CodeEditor::Cursor c) { defs.insert(c); };

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

    void used(CodeEditor::Cursor c) { uses.insert(c); }

    std::unordered_set<CodeEditor::Cursor> defs;
    std::unordered_set<CodeEditor::Cursor> uses;
};

class DataflowAnalysis : public ForwardAnalysisIns<AbstractStack<StackV>>,
                         public InstructionVisitor::Receiver {
  public:
    DataflowAnalysis() : dispatcher_(*this) {}

  protected:
    virtual Dispatcher& dispatcher() override { return dispatcher_; }

    // void dup_(CodeEditor::Cursor ins) override {
    //     auto v = current().pop();
    //     v.used(ins);
    //     current().push(v);
    //     current().push(StackV(ins));
    // }

    void label(CodeEditor::Cursor ins) override {}

    /** All other instructions, don't care for now.
     */
    void any(CodeEditor::Cursor ins) override {
        // pop as many as we need, push as many tops as we need
        current().pop(ins.bc().popCount());
        for (size_t i = 0, e = ins.bc().pushCount(); i != e; ++i)
            current().push(StackV(ins));
    }

    InstructionVisitor dispatcher_;
};
}
#endif
