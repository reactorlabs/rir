#ifndef RIR_OPTIMIZER_CLEANUP_H
#define RIR_OPTIMIZER_CLEANUP_H

#include "code/dataflow.h"

namespace rir {

class BCCleanup : public InstructionDispatcher::Receiver {
  public:
    DataflowAnalysis analysis;
    InstructionDispatcher dispatcher;
    CodeEditor& code_;

    BCCleanup(CodeEditor& code) : dispatcher(*this), code_(code) {}

    void pop_(CodeEditor::Iterator ins) override {
        auto v = analysis[ins].top();
        if (v.defs.size() != 1)
            return;

        auto defI = *v.defs.begin();
        BC def = *defI;

        // push - pop elimination
        if (v.uses.empty()) {
            if (def.is(BC_t::push_)) {
                defI.asCursor(code_).remove();
                ins.asCursor(code_).remove();
                return;
            }
        }

        // double load elimination : ldvar a; pop; ldvar a;
        auto next = ins + 1;
        if ((*next).is(BC_t::ldvar_) && def == *next) {
            CodeEditor::Cursor cur = ins.asCursor(code_);
            cur.remove();
            cur.remove();
            return;
        }
    }

    void ldvar_(CodeEditor::Iterator ins) override {
        // double load elimination : ldvar a; ldvar a;
        auto prev = ins - 1;
        if ((*prev).is(BC_t::ldvar_) && *ins == *prev) {
            CodeEditor::Cursor cur = ins.asCursor(code_);
            cur.remove();
            cur << BC::dup();
            return;
        }
    }

    void run() {
        analysis.analyze(code_);
        for (auto i = code_.begin(); i != code_.end(); ++i) {
            dispatcher.dispatch(i);
        }
    }
};
}
#endif
