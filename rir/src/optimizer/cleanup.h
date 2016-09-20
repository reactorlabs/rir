#ifndef RIR_OPTIMIZER_CLEANUP_H
#define RIR_OPTIMIZER_CLEANUP_H

#include "code/dataflow.h"

namespace rir {

class BCCleanup : public InstructionVisitor::Receiver {
  public:
    DataflowAnalysis analysis;
    InstructionVisitor dispatcher;

    BCCleanup() : dispatcher(*this) {}

    void pop_(CodeEditor::Cursor& ins) override {
        auto v = analysis[ins].top();
        if (v.uses.empty() && v.defs.size() == 1) {
            for (auto def : v.defs) {
                if (def.bc().bc == BC_t::push_) {
                    def.erase();
                    ins.erase();
                }
            }
        }
    }

    void run(CodeEditor& code) {
        analysis.analyze(code);
        for (auto i = code.getCursor(); !i.atEnd(); i.advance()) {
            dispatcher.dispatch(i);
        }
    }
};
}
#endif
