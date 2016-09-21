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
        if (v.uses.empty() && v.defs.size() == 1) {
            for (auto defI : v.defs) {
                BC def = *defI;
                if (def.is(BC_t::push_)) {
                    defI.asCursor(code_).erase();
                    ins.asCursor(code_).erase();
                }
            }
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
