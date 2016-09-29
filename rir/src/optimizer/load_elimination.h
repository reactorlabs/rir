#ifndef RIR_OPTIMIZER_LOAD_ELIMINATION_H
#define RIR_OPTIMIZER_LOAD_ELIMINATION_H

#include "code/scope.h"

namespace rir {

class LoadElimination : public InstructionDispatcher::Receiver {
  public:
    ScopeResolution analysis;
    InstructionDispatcher dispatcher;
    CodeEditor& code_;

    LoadElimination(CodeEditor& code) : dispatcher(*this), code_(code) {}

    void isspecial_(CodeEditor::Iterator ins) override {
        SEXP sym = Pool::get((*ins).immediate.pool);
        if (analysis[ins][sym].t == PointsTo::Type::DefaultValue &&
            analysis[ins][sym].c == sym) {
            ins.asCursor(code_).remove();
        }
    }

    void ldvar_(CodeEditor::Iterator ins) override {
        SEXP sym = Pool::get((*ins).immediate.pool);
        auto v = analysis[ins][sym];
        if (v.t == PointsTo::Type::Argument) {
            auto cur = ins.asCursor(code_);
            cur.remove();
            cur << BC::ldarg(sym);
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
