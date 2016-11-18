#ifndef RIR_LOCALIZE_H
#define RIR_LOCALIZE_H

#include "code/dataflow.h"

namespace rir {

class Localizer : public InstructionDispatcher::Receiver {
  public:
    DataflowAnalysis<Type::Conservative> analysis;
    DataflowAnalysis<Type::NoDeleteNoPromiseStore> optimisticAnalysis;
    InstructionDispatcher dispatcher;
    CodeEditor& code_;

    Localizer(CodeEditor& code) : dispatcher(*this), code_(code) {}

    void ldfun_(CodeEditor::Iterator ins) override {}

    void ldvar_(CodeEditor::Iterator ins) override {
        SEXP sym = Pool::get((*ins).immediate.pool);
        auto cv = analysis[ins][sym];
        auto ov = optimisticAnalysis[ins][sym];
        if (!cv.isPresent() && ov.isValue()) {
            auto cur = ins.asCursor(code_);
            cur.remove();
            cur << BC::guardLocal(sym) << BC::ldlval(sym);
            return;
        }
        if (!cv.isPresent() && ov.t == FValue::Type::Argument) {
            auto cur = ins.asCursor(code_);
            cur.remove();
            cur << BC::guardArg(sym) << BC::ldarg(sym);
            return;
        }
    }

    void run() {
        analysis.analyze(code_);
        optimisticAnalysis.analyze(code_);
        for (auto i = code_.begin(); i != code_.end(); ++i) {
            dispatcher.dispatch(i);
        }
    }
};
}
#endif
