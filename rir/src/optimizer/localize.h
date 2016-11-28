#ifndef RIR_LOCALIZE_H
#define RIR_LOCALIZE_H

#include "code/dataflow.h"

namespace rir {

class Localizer : public InstructionDispatcher::Receiver {
  public:
    DataflowAnalysis<Type::Conservative> analysis;
    InstructionDispatcher dispatcher;
    CodeEditor& code_;
    CodeEditor::Iterator lastCall;
    bool initSteam;
    bool steam;

    Localizer(CodeEditor& code, bool envIsStable)
        : dispatcher(*this), code_(code), initSteam(envIsStable) {}

    void call_(CodeEditor::Iterator ins) override { lastCall = ins; }

    void dispatch_(CodeEditor::Iterator ins) override { lastCall = ins; }

    void call_stack_(CodeEditor::Iterator ins) override { lastCall = ins; }

    void dispatch_stack_(CodeEditor::Iterator ins) override { lastCall = ins; }

    void static_call_stack_(CodeEditor::Iterator ins) override {
        lastCall = ins;
    }

    void ldvar_(CodeEditor::Iterator ins) override {
        SEXP sym = (*ins).immediateConst();
        auto v = analysis[ins][sym];

        if (v.t == FValue::Type::Argument) {
            auto cur = ins.asCursor(code_);
            cur.remove();
            cur << BC::ldarg(sym);
            return;
        }
        if (v.t == FValue::Type::Constant) {
            SEXP constant = analysis.constant(v);
            if (TYPEOF(constant) == CLOSXP && TYPEOF(BODY(constant)) != INTSXP)
                return;
            auto cur = ins.asCursor(code_);
            cur.remove();
            cur << BC::push(constant);
            return;
        }
        if (v.isValue()) {
            auto cur = ins.asCursor(code_);
            cur.remove();
            cur << BC::ldlval(sym);
            return;
        }

        if (steam && lastCall != code_.end()) {
            (lastCall + 1).asCursor(code_) << BC::guardEnv();
            steam = false;
        }
    }

    void run() {
        steam = initSteam;
        analysis.analyze(code_);
        lastCall = code_.end();
        for (auto i = code_.begin(); i != code_.end(); ++i) {
            dispatcher.dispatch(i);
        }
    }
};
}
#endif
