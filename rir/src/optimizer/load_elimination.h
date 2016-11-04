#ifndef RIR_OPTIMIZER_LOAD_ELIMINATION_H
#define RIR_OPTIMIZER_LOAD_ELIMINATION_H

#include "code/scope.h"
#include "code/dataflow.h"

namespace rir {

class LoadElimination : public InstructionDispatcher::Receiver {
  public:
    ScopeResolution<Type::Conservative> analysis;
    InstructionDispatcher dispatcher;
    CodeEditor& code_;

    LoadElimination(CodeEditor& code) : dispatcher(*this), code_(code) {}

    void ldfun_(CodeEditor::Iterator ins) override {
        SEXP sym = Pool::get((*ins).immediate.pool);
        auto v = analysis[ins][sym];
        if (v.t == PointsTo::Type::Constant) {
            SEXP constant = v.constant();
            if (TYPEOF(constant) == CLOSXP && TYPEOF(BODY(constant)) != INTSXP)
                return;
            auto cur = ins.asCursor(code_);
            cur.remove();
            cur << BC::push(constant);
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
        if (v.t == PointsTo::Type::Constant) {
            SEXP constant = v.constant();
            if (TYPEOF(constant) == CLOSXP && TYPEOF(BODY(constant)) != INTSXP)
                return;
            auto cur = ins.asCursor(code_);
            cur.remove();
            cur << BC::push(constant);
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
