#ifndef RIR_LOCALIZE_H
#define RIR_LOCALIZE_H

#include "analysis/dataflow.h"
#include "interpreter/deoptimizer.h"

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

    void asbool_(CodeEditor::Iterator ins) override { lastCall = ins; }

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

        // Try to convert generic ldvar instruction to a more specialized
        // version
        if (v.t == FValue::Type::Argument) {
            auto cur = ins.asCursor(code_);
            cur.remove();
            cur << BC::ldarg(sym);
            return;
        }
        if (v.t == FValue::Type::Constant) {
            SEXP constant = analysis.constant(v);
            if (TYPEOF(constant) == CLOSXP &&
                TYPEOF(BODY(constant)) != EXTERNALSXP)
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

        // Lets see if there was a call before this ldvar and if before that
        // call we knew that the variable is a local variable or argument.
        // If that's the case, we insert a guard_env instruction after the call
        // to optimistically assume this is still the case after the call.
        // TODO: right now we do not immediately use the new won knowledge,
        // since for that we'd have to rerun the analysis, which we cannot,
        // since the code is not commited yet. Therefore the effect of the
        // guard_env on this ldvar-site will only become apparent on the next
        // run of this optimization pass (we assume it's run multiple times).

        if (steam && lastCall != code_.end()) {
            auto vb = analysis[lastCall][sym];
            if (vb.isValue() || vb.t == FValue::Type::Argument) {
                if (lastCall.hasOrigin()) {
                    // Get a new deopt id for this guard, pointing to the pc
                    // right after the call.
                    Opcode* deoptTarget = lastCall.origin();
                    BC::advance(&deoptTarget);
                    uint32_t deoptId =
                        Deoptimizer_register((OpcodeT*)deoptTarget);
                    // Insert the guard
                    (lastCall + 1).asCursor(code_) << BC::guardEnv(deoptId);
                    // Prevent multiple guards being inserted before the
                    // analysis was rerun.
                    steam = false;
                }
            }
        }
    }

    void run() {
        steam = initSteam;
        lastCall = code_.end();
        analysis.analyze(code_);
        for (auto i = code_.begin(); i != code_.end(); ++i) {
            dispatcher.dispatch(i);
        }
    }
};
}
#endif
