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
        if (v.defs == StackV::top)
            return;

        auto defI = v.defs;
        BC def = *defI;

        // push - pop elimination
        if (v.uses == StackV::bottom) {
            if (def.is(BC_t::push_) || def.is(BC_t::dup_)) {
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

    void invisible_(CodeEditor::Iterator ins) override {
        if ((*(ins + 1)).is(BC_t::pop_) || (*(ins + 1)).is(BC_t::visible_) ||
            (*(ins + 1)).is(BC_t::ldvar_)) {
            ins.asCursor(code_).remove();
        }
    }

    void uniq_(CodeEditor::Iterator ins) override {
        if ((*(ins + 1)).is(BC_t::stvar_) || (*(ins + 1)).is(BC_t::pop_) ||
            (*(ins + 1)).is(BC_t::subassign_) ||
            (*(ins + 1)).is(BC_t::subassign2_)) {
            ins.asCursor(code_).remove();
        }
    }

    void guard_fun_(CodeEditor::Iterator ins) override {
        auto prev = ins - 1;
        if ((*prev).is(BC_t::guard_fun_) && *ins == *prev) {
            ins.asCursor(code_).remove();
        }
    }

    void ret_(CodeEditor::Iterator ins) override {
        auto prev = ins - 1;
        if ((*prev).isReturn()) {
            ins.asCursor(code_).remove();
        }
    }

    // TODO there is some brokennes when there is dead code
    // void br_(CodeEditor::Iterator ins) override {
    //     auto target = code_.target(*ins);
    //     auto cont = target+1;
    //     if ((*cont).isReturn()) {
    //         auto cur = ins.asCursor(code_);
    //         cur.remove();
    //         cur << *cont;
    //     }
    // }

    void run() {
        analysis.analyze(code_);
        for (auto i = code_.begin() + 1; i + 1 != code_.end(); ++i) {
            dispatcher.dispatch(i);
        }
    }
};
}
#endif
