#ifndef RIR_OPTIMIZER_CLEANUP_H
#define RIR_OPTIMIZER_CLEANUP_H

#include "code/dataflow.h"

namespace rir {

class BCCleanup : public InstructionDispatcher::Receiver {
  public:
    DataflowAnalysis<Type::Conservative> analysis;
    InstructionDispatcher dispatcher;
    CodeEditor& code_;

    BCCleanup(CodeEditor& code) : dispatcher(*this), code_(code) {}

    void pop_(CodeEditor::Iterator ins) override {
        auto v = analysis[ins].top();
        if (!v.singleDef())
            return;

        auto defI = v.value.u.def;
        BC def = *defI;

        // push - pop elimination
        if (!v.used()) {
            if (def.is(BC_t::push_) || def.is(BC_t::dup_)) {
                defI.asCursor(code_).remove();
                ins.asCursor(code_).remove();
                return;
            }
        }

        // double load elimination : ldvar a; pop; ldvar a;
        if ((ins + 1) != code_.end()) {
            auto next = ins + 1;
            if ((*next).is(BC_t::ldvar_) && def == *next) {
                CodeEditor::Cursor cur = ins.asCursor(code_);
                cur.remove();
                cur.remove();
                return;
            }
        }
    }

    void ldfun_(CodeEditor::Iterator ins) override {
        SEXP sym = Pool::get((*ins).immediate.pool);
        auto v = analysis[ins][sym];
        if (v.t == FValue::Type::Constant) {
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
        if (v.t == FValue::Type::Argument) {
            auto cur = ins.asCursor(code_);
            cur.remove();
            cur << BC::ldarg(sym);
        }
        if (v.t == FValue::Type::Constant) {
            SEXP constant = v.constant();
            if (TYPEOF(constant) == CLOSXP && TYPEOF(BODY(constant)) != INTSXP)
                return;
            // TODO: this breaks tools/Rj while loading a package, but why???
            if (TYPEOF(constant) == STRSXP)
                return;
            auto cur = ins.asCursor(code_);
            cur.remove();
            cur << BC::push(constant);
        }

        // double load elimination : ldvar a; ldvar a;
        if (ins != code_.begin()) {
            auto prev = ins - 1;
            if ((*prev).is(BC_t::ldvar_) && *ins == *prev) {
                CodeEditor::Cursor cur = ins.asCursor(code_);
                cur.remove();
                cur << BC::dup();
                return;
            }
        }
    }

    void invisible_(CodeEditor::Iterator ins) override {
        if ((ins + 1) != code_.end()) {
            if ((*(ins + 1)).is(BC_t::pop_) ||
                (*(ins + 1)).is(BC_t::visible_) ||
                (*(ins + 1)).is(BC_t::ldvar_)) {
                ins.asCursor(code_).remove();
            }
        }
    }

    void uniq_(CodeEditor::Iterator ins) override {
        if ((ins + 1) != code_.end()) {
            if ((*(ins + 1)).is(BC_t::stvar_) || (*(ins + 1)).is(BC_t::pop_) ||
                (*(ins + 1)).is(BC_t::subassign_) ||
                (*(ins + 1)).is(BC_t::subassign2_)) {
                ins.asCursor(code_).remove();
            }
        }
    }

    void guard_fun_(CodeEditor::Iterator ins) override {
        if (ins != code_.begin()) {
            auto prev = ins - 1;
            if ((*prev).is(BC_t::guard_fun_) && *ins == *prev) {
                ins.asCursor(code_).remove();
                return;
            }
        }

        SEXP sym = Pool::get((*ins).immediate.guard_fun_args.name);
        auto v = analysis[ins][sym];
        if (v.t == FValue::Type::Constant) {
            SEXP constant = v.constant();
            SEXP expected = Pool::get((*ins).immediate.guard_fun_args.expected);
            if (constant == expected) {
                auto cur = ins.asCursor(code_);
                cur.remove();
                return;
            }
        }

        CodeEditor::Iterator bubbleUp = ins;
        while (bubbleUp != code_.begin()) {
            bubbleUp = bubbleUp - 1;
            if ((*bubbleUp).is(BC_t::label) || !(*bubbleUp).isPure()) {
                bubbleUp = bubbleUp + 1;
                break;
            }
        }
        if (bubbleUp != ins) {
            bubbleUp.asCursor(code_) << *ins;
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
        for (auto i = code_.begin(); i != code_.end(); ++i) {
            dispatcher.dispatch(i);
        }
    }
};
}
#endif
