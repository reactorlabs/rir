#ifndef RIR_OPTIMIZER_CLEANUP_H
#define RIR_OPTIMIZER_CLEANUP_H

#include "code/dataflow.h"

namespace rir {

class BCCleanup : public InstructionDispatcher::Receiver {
  public:
    DataflowAnalysis<Type::Conservative> analysis;
    InstructionDispatcher dispatcher;
    CodeEditor& code_;
    bool leaksEnvironment;

    BCCleanup(CodeEditor& code) : dispatcher(*this), code_(code) {}

    void pop_(CodeEditor::Iterator ins) override {
        auto v = analysis[ins].top();
        if (!v.singleDef())
            return;

        auto defI = v.u.def;
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
            SEXP constant = analysis.constant(v);
            if (TYPEOF(constant) == CLOSXP && TYPEOF(BODY(constant)) != INTSXP)
                return;
            auto cur = ins.asCursor(code_);
            cur.remove();
            cur << BC::push(constant);
            return;
        }
    }

    void ldvar_(CodeEditor::Iterator ins) override {
        SEXP sym = Pool::get((*ins).immediate.pool);
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

    void add_(CodeEditor::Iterator ins) override {
        auto b = analysis[ins].stack()[0];
        auto a = analysis[ins].stack()[1];
        if (a.t == FValue::Type::Constant && b.t == FValue::Type::Constant) {
            SEXP ca = analysis.constant(a);
            SEXP cb = analysis.constant(b);
            if (!isObject(ca) && !isObject(cb)) {
                Protect p;
                SEXP plus = Rf_install("+");
                SEXP c = LCONS(plus->u.symsxp.value,
                               LCONS(ca, LCONS(cb, R_NilValue)));
                p(c);
                SEXP res = Rf_eval(c, R_BaseEnv);
                auto cur = ins.asCursor(code_);
                cur.remove();
                cur << BC::pop() << BC::pop() << BC::push(res);
            }
        }
    }

    void sub_(CodeEditor::Iterator ins) override {
        auto b = analysis[ins].stack()[0];
        auto a = analysis[ins].stack()[1];
        if (a.t == FValue::Type::Constant && b.t == FValue::Type::Constant) {
            SEXP ca = analysis.constant(a);
            SEXP cb = analysis.constant(b);
            if (!isObject(ca) && !isObject(cb)) {
                Protect p;
                SEXP plus = Rf_install("-");
                SEXP c = LCONS(plus->u.symsxp.value,
                               LCONS(ca, LCONS(cb, R_NilValue)));
                p(c);
                SEXP res = Rf_eval(c, R_BaseEnv);
                auto cur = ins.asCursor(code_);
                cur.remove();
                cur << BC::pop() << BC::pop() << BC::push(res);
            }
        }
    }

    void mul_(CodeEditor::Iterator ins) override {
        auto b = analysis[ins].stack()[0];
        auto a = analysis[ins].stack()[1];
        if (a.t == FValue::Type::Constant && b.t == FValue::Type::Constant) {
            SEXP ca = analysis.constant(a);
            SEXP cb = analysis.constant(b);
            if (!isObject(ca) && !isObject(cb)) {
                Protect p;
                SEXP plus = Rf_install("*");
                SEXP c = LCONS(plus->u.symsxp.value,
                               LCONS(ca, LCONS(cb, R_NilValue)));
                p(c);
                SEXP res = Rf_eval(c, R_BaseEnv);
                auto cur = ins.asCursor(code_);
                cur.remove();
                cur << BC::pop() << BC::pop() << BC::push(res);
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
        SEXP sym = Pool::get((*ins).immediate.guard_fun_args.name);
        auto v = analysis[ins][sym];
        if (v.t == FValue::Type::Constant) {
            SEXP c = analysis.constant(v);
            SEXP expected = Pool::get((*ins).immediate.guard_fun_args.expected);
            if (c == expected) {
                auto cur = ins.asCursor(code_);
                cur.remove();
                return;
            }
        }

        if (ins != code_.begin()) {
            CodeEditor::Iterator bubbleUp = ins - 1;
            while (bubbleUp != code_.begin()) {
                bubbleUp = bubbleUp - 1;
                auto cur = *bubbleUp;
                // We cannot move the guard across those instructions
                if (cur.is(BC_t::label) || !cur.isPure() || cur.isReturn())
                    break;
                if (cur == *ins) {
                    // This guard is redundant, remove it
                    ins.asCursor(code_).remove();
                    break;
                }
            }
        }
    }

    void guard_local_(CodeEditor::Iterator ins) override {
        SEXP sym = Pool::get((*ins).immediate.guard_fun_args.name);
        auto v = analysis[ins][sym];
        if (v.isValue()) {
            auto cur = ins.asCursor(code_);
            cur.remove();
            return;
        }

        if (ins != code_.begin()) {
            CodeEditor::Iterator bubbleUp = ins - 1;
            while (bubbleUp != code_.begin()) {
                bubbleUp = bubbleUp - 1;
                auto cur = *bubbleUp;
                // We cannot move the guard across those instructions
                if (cur.is(BC_t::label) || !cur.isPure() || cur.isReturn())
                    break;
                if (cur == *ins) {
                    // This guard is redundant, remove it
                    ins.asCursor(code_).remove();
                    break;
                }
            }
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
        for (auto i = code_.begin(); i != code_.end(); ++i)
            dispatcher.dispatch(i);
    }
};
}
#endif
