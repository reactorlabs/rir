#ifndef RIR_OPTIMIZER_CLEANUP_H
#define RIR_OPTIMIZER_CLEANUP_H

#include "analysis/dataflow.h"

namespace rir {

class BCCleanup : public InstructionDispatcher::Receiver {
  public:
    DataflowAnalysis<Type::Conservative> analysis;
    InstructionDispatcher dispatcher;
    CodeEditor& code_;
    bool leaksEnvironment;

    BCCleanup(CodeEditor& code) : dispatcher(*this), code_(code) {}

    void nop_(CodeEditor::Iterator ins) override {
        CodeEditor::Cursor cur = ins.asCursor(code_);
        cur.remove();
        return;
    }

    void pop_(CodeEditor::Iterator ins) override {
        auto v = analysis[ins].top();
        if (!v.singleDef())
            return;

        auto defI = v.u.def;
        BC def = *defI;

        bool isDup = def.is(Opcode::dup_);
        // push - pop elimination
        if (def.is(Opcode::push_) || isDup) {
            bool used = v.used();
            if (used && v.singleUse()) {
                CodeEditor::Iterator use = v.use();
                if ((*use).is(Opcode::set_shared_) ||
                    (*use).is(Opcode::make_unique_)) {
                    use.asCursor(code_).remove();
                    used = false;
                }
            }

            if (!used) {
                defI.asCursor(code_).remove();
                ins.asCursor(code_).remove();
                /* if we remove a dup instruction then the potentially
                 * following set_shared_ becomes obsolete as well */
                if (isDup) {
                    if ((defI + 1) != code_.end()) {
                        auto next = defI + 1;
                        if ((*next).is(Opcode::set_shared_)) {
                            next.asCursor(code_).remove();
                        }
                    }
                }
                return;
            }
        }

        // double load elimination : ldvar a; pop; ldvar a;
        if ((ins + 1) != code_.end()) {
            auto next = ins + 1;
            if ((*next).is(Opcode::ldvar_) && def == *next) {
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
            if (TYPEOF(constant) == CLOSXP &&
                TYPEOF(BODY(constant)) != EXTERNALSXP)
                return;
            auto cur = ins.asCursor(code_);
            cur.remove();
            cur << BC::push(constant);
            return;
        }
    }

    void ldvar_(CodeEditor::Iterator ins) override {
        // double load elimination : ldvar a; ldvar a;
        if (ins != code_.begin()) {
            auto prev = ins - 1;
            if ((*prev).is(Opcode::ldvar_) && *ins == *prev) {
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
            if ((*(ins + 1)).is(Opcode::pop_) ||
                (*(ins + 1)).is(Opcode::visible_) ||
                (*(ins + 1)).is(Opcode::ldvar_)) {
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
            CodeEditor::Iterator bubbleUp = ins;
            while (bubbleUp != code_.begin()) {
                bubbleUp = bubbleUp - 1;
                auto cur = *bubbleUp;
                // We cannot move the guard across those instructions
                if (cur.is(Opcode::label) || !cur.isPure() || cur.isReturn())
                    break;
                if (cur == *ins) {
                    // This guard is redundant, remove it
                    ins.asCursor(code_).remove();
                    break;
                }
            }
        }
    }

    void pick_(CodeEditor::Iterator ins) override {
        // double pick elimination : pick 1; pick 1;
        if (ins != code_.begin() && ins.asCursor(code_).bc().immediate.i == 1) {
            auto prev = ins - 1;
            if ((*prev).is(Opcode::pick_) && *ins == *prev) {
                CodeEditor::Cursor cur = prev.asCursor(code_);
                cur.remove();
                cur.remove();
                return;
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
