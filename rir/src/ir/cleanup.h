#ifndef RIR_OPTIMIZER_CLEANUP_H
#define RIR_OPTIMIZER_CLEANUP_H

#include "analysis_framework/dispatchers.h"

namespace rir {

class BCCleanup : public InstructionDispatcher::Receiver {
  public:
    static constexpr int STEAM = 4;

    static void apply(CodeEditor& code) {
        BCCleanup c(code);
        for (int i = 0; i < STEAM; ++i) {
            c.run();
            code.commit();
        }
    }

    InstructionDispatcher dispatcher;
    CodeEditor& code_;
    bool leaksEnvironment;

    BCCleanup(CodeEditor& code) : dispatcher(*this), code_(code) {}

    void nop_(CodeEditor::Iterator ins) override {
        CodeEditor::Cursor cur = ins.asCursor(code_);
        cur.remove();
        return;
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
        SEXP name = Pool::get((*ins).immediate.guard_fun_args.expected);

        if (ins != code_.begin()) {
            CodeEditor::Iterator bubbleUp = ins;
            while (bubbleUp != code_.begin()) {
                bubbleUp = bubbleUp - 1;
                auto cur = *bubbleUp;
                // We cannot move the guard across those instructions
                if (cur.is(Opcode::label) || !cur.isPure() || cur.isReturn()) {
                    if (!cur.is(Opcode::stvar_)) {
                        break;
                    }
                    // stvar that does not interfere with the guard we can
                    // skip. Otherwise we treat it as a barrier. Note, this is a
                    // conservative approximation. Assigning to a variable with
                    // the same name does not guarantee that the guard fails.
                    // We could still:
                    // * override it with the same function
                    // * override it with a non-function value, which (due to
                    //   the amazing R lookup semantics) does not override
                    //   functions.
                    if (Pool::get(cur.immediate.pool) == name)
                        break;
                }
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

    void br_(CodeEditor::Iterator ins) override {
        auto next = removeDead(ins + 1);
        if (next == code_.end())
            return;
        // next is a label here
        auto target = code_.target(*ins);
        if (target == next) {
            ins.asCursor(code_).remove();
            return;
        }
        auto cont = target + 1;
        if (cont == code_.end())
            return;
        // if the continuation is return, propagate to the prev block
        if ((*cont).isReturn()) {
            auto cur = ins.asCursor(code_);
            cur.remove();
            cur << *cont;
        }
    }

    void brtrue_(CodeEditor::Iterator ins) override {
        // change brtrue_ x; br_ y; x: ... to brfalse_ y; x: ...
        if (ins + 1 != code_.end() && ins + 2 != code_.end()) {
            auto n = ins + 1;
            auto nn = ins + 2;
            if ((*n).is(Opcode::br_) && (*nn).isLabel() &&
                code_.target(*ins) == nn) {
                auto target = (*n).immediate.offset;
                auto cur = ins.asCursor(code_);
                cur.remove();
                cur.remove();
                cur << BC::brfalse(target);
            }
        }
    }

    void brfalse_(CodeEditor::Iterator ins) override {
        // change brfalse_ x; br_ y; x: ... to brtrue_ y; x: ...
        if (ins + 1 != code_.end() && ins + 2 != code_.end()) {
            auto n = ins + 1;
            auto nn = ins + 2;
            if ((*n).is(Opcode::br_) && (*nn).isLabel() &&
                code_.target(*ins) == nn) {
                auto target = (*n).immediate.offset;
                auto cur = ins.asCursor(code_);
                cur.remove();
                cur.remove();
                cur << BC::brtrue(target);
            }
        }
    }

    void ldloc_(CodeEditor::Iterator ins) override {
        // replace ldloc x; stloc y; by copyloc x y
        // remove ldloc x; stloc x
        if (ins + 1 != code_.end()) {
            auto next = ins + 1;
            if ((*next).is(Opcode::stloc_)) {
                auto src = (*ins).immediate.loc;
                auto trg = (*next).immediate.loc;
                // remove them
                auto cur = ins.asCursor(code_);
                cur.remove();
                cur.remove();
                if (src != trg)
                    cur << BC::copyloc(trg, src);
            }
        }
    }

    void get_env_(CodeEditor::Iterator ins) override {
        // remove get_env_; set_env_
        if (ins + 1 != code_.end()) {
            auto next = ins + 1;
            if ((*next).is(Opcode::set_env_)) {
                auto cur = ins.asCursor(code_);
                cur.remove();
                cur.remove();
            }
        }
    }

    void ret_(CodeEditor::Iterator ins) override { removeDead(ins + 1); }

    void return_(CodeEditor::Iterator ins) override { removeDead(ins + 1); }

    void run() {
        // Go over all instructions. Since some might get marked for deletion
        // before they are dispatched and the change commited, skip those here
        for (auto i = code_.begin(); i != code_.end(); ++i) {
            if (!i.deleted())
                dispatcher.dispatch(i);
        }
    }

  private:
    CodeEditor::Iterator removeDead(CodeEditor::Iterator ins) {
        // everything after br_ ret_ and return_ until end or next label is dead
        // code...
        auto cur = ins.asCursor(code_);
        while (!cur.atEnd() && !cur.bc().isLabel())
            cur.remove();
        return cur.asItr();
    }
};
} // namespace rir
#endif
