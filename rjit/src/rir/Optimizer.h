#ifndef RIR_OPTIMIZER_H
#define RIR_OPTIMIZER_H

#include "../Symbols.h"
#include "BC.h"
#include "CodeStream.h"
#include <cassert>

#include "CodeEditor.h"

namespace rjit {
namespace rir {

namespace {

// ============================================================================
// ==== Thats just a toy example of how to compile away a call to some specials
//
void optimize_(Code* fun);
void optimize(CodeEditor& e, Code* fun);

void inlProm(CodeEditor::Cursor cur, Code* fun, fun_idx_t idx) {
    CodeEditor ce(fun->children[idx]);
    ce.normalizeReturn();
    cur << ce;
}

void doInlineBlock(CodeEditor::Cursor& cur, Code* fun) {
    assert((*cur).bc == BC_t::ldfun_);
    cur.remove();
    BC bc = *cur;
    assert(bc.bc == BC_t::call_);
    cur.remove();

    cur << BC::check_primitive(symbol::Block);

    fun_idx_t* args = bc.immediateCallArgs();
    num_args_t nargs = bc.immediateCallNargs();

    for (int i = 0; i < nargs; ++i) {
        inlProm(cur, fun, args[i]);
        if (i != nargs - 1)
            cur << BC::pop();
    }
}

void doInlineIf(CodeEditor::Cursor& cur, Code* fun) {
    assert((*cur).bc == BC_t::ldfun_);
    cur.remove();
    BC bc = *cur;
    assert(bc.bc == BC_t::call_);
    cur.remove();

    cur << BC::check_primitive(symbol::If);

    fun_idx_t* args = bc.immediateCallArgs();
    num_args_t nargs = bc.immediateCallNargs();

    Label trueBranch = cur.mkLabel();
    Label nextBranch = cur.mkLabel();

    inlProm(cur, fun, args[0]);
    cur << BC::to_bool() << BC::jmp_true(trueBranch);

    if (nargs < 3) {
        cur << BC::push(R_NilValue);
    } else {
        inlProm(cur, fun, args[2]);
    }
    cur << BC::jmp(nextBranch);

    cur << BC::label(trueBranch);
    inlProm(cur, fun, args[1]);

    cur << BC::label(nextBranch);
}

void doInlinePar(CodeEditor::Cursor& cur, Code* fun) {
    assert((*cur).bc == BC_t::ldfun_);
    cur.remove();
    BC bc = *cur;
    assert(bc.bc == BC_t::call_);
    cur.remove();

    cur << BC::check_primitive(symbol::Parenthesis);

    fun_idx_t* args = bc.immediateCallArgs();
    num_args_t nargs = bc.immediateCallNargs();

    assert(nargs == 1);

    inlProm(cur, fun, args[0]);
}

void doInlineAdd(CodeEditor::Cursor& cur, Code* fun) {
    assert((*cur).bc == BC_t::ldfun_);
    cur.remove();
    BC bc = *cur;
    assert(bc.bc == BC_t::call_);
    cur.remove();

    cur << BC::check_primitive(symbol::Add);

    fun_idx_t* args = bc.immediateCallArgs();
    num_args_t nargs = bc.immediateCallNargs();

    assert(nargs == 2);
    inlProm(cur, fun, args[0]);
    inlProm(cur, fun, args[1]);
    cur << BC::add();
}

void doInlineSub(CodeEditor::Cursor& cur, Code* fun) {
    assert((*cur).bc == BC_t::ldfun_);
    cur.remove();
    BC bc = *cur;
    assert(bc.bc == BC_t::call_);
    cur.remove();

    cur << BC::check_primitive(symbol::Sub);

    fun_idx_t* args = bc.immediateCallArgs();
    num_args_t nargs = bc.immediateCallNargs();

    assert(nargs == 2);
    inlProm(cur, fun, args[0]);
    inlProm(cur, fun, args[1]);
    cur << BC::sub();
}

void doInlineLt(CodeEditor::Cursor& cur, Code* fun) {
    assert((*cur).bc == BC_t::ldfun_);
    cur.remove();
    BC bc = *cur;
    assert(bc.bc == BC_t::call_);
    cur.remove();

    cur << BC::check_primitive(symbol::Lt);

    fun_idx_t* args = bc.immediateCallArgs();
    num_args_t nargs = bc.immediateCallNargs();

    assert(nargs == 2);
    inlProm(cur, fun, args[0]);
    inlProm(cur, fun, args[1]);
    cur << BC::lt();
}

void optimize(CodeEditor& e, Code* fun) {
    for (auto cur = e.getCursor(); !cur.atEnd(); ++cur) {
        BC bc = *cur;

        switch (bc.bc) {
        case BC_t::ldfun_:
            if (bc.immediateConst() == symbol::If) {
                doInlineIf(cur, fun);
                continue;
            }
            if (bc.immediateConst() == symbol::Block) {
                doInlineBlock(cur, fun);
                continue;
            }
            if (bc.immediateConst() == symbol::Lt) {
                doInlineLt(cur, fun);
                continue;
            }
            if (bc.immediateConst() == symbol::Add) {
                doInlineAdd(cur, fun);
                continue;
            }
            if (bc.immediateConst() == symbol::Sub) {
                doInlineSub(cur, fun);
                continue;
            }
            if (bc.immediateConst() == symbol::Parenthesis) {
                doInlinePar(cur, fun);
                continue;
            }

            break;

        default:
            break;
        }
    }
}

void optimize_(Code* fun) {
    CodeEditor edit(fun);
    optimize(edit, fun);
    Code* opt = edit.toCode();
    *fun = std::move(*opt);
    delete opt;
}
}

class Optimizer {
  public:
    static void optimize(Code* fun) {
        for (int i = 0; i < 5; ++i) {
            optimize_(fun);
        }
    }
};

} // rir
} // rjit

#endif
