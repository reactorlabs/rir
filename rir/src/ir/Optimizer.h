#ifndef RIR_OPTIMIZER_H
#define RIR_OPTIMIZER_H

#include "R/Symbols.h"
#include "BC.h"
#include "CodeStream.h"
#include <cassert>

#include "CodeEditor.h"

namespace rir {

namespace {

// ============================================================================
// ==== Thats just a toy example of how to compile away a call to some specials

void inlProm(CodeEditor& e, CodeEditor::Cursor cur, fun_idx_t idx) {
    CodeEditor& ce = *e.detachPromise(idx);
    ce.normalizeReturn();
    cur << ce;
}

void doInlineBlock(CodeEditor& e, CodeEditor::Cursor& cur) {
    assert((*cur).bc == BC_t::ldfun_);
    cur.remove();
    BC bc = *cur;
    assert(bc.bc == BC_t::call_);
    cur.remove();

    cur << BC::isspecial(symbol::Block);

    fun_idx_t* args = bc.immediateCallArgs();
    num_args_t nargs = bc.immediateCallNargs();

    if (nargs == 0) {
        cur << BC::push(R_NilValue);
        return;
    }

    for (size_t i = 0; i < nargs; ++i) {
        CodeEditor& ce = *e.detachPromise(args[i]);
        ce.normalizeReturn();
        cur << ce;
        if (i != nargs - 1)
            cur << BC::pop();
    }
}

void doInlineIf(CodeEditor& e, CodeEditor::Cursor& cur) {
    assert((*cur).bc == BC_t::ldfun_);
    cur.remove();
    BC bc = *cur;
    assert(bc.bc == BC_t::call_);
    cur.remove();

    cur << BC::isspecial(symbol::If);

    fun_idx_t* args = bc.immediateCallArgs();
    num_args_t nargs = bc.immediateCallNargs();

    Label trueBranch = cur.mkLabel();
    Label nextBranch = cur.mkLabel();

    inlProm(e, cur, args[0]);
    cur << BC::asbool() << BC::brtrue(trueBranch);

    if (nargs < 3) {
        cur << BC::push(R_NilValue);
        cur << BC::invisible();
    } else {
        inlProm(e, cur, args[2]);
    }
    cur << BC::br(nextBranch);

    cur << BC::label(trueBranch);
    inlProm(e, cur, args[1]);

    cur << BC::label(nextBranch);
}

// void doInlinePar(CodeEditor& e, CodeEditor::Cursor& cur) {
//     assert((*cur).bc == BC_t::ldfun_);
//     cur.remove();
//     BC bc = *cur;
//     assert(bc.bc == BC_t::call_);
//     cur.remove();
// 
//     cur << BC::isspecial(symbol::Parenthesis);
// 
//     fun_idx_t* args = bc.immediateCallArgs();
//     num_args_t nargs = bc.immediateCallNargs();
// 
//     assert(nargs == 1);
// 
//     inlProm(e, cur, args[0]);
// }

void optimize_(CodeEditor& e) {
    for (auto cur = e.getCursor(); !cur.atEnd(); ++cur) {
        BC bc = *cur;
        switch (bc.bc) {
        case BC_t::ldfun_:
            if (bc.immediateConst() == symbol::If) {
                doInlineIf(e, cur);
                continue;
            }
            if (bc.immediateConst() == symbol::Block) {
                doInlineBlock(e, cur);
                continue;
            }
            // TODO: in the current form breaks (...)
            // TODO: in the current form breaks (...)
            // if (bc.immediateConst() == symbol::Parenthesis) {
            //     doInlinePar(e, cur);
            //     continue;
            // }
            break;

        default:
            break;
        }
    }
}

}

class Optimizer {
  public:
    static FunctionHandle optimize(FunctionHandle fun) {
        CodeEditor edit(fun);
        for (int i = 0; i < 5; ++i) {
            optimize_(edit);
            if (!edit.changed)
                break;
        }
        return edit.finalize();
    }
};

} // rir

#endif
