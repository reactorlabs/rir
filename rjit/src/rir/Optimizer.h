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
//
// //
// ============================================================================
// // ==== Thats just a toy example of how to compile away a call to some
// specials
// //
// void optimize_(Code* fun);
// void optimize(CodeEditor& e, Code* fun);

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
        if (i != nargs - 1)
            ce.getCursorAtEnd() << BC::pop();
        cur << ce;
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
    } else {
        inlProm(e, cur, args[2]);
    }
    cur << BC::br(nextBranch);

    cur << BC::label(trueBranch);
    inlProm(e, cur, args[1]);

    cur << BC::label(nextBranch);
}

void doInlinePar(CodeEditor& e, CodeEditor::Cursor& cur) {
    assert((*cur).bc == BC_t::ldfun_);
    cur.remove();
    BC bc = *cur;
    assert(bc.bc == BC_t::call_);
    cur.remove();

    cur << BC::isspecial(symbol::Parenthesis);

    fun_idx_t* args = bc.immediateCallArgs();
    num_args_t nargs = bc.immediateCallNargs();

    assert(nargs == 1);

    inlProm(e, cur, args[0]);
}

// void doInlineAdd(CodeEditor::Cursor& cur, Code* fun) {
//     assert((*cur).bc == BC_t::ldfun_);
//     cur.remove();
//     BC bc = *cur;
//     assert(bc.bc == BC_t::call_);
//     cur.remove();
//
//     cur << BC::check_primitive(symbol::Add);
//
//     fun_idx_t* args = bc.immediateCallArgs();
//     num_args_t nargs = bc.immediateCallNargs();
//
//     assert(nargs == 2);
//     inlProm(cur, fun, args[0]);
//     inlProm(cur, fun, args[1]);
//     cur << BC::add();
// }
//
// void doInlineSub(CodeEditor::Cursor& cur, Code* fun) {
//     assert((*cur).bc == BC_t::ldfun_);
//     cur.remove();
//     BC bc = *cur;
//     assert(bc.bc == BC_t::call_);
//     cur.remove();
//
//     cur << BC::check_primitive(symbol::Sub);
//
//     fun_idx_t* args = bc.immediateCallArgs();
//     num_args_t nargs = bc.immediateCallNargs();
//
//     assert(nargs == 2);
//     inlProm(cur, fun, args[0]);
//     inlProm(cur, fun, args[1]);
//     cur << BC::sub();
// }
//
// void doInlineLt(CodeEditor::Cursor& cur, Code* fun) {
//     assert((*cur).bc == BC_t::ldfun_);
//     cur.remove();
//     BC bc = *cur;
//     assert(bc.bc == BC_t::call_);
//     cur.remove();
//
//     cur << BC::check_primitive(symbol::Lt);
//
//     fun_idx_t* args = bc.immediateCallArgs();
//     num_args_t nargs = bc.immediateCallNargs();
//
//     assert(nargs == 2);
//     inlProm(cur, fun, args[0]);
//     inlProm(cur, fun, args[1]);
//     cur << BC::lt();
// }

void optimize(CodeEditor& e) {
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
            //             if (bc.immediateConst() == symbol::Lt) {
            //                 doInlineLt(cur, fun);
            //                 continue;
            //             }
            //             if (bc.immediateConst() == symbol::Add) {
            //                 doInlineAdd(cur, fun);
            //                 continue;
            //             }
            //             if (bc.immediateConst() == symbol::Sub) {
            //                 doInlineSub(cur, fun);
            //                 continue;
            //             }
            if (bc.immediateConst() == symbol::Parenthesis) {
                doInlinePar(e, cur);
                continue;
            }
            break;

        default:
            break;
        }
    }
}

FunctionHandle optimize_(FunctionHandle fun) {
    CodeEditor edit(fun);
    // std::cout << "==================================\nbefore \n";
    // edit.print();
    optimize(edit);
    // std::cout << "==================================\nafter \n";
    // edit.print();
    FunctionHandle res = edit.finalize();
    return res;
}
}

class Optimizer {
  public:
    static FunctionHandle optimize(FunctionHandle fun) {
        for (int i = 0; i < 5; ++i)
            fun = optimize_(fun);
        return fun;
    }
};

} // rir
} // rjit

#endif
