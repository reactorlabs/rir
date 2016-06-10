#ifndef RIR_OPTIMIZER_H
#define RIR_OPTIMIZER_H

#include "../Symbols.h"
#include "BC.h"
#include "CodeStream.h"
#include <cassert>

namespace rjit {
namespace rir {

namespace {

// ============================================================================
// ==== Thats just a toy example of how to compile away a call to some specials
//
void optimize_(Code* c);
void optimize(CodeStream& cs, Code* cur);

BC_t* doInlineIf(CodeStream& cs, Code* cur, BC_t* pc, BC_t* end) {

    cs << BC::check_special(symbol::If);

    BC bc = BC::advance(&pc);

    assert(bc.bc == BC_t::call);
    SEXP args_ = bc.immediateCallArgs();
    int* args = INTEGER(args_);
    int nargs = Rf_length(args_);

    Label trueBranch = cs.mkLabel();
    Label nextBranch = cs.mkLabel();

    optimize(cs, cur->children[args[0]]);
    cs << BC::to_bool() << BC::jmp_true(trueBranch);

    if (nargs < 3) {
        cs << BC::push(R_NilValue);
    } else {
        optimize(cs, cur->children[args[2]]);
    }
    cs << BC::jmp(nextBranch);

    cs << trueBranch;
    optimize(cs, cur->children[args[1]]);

    cs << nextBranch;

    return pc;
}

BC_t* doInlineBlock(CodeStream& cs, Code* cur, BC_t* pc, BC_t* end) {

    cs << BC::check_special(symbol::Block);

    BC bc = BC::advance(&pc);

    assert(bc.bc == BC_t::call);
    SEXP args_ = bc.immediateCallArgs();
    int* args = INTEGER(args_);
    int nargs = Rf_length(args_);

    for (int i = 0; i < nargs; ++i) {
        optimize(cs, cur->children[args[i]]);
    }

    return pc;
}

void optimize(CodeStream& cs, Code* cur) {
    BC_t* pc = cur->bc;
    BC_t* end = (BC_t*)(uintptr_t)pc + cur->size;

    while (pc != end) {
        BC bc = BC::advance(&pc);

        switch (bc.bc) {
        case BC_t::getfun:
            if (bc.immediateConst() == symbol::If) {
                pc = doInlineIf(cs, cur, pc, end);
                continue;
            }
            if (bc.immediateConst() == symbol::Block) {
                pc = doInlineBlock(cs, cur, pc, end);
                continue;
            }
            break;

        case BC_t::ret:
            continue;

        case BC_t::call: {
            SEXP args = bc.immediateCallArgs();
            for (int i = 0; i < Rf_length(args); ++i) {
                optimize_(cur->children[INTEGER(args)[i]]);
            }
            break;
        }

        default:
            break;
        }
        cs << bc;
    }
}

void optimize_(Code* c) {
    CodeStream opt(c->ast);
    optimize(opt, c);
    opt << BC::ret();

    Code* optCode = opt.toCode();
    *c = std::move(*optCode);
    delete optCode; // is empty now
}
}

class Optimizer {
  public:
    static void optimize(Code* fun) { optimize_(fun); }
};

} // rir
} // rjit

#endif
