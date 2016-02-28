#include "ir/Pass.h"
#include "ir/Ir.h"

namespace rjit {
namespace ir {

bool Pass::dispatch(llvm::BasicBlock::iterator& i) {
    Pattern* ins = rjit::ir::Pattern::get(i);
    if (ins != nullptr) {
        ins->advance(i);
        defaultMatch(ins);
    } else {
        defaultMatch(i);
        ++i;
    }
    return true;
}

void Optimization::replaceAllUsesWith(llvm::Value * o, Pattern* n) {
    changed_ = true;
    o->replaceAllUsesWith(n->ins_);
}

void Optimization::replaceAllUsesWith(Pattern* o, llvm::Value * n) {
    changed_ = true;
    o->ins_->replaceAllUsesWith(n);
}

void Optimization::replaceAllUsesWith(Pattern* o, Pattern* n) {
    changed_ = true;
    o->ins_->replaceAllUsesWith(n->ins_);
}

void Optimization::eraseFromParent(Pattern* p) {
    changed_ = true;
    llvm::Instruction* last = p->last();
    llvm::Instruction* i = p->first();
    while (true) {
        if (i == last) {
            i->eraseFromParent();
            delete p;
            return;
        }
        llvm::Instruction* ii = i;
        i = i->getNextNode();
        ii->eraseFromParent();
    }
}



}
}
