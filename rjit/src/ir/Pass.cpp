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

void Optimization::replaceAllUsesWith(llvm::Value* o, Pattern* n) {
    changed_ = true;
    o->replaceAllUsesWith(n->ins_);
}

void Optimization::replaceAllUsesWith(Pattern* o, llvm::Value* n) {
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

    // Since we do not know the dependencies of those instructions, remove is
    // actually a bit of a dance. First we collect all affected instructions.
    // Then we remove them one by one, only ever taking out the unused ones.
    // Since we expect dependency to go backwards, we start from the back and in
    // most cases this should work in one pass. If they are used outside the
    // pattern we'll get stuck...
    int toRemove = p->length();

    if (toRemove == 1) {
        i->eraseFromParent();
        delete p;
        return;
    }

    std::vector<llvm::Instruction*> insts(toRemove);
    insts[0] = i;
    for (int j = 1; j < toRemove; ++j) {
        i = i->getNextNode();
        insts[j] = i;
    }
    assert(i == last);

    while (toRemove) {
        int pos = insts.size() - 1;
        while (pos >= 0) {
            llvm::Instruction* i = insts[pos];
            if (i && i->use_empty()) {
                insts[pos] = nullptr;
                i->eraseFromParent();
                toRemove--;
            }
            pos--;
        }
    }

    delete p;
}
}
}
