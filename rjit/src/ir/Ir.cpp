#include "Ir.h"

using namespace llvm;

namespace rjit {
namespace ir {

char const* const Pattern::MD_NAME = "r_ir_type";

bool Pattern::isInstruction(llvm::Instruction* ins) {
    return ins->getMetadata(MD_NAME) ? true : false;
}

/** Returns the IR type of the intrinsic call for faster matching.
 */
Pattern* Pattern::getIR(llvm::Instruction* ins) {
    llvm::MDNode* m = ins->getMetadata(MD_NAME);
    assert(m);
    llvm::Metadata* mx = m->getOperand(0);
    llvm::APInt const& ap =
        llvm::cast<llvm::ConstantInt>(
            llvm::cast<llvm::ValueAsMetadata>(mx)->getValue())
            ->getUniqueInteger();
    assert(ap.isIntN(64) and "Expected 64bit integer");
    Pattern* res = reinterpret_cast<Pattern*>(ap.getZExtValue());
    assert(res);
    return res;
}

/** TODO the match should return the IR type pointer itself.
  */
Pattern* Pattern::match(BasicBlock::iterator& i) {
    llvm::Instruction* ins = &*i;
    ++i; // move to next instruction
    Pattern* rjitIns = Pattern::getIR(ins);
    assert(rjitIns);
    switch (rjitIns->getKind()) {
    case Pattern::PatternKind::Cbr:
        assert(isa<BranchInst>(ins->getNextNode()) and
               "ICmpInst can only be followed by branch for now.");
        ++i; // move past the branch as well
        break;
    default:
        // pass
        break;
    }
    return rjitIns;
}

void Cbr::create(Builder& b, Value* cond, BasicBlock* trueCase,
                 BasicBlock* falseCase) {
    ICmpInst* test = new ICmpInst(*b.block(), ICmpInst::ICMP_NE, cond,
                                  b.integer(0), "condition");
    BranchInst::Create(trueCase, falseCase, test, b);
}

} // namespace ir

} // namespace rjit
