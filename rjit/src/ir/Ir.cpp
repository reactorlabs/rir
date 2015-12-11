#include "Ir.h"

using namespace llvm;

namespace rjit {
namespace ir {

Instruction::InstructionKind Instruction::match(BasicBlock::iterator& i) {
    llvm::Instruction* ins = i;
    ++i; // move to next instruction
    if (isa<CallInst>(ins)) {
        InstructionKind t = Intrinsic::getIRType(ins);
        if (t != InstructionKind::unknown) {
            return t;
        }
    } else if (isa<ReturnInst>(ins)) {
        return InstructionKind::Return;
    } else if (isa<BranchInst>(ins)) {
        assert(cast<BranchInst>(ins)->isUnconditional() and
               "Conditional branch instruction should start with ICmpInst");
        return InstructionKind::Branch;
    } else if (isa<ICmpInst>(ins)) {
        assert(isa<BranchInst>(ins->getNextNode()) and
               "ICmpInst can only be followed by branch for now.");
        ++i; // move past the branch as well
        return InstructionKind::Cbr;
    }
    return InstructionKind::unknown;
}

void Cbr::create(Builder& b, Value* cond, BasicBlock* trueCase,
                 BasicBlock* falseCase) {
    ICmpInst* test = new ICmpInst(*b.block(), ICmpInst::ICMP_NE, cond,
                                  b.integer(0), "condition");
    BranchInst::Create(trueCase, falseCase, test, b);
}

char const* const Intrinsic::MD_NAME = "r_ir_type";

} // namespace ir

} // namespace rjit
