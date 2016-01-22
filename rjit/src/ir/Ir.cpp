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

VectorGetElement VectorGetElement::create(Builder& b, llvm::Value* vector,
                                          llvm::Value* index) {
    ConstantInt* int64_1 =
        ConstantInt::get(b.getContext(), APInt(64, StringRef("1"), 10));
    auto realVector = new BitCastInst(
        vector, PointerType::get(t::VECTOR_SEXPREC, 1), "", b.block());
    auto payload = GetElementPtrInst::Create(t::VECTOR_SEXPREC, realVector,
                                             std::vector<Value*>({int64_1}), "",
                                             b.block());
    auto payloadPtr =
        new BitCastInst(payload, PointerType::get(t::SEXP, 1), "", b.block());
    GetElementPtrInst* el_ptr =
        GetElementPtrInst::Create(t::SEXP, payloadPtr, index, "", b.block());
    auto res = new LoadInst(el_ptr, "", false, b.block());
    res->setAlignment(8);
    return res;
}

void MarkNotMutable::create(Builder& b, llvm::Value* val) {
    ConstantInt* int32_0 =
        ConstantInt::get(b.getContext(), APInt(32, StringRef("0"), 10));
    ConstantInt* c1 =
        ConstantInt::get(b.getContext(), APInt(32, StringRef("-193"), 10));
    ConstantInt* c2 =
        ConstantInt::get(b.getContext(), APInt(32, StringRef("128"), 10));
    auto sexpinfo = GetElementPtrInst::Create(
        t::SEXPREC, val, std::vector<Value*>({int32_0, int32_0}), "",
        b.block());
    auto sexpint =
        new BitCastInst(sexpinfo, PointerType::get(t::Int, 1), "", b.block());
    auto sexpval = new LoadInst(sexpint, "", false, b.block());
    sexpval->setAlignment(4);
    auto c = llvm::BinaryOperator::Create(llvm::Instruction::And, sexpval, c1,
                                          "", b.block());
    auto s = llvm::BinaryOperator::Create(llvm::Instruction::Or, c, c2, "",
                                          b.block());
    auto store = new StoreInst(s, sexpint, b.block());
    store->setAlignment(4);
}

} // namespace ir

} // namespace rjit
