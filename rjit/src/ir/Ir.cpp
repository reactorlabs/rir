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
    Pattern* rjitIns = Pattern::getIR(ins);
    assert(rjitIns->start() == ins);
    while (&*i && &*i != rjitIns->end())
        i++;
    assert(rjitIns->end() == &*i);
    ++i; // move to next instruction
    return rjitIns;
}

void Cbr::create(Builder& b, Value* cond, BasicBlock* trueCase,
                 BasicBlock* falseCase) {
    ICmpInst* test = new ICmpInst(*b.block(), ICmpInst::ICMP_NE, cond,
                                  b.integer(0), "condition");
    auto branch = BranchInst::Create(trueCase, falseCase, test, b);
    new Cbr(test, branch);
}

Car* Car::create(Builder& b, llvm::Value* sexp) {
    ConstantInt* int_0 =
        ConstantInt::get(b.getContext(), APInt(32, StringRef("0"), 10));
    ConstantInt* int_4 =
        ConstantInt::get(b.getContext(), APInt(32, StringRef("4"), 10));
    auto consValuePtr = GetElementPtrInst::Create(
        t::SEXPREC, sexp, std::vector<Value*>({int_0, int_4}), "", b.block());
    auto ptr = GetElementPtrInst::Create(t::SEXP_u1, consValuePtr,
                                         std::vector<Value*>({int_0, int_0}),
                                         "", b.block());
    auto value = new LoadInst(ptr, "", false, b.block());
    return new Car(consValuePtr, value);
}

Cdr* Cdr::create(Builder& b, llvm::Value* sexp) {
    ConstantInt* int_0 =
        ConstantInt::get(b.getContext(), APInt(32, StringRef("0"), 10));
    ConstantInt* int_1 =
        ConstantInt::get(b.getContext(), APInt(32, StringRef("1"), 10));
    ConstantInt* int_4 =
        ConstantInt::get(b.getContext(), APInt(32, StringRef("4"), 10));
    auto consValuePtr = GetElementPtrInst::Create(
        t::SEXPREC, sexp, std::vector<Value*>({int_0, int_4}), "", b.block());
    auto ptr = GetElementPtrInst::Create(t::SEXP_u1, consValuePtr,
                                         std::vector<Value*>({int_0, int_1}),
                                         "", b.block());
    auto value = new LoadInst(ptr, "", false, b.block());
    return new Cdr(consValuePtr, value);
}

Tag* Tag::create(Builder& b, llvm::Value* sexp) {
    ConstantInt* int_2 =
        ConstantInt::get(b.getContext(), APInt(32, StringRef("2"), 10));
    ConstantInt* int_0 =
        ConstantInt::get(b.getContext(), APInt(32, StringRef("0"), 10));
    ConstantInt* int_4 =
        ConstantInt::get(b.getContext(), APInt(32, StringRef("4"), 10));
    auto consValuePtr = GetElementPtrInst::Create(
        t::SEXPREC, sexp, std::vector<Value*>({int_0, int_4}), "", b.block());
    auto ptr = GetElementPtrInst::Create(t::SEXP_u1, consValuePtr,
                                         std::vector<Value*>({int_0, int_2}),
                                         "", b.block());
    auto value = new LoadInst(ptr, "", false, b.block());
    return new Tag(consValuePtr, value);
}

VectorGetElement* VectorGetElement::create(llvm::Instruction* insert,
                                           LLVMContext& c, llvm::Value* vector,
                                           llvm::Value* index) {
    ConstantInt* int64_1 = ConstantInt::get(c, APInt(64, StringRef("1"), 10));
    auto realVector = new BitCastInst(
        vector, PointerType::get(t::VECTOR_SEXPREC, 1), "", insert);
    auto payload =
        GetElementPtrInst::Create(t::VECTOR_SEXPREC, realVector,
                                  std::vector<Value*>({int64_1}), "", insert);
    auto payloadPtr =
        new BitCastInst(payload, PointerType::get(t::SEXP, 1), "", insert);
    GetElementPtrInst* el_ptr =
        GetElementPtrInst::Create(t::SEXP, payloadPtr, index, "", insert);
    auto res = new LoadInst(el_ptr, "", false, insert);
    res->setAlignment(8);
    return new VectorGetElement(realVector, res);
}

void MarkNotMutable::create(llvm::Instruction* insert, LLVMContext& c,
                            llvm::Value* val) {
    ConstantInt* int32_0 = ConstantInt::get(c, APInt(32, StringRef("0"), 10));
    ConstantInt* c1 = ConstantInt::get(c, APInt(32, StringRef("-193"), 10));
    ConstantInt* c2 = ConstantInt::get(c, APInt(32, StringRef("128"), 10));
    auto sexpinfo = GetElementPtrInst::Create(
        t::SEXPREC, val, std::vector<Value*>({int32_0, int32_0}), "", insert);
    auto sexpint =
        new BitCastInst(sexpinfo, PointerType::get(t::Int, 1), "", insert);
    auto sexpval = new LoadInst(sexpint, "", false, insert);
    sexpval->setAlignment(4);
    auto clear = llvm::BinaryOperator::Create(llvm::Instruction::And, sexpval,
                                              c1, "", insert);
    auto set = llvm::BinaryOperator::Create(llvm::Instruction::Or, clear, c2,
                                            "", insert);
    auto store = new StoreInst(set, sexpint, insert);
    store->setAlignment(4);
    new MarkNotMutable(sexpinfo, store);
}

} // namespace ir

} // namespace rjit
