#include "Ir.h"

using namespace llvm;

namespace rjit {
namespace ir {

char const* const Pattern::MD_NAME = "r_ir_type";

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
    return new Car(consValuePtr, ptr, value);
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
    return new Cdr(consValuePtr, ptr, value);
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
    return new Tag(consValuePtr, ptr, value);
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
    VectorGetElement * p = new VectorGetElement(res);
    p->attach(realVector);
    p->attach(payload);
    p->attach(payloadPtr);
    p->attach(el_ptr);
    return p;
}

} // namespace ir

} // namespace rjit
