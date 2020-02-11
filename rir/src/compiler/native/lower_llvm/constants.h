#pragma once

#include "compiler/pir/instruction.h"
#include "representation.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/MDBuilder.h"

namespace rir {
namespace pir {

using namespace llvm;

LLVMContext& C = rir::pir::JitLLVM::C;

static llvm::Constant* c(void* i) {
    return llvm::ConstantInt::get(C, APInt(64, (intptr_t)i));
}

static llvm::Constant* c(unsigned long i, int bs = 64) {
    return llvm::ConstantInt::get(C, APInt(bs, i));
}

static llvm::Constant* c(long i, int bs = 64) {
    return llvm::ConstantInt::get(C, APInt(bs, i));
}

static llvm::Constant* c(unsigned int i, int bs = 32) {
    return llvm::ConstantInt::get(C, APInt(bs, i));
}

static llvm::Constant* c(int i, int bs = 32) {
    return llvm::ConstantInt::get(C, APInt(bs, i));
}

static llvm::Constant* c(double d) {
    return llvm::ConstantFP::get(C, llvm::APFloat(d));
}

static llvm::Constant* c(const std::vector<unsigned int>& array) {
    std::vector<llvm::Constant*> init;
    for (const auto& e : array)
        init.push_back(c(e));
    auto ty = llvm::ArrayType::get(t::Int, array.size());
    return llvm::ConstantArray::get(ty, init);
}

static llvm::Constant* cTrue() {
    return llvm::ConstantInt::get(C, llvm::APInt(32, 1));
}

static llvm::Constant* cFalse() {
    return llvm::ConstantInt::get(C, llvm::APInt(32, 0));
}

static llvm::Value* globalConst(llvm::Constant* init,
                                llvm::Type* ty = nullptr) {
    if (!ty)
        ty = init->getType();
    return new llvm::GlobalVariable(JitLLVM::module(), ty, true,
                                    llvm::GlobalValue::PrivateLinkage, init);
};

} // namespace pir
} // namespace rir