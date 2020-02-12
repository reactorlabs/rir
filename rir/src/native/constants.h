#pragma once

#include "llvm_imports.h"

#include "compiler/pir/instruction.h"
#include "jit.h"
#include "representation.h"

namespace rir {
namespace pir {

using namespace llvm;

static inline llvm::Constant* c(void* i) {
    return llvm::ConstantInt::get(rir::pir::Jit::C, APInt(64, (intptr_t)i));
}

static inline llvm::Constant* c(unsigned long i, int bs = 64) {
    return llvm::ConstantInt::get(rir::pir::Jit::C, APInt(bs, i));
}

static inline llvm::Constant* c(long i, int bs = 64) {
    return llvm::ConstantInt::get(rir::pir::Jit::C, APInt(bs, i));
}

static inline llvm::Constant* c(unsigned int i, int bs = 32) {
    return llvm::ConstantInt::get(rir::pir::Jit::C, APInt(bs, i));
}

static inline llvm::Constant* c(int i, int bs = 32) {
    return llvm::ConstantInt::get(rir::pir::Jit::C, APInt(bs, i));
}

static inline llvm::Constant* c(double d) {
    return llvm::ConstantFP::get(rir::pir::Jit::C, llvm::APFloat(d));
}

static inline llvm::Constant* c(const std::vector<unsigned int>& array) {
    std::vector<llvm::Constant*> init;
    for (const auto& e : array)
        init.push_back(c(e));
    auto ty = llvm::ArrayType::get(t::Int, array.size());
    return llvm::ConstantArray::get(ty, init);
}

static inline llvm::Constant* cTrue() {
    return llvm::ConstantInt::get(rir::pir::Jit::C, llvm::APInt(32, 1));
}

static inline llvm::Constant* cFalse() {
    return llvm::ConstantInt::get(rir::pir::Jit::C, llvm::APInt(32, 0));
}

static inline llvm::Value* globalConst(llvm::Constant* init,
                                       llvm::Type* ty = nullptr) {
    if (!ty)
        ty = init->getType();
    return new llvm::GlobalVariable(Jit::module(), ty, true,
                                    llvm::GlobalValue::PrivateLinkage, init);
};

} // namespace pir
} // namespace rir