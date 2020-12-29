#include "lower_function_llvm.h"
#include "representation_llvm.h"

namespace rir {
namespace pir {

bool LowerFunctionLLVM::Variable::deadMove(const Variable& other) const {
    return slot == other.slot ||
           (stackSlot != (size_t)-1 && stackSlot == other.stackSlot);
}

LowerFunctionLLVM::Variable
LowerFunctionLLVM::Variable::MutableRVariable(Instruction* i, size_t pos,
                                              llvm::IRBuilder<>& builder,
                                              llvm::Value* basepointer) {
    auto v = RVariable(i, pos, builder, basepointer);
    v.kind = MutableLocalRVariable;
    return v;
}

LowerFunctionLLVM::Variable
LowerFunctionLLVM::Variable::RVariable(Instruction* i, size_t pos,
                                       llvm::IRBuilder<>& builder,
                                       llvm::Value* basepointer) {
    assert(i->producesRirResult());
    assert(!LdConst::Cast(i));
    assert(Representation::Of(i) == Representation::Sexp);
    auto ptr = builder.CreateGEP(basepointer, {c(pos), c(1)});
    ptr->setName(i->getRef());
    return {ImmutableLocalRVariable, ptr, false, pos};
}

LowerFunctionLLVM::Variable
LowerFunctionLLVM::Variable::Mutable(Instruction* i,
                                     llvm::AllocaInst* location) {
    assert(i->producesRirResult());
    auto r = Representation::Of(i);
    assert(r != Representation::Sexp);
    location->setName(i->getRef());
    return {MutablePrimitive, location, false, (size_t)-1};
}

LowerFunctionLLVM::Variable
LowerFunctionLLVM::Variable::Immutable(Instruction* i) {
    assert(i->producesRirResult());
    auto r = Representation::Of(i);
    assert(r != Representation::Sexp);
    return {ImmutablePrimitive, nullptr, false, (size_t)-1};
}

llvm::Value* LowerFunctionLLVM::Variable::get(llvm::IRBuilder<>& builder) {
    assert(initialized);
    assert(slot);
    switch (kind) {
    case ImmutableLocalRVariable:
    case MutableLocalRVariable:
    case MutablePrimitive:
        return builder.CreateLoad(slot);
    case ImmutablePrimitive:
        return slot;
    }
    assert(false);
    return nullptr;
}

void LowerFunctionLLVM::Variable::update(llvm::IRBuilder<>& builder,
                                         llvm::Value* val, bool volatile_) {
    initialized = true;
    switch (kind) {
    case MutableLocalRVariable:
    case MutablePrimitive:
        assert(slot);
        builder.CreateStore(val, slot, volatile_);
        break;
    case ImmutableLocalRVariable:
    case ImmutablePrimitive:
        assert(false);
        break;
    }
}

void LowerFunctionLLVM::Variable::set(llvm::IRBuilder<>& builder,
                                      llvm::Value* val, bool volatile_) {
    assert(!initialized);
    initialized = true;
    switch (kind) {
    case ImmutableLocalRVariable:
    case MutableLocalRVariable:
    case MutablePrimitive:
        assert(slot);
        builder.CreateStore(val, slot, volatile_);
        break;
    case ImmutablePrimitive:
        slot = val;
        break;
    }
}

} // namespace pir
} // namespace rir
