#pragma once

#include "compiler/pir/instruction.h"
#include "constants.h"
#include "representation.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/MDBuilder.h"

namespace rir {
namespace pir {

using namespace llvm;

struct Variable {
    enum Kind {
        MutableLocalRVariable,
        ImmutableLocalRVariable,
        MutablePrimitive,
        ImmutablePrimitive,
    };
    Kind kind;

    static Variable MutableRVariable(Instruction* i, size_t pos,
                                     IRBuilder<>& builder,
                                     llvm::Value* basepointer) {
        auto v = RVariable(i, pos, builder, basepointer);
        v.kind = MutableLocalRVariable;
        return v;
    }

    static Variable RVariable(Instruction* i, size_t pos, IRBuilder<>& builder,
                              llvm::Value* basepointer) {
        assert(i->producesRirResult());
        assert(!LdConst::Cast(i));
        assert(!CastType::Cast(i));
        assert(representationOf(i) == Representation::Sexp);
        auto ptr = builder.CreateGEP(basepointer, {c(pos), c(1)});
        ptr->setName(i->getRef());
        return {ImmutableLocalRVariable, ptr, false};
    }

    static Variable Mutable(Instruction* i, AllocaInst* location) {
        assert(i->producesRirResult());
        auto r = representationOf(i);
        assert(r != Representation::Sexp);
        location->setName(i->getRef());
        return {MutablePrimitive, location, false};
    }

    static Variable Immutable(Instruction* i) {
        assert(i->producesRirResult());
        auto r = representationOf(i);
        assert(r != Representation::Sexp);
        return {ImmutablePrimitive, nullptr, false};
    }

    llvm::Value* get(IRBuilder<>& builder) {
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

    void update(IRBuilder<>& builder, llvm::Value* val,
                bool volatile_ = false) {
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

    void set(IRBuilder<>& builder, llvm::Value* val, bool volatile_ = false) {
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

    llvm::Value* slot;
    bool initialized;
};

} // namespace pir
} // namespace rir