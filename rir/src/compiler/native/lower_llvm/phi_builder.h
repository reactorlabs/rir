#pragma once

#include "llvm_imports.h"

#include "compiler/pir/instruction.h"
#include "constants.h"
#include "representation.h"

namespace rir {
namespace pir {

using namespace llvm;

class PhiBuilder {
    std::vector<std::pair<llvm::Value*, llvm::BasicBlock*>> inputs;

    llvm::Type* type;
    IRBuilder<>& builder;
    bool created = false;

  public:
    PhiBuilder(IRBuilder<>& builder, llvm::Type* type)
        : type(type), builder(builder) {}

    void addInput(llvm::Value* v) { addInput(v, builder.GetInsertBlock()); }
    void addInput(llvm::Value* v, llvm::BasicBlock* b) {
        assert(!created);
        assert(v->getType() == type);
        inputs.push_back({v, b});
    }

    llvm::Value* operator()() {
        assert(!created);
        created = true;
        assert(inputs.size() > 0);
        if (inputs.size() == 1)
            return inputs[0].first;
        assert(builder.GetInsertBlock()->hasNPredecessors(inputs.size()));
        auto phi = builder.CreatePHI(type, inputs.size());
        for (auto& in : inputs)
            phi->addIncoming(in.first, in.second);
        return phi;
    }

    ~PhiBuilder() { assert(created && "dangling PhiBuilder"); }
};

} // namespace pir
} // namespace rir