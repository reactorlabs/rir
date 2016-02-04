#ifndef CONSTANT_LOAD
#define CONSTANT_LOAD

#include "ir/Pass.h"
#include "ir/PassDriver.h"

#include "api.h"

#include "RIntlns.h"

#include <unordered_map>

namespace rjit {
namespace ir {

// TODO as a proof of concept this is great, however in long run it might be
// better to let llvm know about the primitive functions and inline them where
// appropriate?
class ConstantLoadPass : public Pass {
  public:
    ConstantLoadPass() : Pass() {}

    match u(UserLiteral* var) {
        // TODO This is ugly, make getModule() in pattern, or in the builder, in
        // fact TODO make the builder:)
        Module* m = var->result()->getParent()->getParent()->getParent();
        auto res = ir::VectorGetElement::create(
            var->first(), m->getContext(), var->constantPool(),
            ir::Builder::integer(var->index()));
        ir::MarkNotMutable::create(var->first(), m->getContext(),
                                   res->result());
        var->result()->replaceAllUsesWith(res->result());
        var->result()->removeFromParent();
    }

    match c(Constant* var) {
        Module* m = var->result()->getParent()->getParent()->getParent();
        auto res = ir::VectorGetElement::create(
            var->first(), m->getContext(), var->constantPool(),
            ir::Builder::integer(var->index()));
        var->result()->replaceAllUsesWith(res->result());
        var->result()->removeFromParent();
    }

    bool dispatch(llvm::BasicBlock::iterator& i) override;
};

class ConstantLoadOptimization : public ForwardDriver<ConstantLoadPass> {};
}
}

#endif
