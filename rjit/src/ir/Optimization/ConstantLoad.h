#ifndef CONSTANT_LOAD
#define CONSTANT_LOAD

#include "ir/Pass.h"
#include "ir/PassDriver.h"

#include "api.h"

#include "RIntlns.h"

#include <unordered_map>

namespace rjit {
namespace ir {

class ConstantLoadPass : public Pass {
  public:
    ConstantLoadPass() : Pass() {}

    match u(UserLiteral* var) {
        Module* m = var->r()->getParent()->getParent()->getParent();
        auto res = ir::VectorGetElement::create(
            var->start(), m->getContext(), var->constantPool(),
            ir::Builder::integer(var->index()));
        ir::MarkNotMutable::create(var->start(), m->getContext(), res->r());
        var->r()->replaceAllUsesWith(res->r());
        var->r()->removeFromParent();
    }
    match c(Constant* var) {
        Module* m = var->r()->getParent()->getParent()->getParent();
        auto res = ir::VectorGetElement::create(
            var->start(), m->getContext(), var->constantPool(),
            ir::Builder::integer(var->index()));
        var->r()->replaceAllUsesWith(res->r());
        var->r()->removeFromParent();
    }

    bool dispatch(llvm::BasicBlock::iterator& i) override;
};

class ConstantLoadOptimization : public ForwardDriver<ConstantLoadPass> {};
}
}

#endif
