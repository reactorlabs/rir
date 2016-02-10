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
        auto ve = VectorGetElement::insertBefore(
            var, var->constantPool(), Builder::integer(var->index()));
        MarkNotMutable::insertBefore(var, ve->result());
        replaceAllUsesWith(var, ve);
        eraseFromParent(var);
    }

    match c(Constant* var) {
        auto ve = VectorGetElement::insertBefore(
            var, var->constantPool(), Builder::integer(var->index()));
        replaceAllUsesWith(var, ve);
        eraseFromParent(var);
    }

    bool dispatch(llvm::BasicBlock::iterator& i) override;
};

class ConstantLoadOptimization : public ForwardDriver<ConstantLoadPass> {};
}
}

#endif
