#ifndef OPTIMIZATION_BOXINGREMOVAL_H
#define OPTIMIZATION_BOXINGREMOVAL_H

#include "ir/Analysis/ScalarsTracking.h"

namespace rjit {
namespace optimization {

class BoxingRemoval;

/** Removes scalar unboxing (get element ptr) when the scalar is already present
 * in a register.
 */
class BoxingRemovalPass : public ir::Pass, public ir::Optimization {
  public:
    typedef analysis::ScalarsTracking::Value Value;

    /** Getting a scalar we already know can be replaced with the original
     * register.
     */
    match getElement(ir::GetVectorElement* p) {
        Value& v = st()[p->vector()];
        if (v.ptr() != nullptr and v.ptr() != p->result()) {
            replaceAllUsesWith(p, v.ptr());
            eraseFromParent(p);
        }
    }

    bool dispatch(llvm::BasicBlock::iterator& i) override;

  protected:
    friend class BoxingRemoval;

    analysis::ScalarsTrackingPass* st_ = nullptr;

    analysis::ScalarsTrackingPass& st() { return *st_; }
};

class BoxingRemoval : public ir::OptimizationDriver<BoxingRemovalPass,
                                                    analysis::ScalarsTracking> {
  protected:
    void setFunction(llvm::Function* f) override {
        ir::OptimizationDriver<BoxingRemovalPass,
                               analysis::ScalarsTracking>::setFunction(f);
        pass.st_ = getAnalysis<analysis::ScalarsTracking>().pass();
    }
};

} // namespace optimization
} // namespace rjit

#endif // OPTIMIZATION_BOXINGREMOVAL_H
