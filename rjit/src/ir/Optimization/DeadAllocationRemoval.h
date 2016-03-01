#ifndef OPTIMIZATION_DEADALLOCATIONREMOVAL_H
#define OPTIMIZATION_DEADALLOCATIONREMOVAL_H

#include "ir/Ir.h"
#include "ir/IrScalars.h"
#include "ir/Pass.h"
#include "ir/PassDriver.h"

namespace rjit {
namespace optimization {

/** Removes unnecessary allocation for scalars.

  Scalars use the CreateAndSetScalar pattern to allocate a scalar SEXP and set
  its value from the unboxed one. Because allocation is a call to C function,
  LLVM would not understand it as dead code so this simple pass deletes all
  scalar alocations that are only used to set the value, but never afterwards.
 */
class DeadAllocationRemovalPass : public ir::Pass, public ir::Optimization {
  public:
    match scalarAllocation(ir::CreateAndSetScalar* p) {
        if (p->result()->getNumUses() == 1)
            eraseFromParent(p);
    }

    bool dispatch(llvm::BasicBlock::iterator& i) override;
};

class DeadAllocationRemoval
    : public ir::LinearDriver<DeadAllocationRemovalPass> {
  public:
};

} // namespace optimization
} // namespace rjit

#endif // OPTIMIZATION_DEADALLOCATIONREMOVAL_H
