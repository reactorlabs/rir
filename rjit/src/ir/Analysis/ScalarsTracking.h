#ifndef ANALYSIS_SCALARS_TRACKING_H
#define ANALYSIS_SCALARS_TRACKING_H

#include "ir/Builder.h"
#include "ir/Ir.h"
#include "ir/Pass.h"
#include "ir/PassDriver.h"
#include "ir/IrScalars.h"
#include "ir/primitive_calls.h"

namespace rjit {
namespace analysis {

/** Scalar tracking

  For any vector, tracks if its scalar value is available in any of the
  registers.

  Nullptr means the scalar is not available, any other value points to the
  unboxed scalar value.

  Getting the scalar value again is a no-op as the first available register will
  always be used. Setting scalar value always sets the mapping as well,
  invalidating any previous one because the scalar value now changed.
 */
class TrackingValue {
  public:
    TrackingValue() : ptr_(nullptr) {}

    TrackingValue(ir::Value ptr) : ptr_(ptr) {}

    llvm::Value* ptr() const { return ptr_; }

    bool mergeWith(TrackingValue const& other) {
        if (ptr_ == other.ptr_) {
            return false;
        } else {
            ptr_ = nullptr;
            return true;
        }
    }

    TrackingValue& operator=(ir::Value const& value) {
        ptr_ = value;
        return *this;
    }

    void update(ir::Value ptr) {
        if (ptr_ == nullptr)
            ptr_ = ptr;
    }

  private:
    llvm::Value* ptr_;
};

class ScalarsTrackingPass : public ir::Fixpoint<ir::AState<TrackingValue>> {
  public:
    typedef TrackingValue Value;
    typedef ir::AState<Value> State;

    /** we are only interested in getting 0th element of a vector that is sexp
      of either double or integer.

      TODO this can be relaxed for any types as the optimization would still
      work but I am keeping it simple for now.
     */
    match getVectorElement(ir::GetVectorElement* p) {
        llvm::Value* index = p->index();
        llvm::Type* type = p->type();
        if (llvm::isa<llvm::ConstantInt>(index) and
            ir::Builder::integer(index) == 0)
            if (type == t::Double or type == t::Int)
                state[p->vector()].update(p->result());
    }

    /** Setting scalar value to a vector associates the vector with the new
     * scalar overriding any possible existing associations.
     */
    match setVectorElement(ir::SetVectorElement* p) {
        llvm::Value* index = p->index();
        llvm::Type* type = p->type();
        if (llvm::isa<llvm::ConstantInt>(index) and
            ir::Builder::integer(index) == 0)
            if (type == t::Double or type == t::Int)
                state[p->vector()] = p->value();
    }

    /** Links the result SEXP to the scalar it is initialized with.
     */
    match createAndSetScalar(ir::CreateAndSetScalar* p) {
        state[p].update(p->scalar());
    }

    bool dispatch(llvm::BasicBlock::iterator& i) override;
};

class ScalarsTracking : public ir::ForwardDriver<ScalarsTrackingPass> {
  public:
    typedef TrackingValue Value;
    typedef ir::AState<Value> State;
};

} // namespace analysis
} // namespace rjit

#endif // ANALYSIS_SCALARS_TRACKING_H
