#ifndef RSH_NATIVE_ALLOCATOR
#define RSH_NATIVE_ALLOCATOR

#include "compiler/analysis/cfg.h"
#include "compiler/analysis/liveness.h"
#include "compiler/pir/pir.h"

namespace rir {
namespace pir {

/*
 * NativeAllocator assigns each instruction to a local variable number, or the
 * stack. It uses the following algorithm:
 *
 * 1. Split phis with moves. This translates the IR to CSSA (see toCSSA).
 * 2. Compute liveness (see liveness.h):
 * 3. For now, just put everything on stack. (step 4 is thus skipped...)
 * 4. Assign the remaining Instructions to local RIR variable numbers
 *    (see computeAllocation):
 *    1. Coalesce all remaining phi with their inputs. This is save since we are
 *       already in CSSA. Directly allocate a register on the fly, such that.
 *    2. Traverse the dominance tree and eagerly allocate the remaining ones
 * 5. For debugging, verify the assignment with a static analysis that simulates
 *    the variable and stack usage (see verify).
 */
class NativeAllocator {
  private:
    typedef unsigned Slot;

    Code* code;
    DominanceGraph dom;

    const LivenessIntervals& livenessIntervals;

    const static Slot unassignedSlot = 0;
    size_t slots_ = 0;

    std::unordered_map<Instruction*, Slot> allocation;
    std::unordered_map<Instruction*, Slot> hints;

    bool needsASlot(Instruction* i) const;
    void compute();
    void verify();

  public:
    NativeAllocator(Code* code, const LivenessIntervals& livenessIntervals);
    bool needsAVariable(Instruction* i) const;
    Slot operator[](Instruction* i) const;
    size_t slots() const;
};

} // namespace pir
} // namespace rir

#endif
