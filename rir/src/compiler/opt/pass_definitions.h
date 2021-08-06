#ifndef PASS_DEFINITIONS_H
#define PASS_DEFINITIONS_H

#include "pass.h"

namespace rir {
namespace pir {

class LogStream;
class Closure;

#define PASS(__name__, __runOnPromises__, __slow__)                            \
    class __name__ : public Pass, public detail::HasIDCounter<__name__> {      \
      public:                                                                  \
        __name__() : Pass(#__name__) {}                                        \
        bool apply(Compiler& cmp, ClosureVersion* function, Code* code,        \
                   LogStream& log) const final override;                       \
        bool runOnPromises() const final override {                            \
            return __runOnPromises__;                                          \
        }                                                                      \
        bool isSlow() const final override { return __slow__; }                \
        size_t id() const final override { return __name__::_id(); }           \
    }

/*
 * Uses scope analysis to get rid of as many `LdVar`'s as possible.
 *
 * Similar to llvm's mem2reg pass, we try to lift as many loads from the R
 * environment, to pir SSA variables.
 *
 */
PASS(ScopeResolution, false, true);

/*
 * ElideEnv removes envrionments which are not needed. It looks at all uses of
 * a `MkEnv` instruction. If the environment does not leak, and none of the
 * uses have any effect (besides changing the unnecessary environment), then it
 * can be removed.
 *
 */

PASS(ElideEnv, true, false);

/*
 * This pass searches for dominating force instructions.
 *
 * If we identify such an instruction, and we statically know which promise is
 * being forced, then it inlines the promise code at the place of the
 * dominating force, and replaces all subsequent forces with its result.
 *
 */
PASS(ForceDominance, false, true);

/*
 * DelayInstr tries to schedule instructions right before they are needed.
 *
 */
PASS(DelayInstr, false, false);

/*
 * The DelayEnv pass tries to delay the scheduling of `MkEnv` instructions as
 * much as possible. In case an environment is only necessary in some traces,
 * the goal is to move it out of the others.
 *
 */
PASS(DelayEnv, false, false);

/*
 * Inlines a closure. Intentionally stupid. It does not resolve inner
 * environments, but rather just copies instructions and leads to functions
 * with multiple environments. Later scope resolution and force dominance
 * passes will do the smart parts.
 */
PASS(Inline, false, false);

/*
 * Goes through every operation that for the general case needs an environment
 * but could be elided for some particular inputs. Analyzes the profiling
 * information of the inputs and if all the observed values are compatible with
 * the version operation without an environment, it avoids creating the
 * environment and add the corresponding guard to deoptimize in case an
 * incompatible input appears at run time. It also goes through every force
 * instruction for which we could not prove it does not access the parent
 * environment reflectively and speculate it will not.
 */
PASS(ElideEnvSpec, false, false);

/*
 * Constantfolding and dead branch removal.
 */
PASS(Constantfold, true, false);

// Constantfolding to be used in rir2pi
PASS(EarlyConstantfold, true, false);

/*
 * Generic instruction and controlflow cleanup pass.
 */
PASS(Cleanup, true, true);

/*
 * Checkpoints keep values alive. Thus it makes sense to remove them if they
 * are unused after a while.
 */
PASS(CleanupCheckpoints, true, false);

/*
 * Unused framestate instructions usually get removed automatically. Except
 * some call instructions consume framestates, but just for the case where we
 * want to inline. This pass removes those framestates from the calls, such
 * that they can be removed later, if they are not actually used by any
 * checkpoint/deopt.
 */
PASS(CleanupFramestate, true, false);

/*
 * Trying to group assumptions, by pushing them up. This well lead to fewer
 * checkpoints being used overall.
 */
PASS(OptimizeAssumptions, false, false);

PASS(EagerCalls, false, false);

PASS(OptimizeVisibility, true, false);

PASS(OptimizeContexts, false, false);

PASS(DeadStoreRemoval, false, true);

PASS(DotDotDots, false, false);

PASS(MatchCallArgs, false, false);

/*
 * At this point, loop code invariant mainly tries to hoist ldFun operations
 * outside the loop in case it can prove that the loop body will not change
 * the binding
 */
PASS(LoopInvariant, false, false);

PASS(GVN, true, true);

PASS(LoadElision, false, false);

PASS(TypeInference, true, false);

PASS(TypeSpeculation, false, false);

PASS(PromiseSplitter, false, false);

PASS(InlineForcePromises, false, false);

/*
 * Range analysis to detect and optimize code which will not create overflows /
 * underflows
 */
PASS(Overflow, true, false);

/*
 * Loop Invariant Code motion
 */
PASS(HoistInstruction, false, false);

class PhaseMarker : public Pass, detail::HasIDCounter<PhaseMarker> {
  public:
    explicit PhaseMarker(const std::string& name) : Pass(name) {}
    bool apply(Compiler&, ClosureVersion*, Code*,
               LogStream&) const final override {
        return false;
    }
    bool isPhaseMarker() const final override { return true; }
    size_t id() const final override { return PhaseMarker::_id(); }
};

} // namespace pir
} // namespace rir

#undef PASS

#endif
