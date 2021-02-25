#ifndef PASS_DEFINITIONS_H
#define PASS_DEFINITIONS_H

#include "pass.h"

namespace rir {
namespace pir {

class LogStream;
class Closure;

#define PASS(name, __runOnPromises__)                                          \
    name:                                                                      \
  public                                                                       \
    Pass {                                                                     \
      public:                                                                  \
        name() : Pass(#name){};                                                \
        bool apply(Compiler& cmp, ClosureVersion* function, Code* code,        \
                   LogStream& log) const final override;                       \
        bool runOnPromises() const final override {                            \
            return __runOnPromises__;                                          \
        }                                                                      \
    };

/*
 * Uses scope analysis to get rid of as many `LdVar`'s as possible.
 *
 * Similar to llvm's mem2reg pass, we try to lift as many loads from the R
 * environment, to pir SSA variables.
 *
 */
class PASS(ScopeResolution, false);

/*
 * ElideEnv removes envrionments which are not needed. It looks at all uses of
 * a `MkEnv` instruction. If the environment does not leak, and none of the
 * uses have any effect (besides changing the unnecessary environment), then it
 * can be removed.
 *
 */

class PASS(ElideEnv, true);

/*
 * This pass searches for dominating force instructions.
 *
 * If we identify such an instruction, and we statically know which promise is
 * being forced, then it inlines the promise code at the place of the
 * dominating force, and replaces all subsequent forces with its result.
 *
 */
class PASS(ForceDominance, false);

/*
 * DelayInstr tries to schedule instructions right before they are needed.
 *
 */
class PASS(DelayInstr, false);

/*
 * The DelayEnv pass tries to delay the scheduling of `MkEnv` instructions as
 * much as possible. In case an environment is only necessary in some traces,
 * the goal is to move it out of the others.
 *
 */
class PASS(DelayEnv, false);

/*
 * Inlines a closure. Intentionally stupid. It does not resolve inner
 * environments, but rather just copies instructions and leads to functions
 * with multiple environments. Later scope resolution and force dominance
 * passes will do the smart parts.
 */
class PASS(Inline, false);

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
class PASS(ElideEnvSpec, false);

/*
 * Constantfolding and dead branch removal.
 */
class PASS(Constantfold, true);

// Constantfolding to be used in rir2pi
class PASS(EarlyConstantfold, true);

/*
 * Generic instruction and controlflow cleanup pass.
 */
class PASS(Cleanup, true);

/*
 * Checkpoints keep values alive. Thus it makes sense to remove them if they
 * are unused after a while.
 */
class PASS(CleanupCheckpoints, true);

/*
 * Unused framestate instructions usually get removed automatically. Except
 * some call instructions consume framestates, but just for the case where we
 * want to inline. This pass removes those framestates from the calls, such
 * that they can be removed later, if they are not actually used by any
 * checkpoint/deopt.
 */
class PASS(CleanupFramestate, true);

/*
 * Trying to group assumptions, by pushing them up. This well lead to fewer
 * checkpoints being used overall.
 */
class PASS(OptimizeAssumptions, false);

class PASS(EagerCalls, false);

class PASS(OptimizeVisibility, true);

class PASS(OptimizeContexts, false);

class PASS(DeadStoreRemoval, false);

class PASS(DotDotDots, false);

class PASS(MatchCallArgs, false);

/*
 * At this point, loop code invariant mainly tries to hoist ldFun operations
 * outside the loop in case it can prove that the loop body will not change
 * the binding
 */
class PASS(LoopInvariant, false);

class PASS(GVN, true);

class PASS(LoadElision, false);

class PASS(TypeInference, true);

class PASS(TypeSpeculation, false);

class PASS(PromiseSplitter, false);

/*
 * Range analysis to detect and optimize code which will not create overflows /
 * underflows
 */
class PASS(Overflow, true);

/*
 * Loop Invariant Code motion
 */
class PASS(HoistInstruction, false);

class PhaseMarker : public Pass {
  public:
    explicit PhaseMarker(const std::string& name) : Pass(name) {}
    bool apply(Compiler&, ClosureVersion*, Code*,
               LogStream&) const final override {
        return false;
    }
    bool isPhaseMarker() const final override { return true; }
};

} // namespace pir
} // namespace rir

#undef PASS

#endif
