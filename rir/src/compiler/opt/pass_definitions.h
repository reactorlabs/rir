#ifndef PASS_DEFINITIONS_H
#define PASS_DEFINITIONS_H

#include "../debugging/stream_logger.h"
#include "../translations/pir_translator.h"

namespace rir {
namespace pir {

class Closure;

#define PASS(name, desc)                                                       \
    name:                                                                      \
  public                                                                       \
    PirTranslator {                                                            \
      public:                                                                  \
        name() : PirTranslator(desc){};                                        \
        void apply(RirCompiler&, Closure* function, LogStream& log)            \
            const final override;                                              \
    };

/*
 * Uses scope analysis to get rid of as many `LdVar`'s as possible.
 *
 * Similar to llvm's mem2reg pass, we try to lift as many loads from the R
 * environment, to pir SSA variables.
 *
 */
class PASS(ScopeResolution, "Scope resolution");

/*
 * ElideEnv removes envrionments which are not needed. It looks at all uses of
 * a `MkEnv` instruction. If the environment does not leak, and none of the
 * uses have any effect (besides changing the unnecessary environment), then it
 * can be removed.
 *
 */

class PASS(ElideEnv, "Elide environments not needed");

/*
 * This pass searches for dominating force instructions.
 *
 * If we identify such an instruction, and we statically know which promise is
 * being forced, then it inlines the promise code at the place of the
 * dominating force, and replaces all subsequent forces with its result.
 *
 */
class PASS(ForceDominance, "Inline Promises");

/*
 * DelayInstr tries to schedule instructions right before they are needed.
 *
 */
class PASS(DelayInstr, "Delay instructions");

/*
 * The DelayEnv pass tries to delay the scheduling of `MkEnv` instructions as
 * much as possible. In case an environment is only necessary in some traces,
 * the goal is to move it out of the others.
 *
 */
class PASS(DelayEnv, "Move environment creation as far as possible");

/*
 * Inlines a closure. Intentionally stupid. It does not resolve inner
 * environments, but rather just copies instructions and leads to functions
 * with multiple environments. Later scope resolution and force dominance
 * passes will do the smart parts.
 */
class PASS(Inline, "Inline closures");

/*
 * Goes through every operation that for the general case needs an environment
 * but could be elided for some particular inputs. Analyzes the profiling
 * information of the inputs and if all the observed values are compatible with
 * the version operation without an environment, it avoids creating the
 * environment and add the corresponding guard to deoptimize in case an
 * incompatible input appears at run time.
 */
class PASS(ElideEnvSpec, "Speculate on values to elide environments");

/*
 * Constantfolding and dead branch removal.
 */
class PASS(Constantfold, "Constant folding");

/*
 * Generic instruction and controlflow cleanup pass.
 */
class PASS(Cleanup, "Cleanup redundant operations");

/*
 * Checkpoints keep values alive. Thus it makes sense to remove them if they
 * are unused after a while.
 */
class PASS(CleanupCheckpoints, "Cleanup unused checkpoints");

/*
 * Unused framestate instructions usually get removed automatically. Except
 * some call instructions consume framestates, but just for the case where we
 * want to inline. This pass removes those framestates from the calls, such
 * that they can be removed later, if they are not actually used by any
 * checkpoint/deopt.
 */
class PASS(CleanupFramestate, "Cleanup framestates unused by checkpoints");

} // namespace pir
} // namespace rir

#undef PASS

#endif
