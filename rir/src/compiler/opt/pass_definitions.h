#ifndef PASS_DEFINITIONS_H
#define PASS_DEFINITIONS_H

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
        void apply(Closure* function) const final override;                    \
    };

/*
 * Uses scope analysis to get rid of as many `LdVar`'s as possible.
 *
 * Similar to llvm's mem2reg pass, we try to lift as many loads from the R
 * environment, to pir SSA variables.
 *
 */
class PASS(ScopeResolution, "Dead store elimination");

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
 * dominating force, and replaces all subsequent forces with it's result.
 *
 */
class PASS(ForceDominance, "Eliminate redudant force instructions");

/*
 * DelayInstr tries to schedule instruction right before they are needed.
 *
 */
class PASS(DelayInstr, "Eliminate redudant force instructions");

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
 * This PASS goes through every operation that for the general case needs
 * an environment, but it could elide it for some particular inputs. The PASS
 * then analyzes the profiling information and if all the observed inputs are
 * compatible with the operation without an environment, it avoids creating the
 * environment and add the corresponding guard to deoptimize in case an
 * incompatible input appears at run time.
 */
class PASS(
    AdaptForSpec,
    "Adapt the CFG to the format required by pir's speculative operations");

/*
 * Goes through every operation that for the general case needs an environment
 * but could be elided for some particular inputs. Analyzes the profiling
 * information of the inputs and if all the observed values are compatible with
 * the version operation without an environment, it avoids creating the
 * environment and add the corresponding guard to deoptimize in case an
 * incompatible input appears at run time.
 */
class PASS(ElideEnvSpec, "Speculate on values to elide environments");

class PASS(Cleanup, "...");
class PASS(CleanupFrameState, "...");

} // namespace pir
} // namespace rir

#endif
