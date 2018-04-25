#ifndef PIR_FORCE_DOMINANCE_H
#define PIR_FORCE_DOMINANCE_H

#include "../translations/ir_translator.h"

namespace rir {
namespace pir {

/*
 * This pass searches for dominating force instructions.
 *
 * If we identify such an instruction, and we statically know which promise is
 * being forced, then it inlines the promise code at the place of the
 * dominating force, and replaces all subsequent forces with it's result.
 *
 */
class Closure;
class ForceDominance : public IRTranslator {
  public:
    ForceDominance(RirCompiler& compiler) : IRTranslator(compiler, "force dominance") {};

    void apply(IRCode) override;
};
}
}

#endif
