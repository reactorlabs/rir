#ifndef PIR_DELAY_ENV_H
#define PIR_DELAY_ENV_H

#include "../translations/ir_translator.h"

namespace rir {
namespace pir {

/*
 * The DelayEnv pass tries to delay the scheduling of `MkEnv` instructions as
 * much as possible. In case an environment is only necessary in some traces,
 * the goal is to move it out of the others.
 *
 */

class DelayEnv : public IRTranslator {
  public:
    DelayEnv(RirCompiler& compiler)
        : IRTranslator(compiler, "Delay Environment"){};

    void apply(IRCode) override;
};
}
}

#endif
