#ifndef PIR_DELAY_ENV_H
#define PIR_DELAY_ENV_H

#include "../translations/rir_compiler.h"

namespace rir {
namespace pir {

/*
 * The DelayEnv pass tries to delay the scheduling of `MkEnv` instructions as
 * much as possible. In case an environment is only necessary in some traces,
 * the goal is to move it out of the others.
 *
 */

class Closure;
class DelayEnv : public PirTranslator {
  public:
    DelayEnv() : PirTranslator("Delay Environment"){};

  protected:
    void applyTranslation(Closure* function);
};
}
}

#endif
