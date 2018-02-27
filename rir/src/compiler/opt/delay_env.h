#ifndef PIR_DELAY_ENV_H
#define PIR_DELAY_ENV_H

namespace rir {
namespace pir {

/*
 * The DelayEnv pass tries to delay the scheduling of `MkEnv` instructions as
 * much as possible. In case an envrionment is only necessary in some traces,
 * the goal is to move it out of the others.
 *
 */
class Function;
class DelayEnv {
  public:
    static void apply(Function*);
};
}
}

#endif
