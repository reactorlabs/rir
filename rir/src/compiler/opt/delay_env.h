#ifndef PIR_DELAY_ENV_H
#define PIR_DELAY_ENV_H

namespace rir {
namespace pir {

class Function;
class DelayEnv {
  public:
    static void apply(Function*);
};
}
}

#endif
