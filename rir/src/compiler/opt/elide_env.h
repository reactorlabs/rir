#ifndef PIR_ELIDE_ENV_H
#define PIR_ELIDE_ENV_H

namespace rir {
namespace pir {

class Function;
class ElideEnv {
  public:
    static void apply(Function* function);
};
}
}

#endif
