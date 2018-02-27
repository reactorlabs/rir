#ifndef PIR_CLEANUP_H
#define PIR_CLEANUP_H

namespace rir {
namespace pir {

class Function;
class Cleanup {
  public:
    static void apply(Function* function);
};
}
}

#endif
