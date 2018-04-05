#ifndef PIR_CLEANUP_H
#define PIR_CLEANUP_H

namespace rir {
namespace pir {

class Closure;
class Cleanup {
  public:
    static void apply(Closure* function);
};
}
}

#endif
