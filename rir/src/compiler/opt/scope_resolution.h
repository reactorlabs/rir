#ifndef PIR_SCOPE_RESOLUTION_H
#define PIR_SCOPE_RESOLUTION_H

namespace rir {
namespace pir {

class Function;
class ScopeResolution {
  public:
    static void apply(Function* function);
};
}
}

#endif
