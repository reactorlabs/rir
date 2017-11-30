#ifndef PIR_FORCE_DOMINANCE_H
#define PIR_FORCE_DOMINANCE_H

namespace rir {
namespace pir {

class Function;
class ForceDominance {
  public:
    static void apply(Function*);
};
}
}

#endif
