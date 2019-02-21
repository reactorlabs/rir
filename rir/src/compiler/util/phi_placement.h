#ifndef PIR_PHI_PLACEMENT
#define PIR_PHI_PLACEMENT

#include "../pir/pir.h"
#include "cfg.h"

namespace rir {
namespace pir {

class PhiPlacement {
  public:
    static BB* find(const CFG& cfg, BB* searchBlock,
                    const std::vector<BB*>& inputs);
};

} // namespace pir
} // namespace rir

#endif
