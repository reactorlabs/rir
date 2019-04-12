#ifndef PIR_PHI_PLACEMENT
#define PIR_PHI_PLACEMENT

#include "../pir/pir.h"
#include "cfg.h"
#include "utils/Set.h"

#include <unordered_map>

namespace rir {
namespace pir {

class PhiPlacement {
  public:
    struct PhiInput {
        BB* inputBlock;
        BB* otherPhi;
        Value* aValue;
        bool operator==(const PhiInput& other) const {
            return inputBlock == other.inputBlock &&
                   otherPhi == other.otherPhi && aValue == other.aValue;
        }
    };
    typedef std::unordered_map<BB*, SmallSet<PhiInput>> Phis;
    bool success = false;
    BB* targetPhiPosition = nullptr;
    Phis placement;

    PhiPlacement(ClosureVersion* cls, BB* target,
                 const std::unordered_map<BB*, Value*>& inputs, const CFG& cfg,
                 const DominanceGraph& dom, const DominanceFrontier&);
};

} // namespace pir
} // namespace rir

#endif
