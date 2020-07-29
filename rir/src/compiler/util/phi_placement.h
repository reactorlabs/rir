#ifndef PIR_PHI_PLACEMENT
#define PIR_PHI_PLACEMENT

#include "../pir/pir.h"
#include "compiler/analysis/cfg.h"
#include "utils/Set.h"

#include <unordered_map>

namespace rir {
namespace pir {

class Phi;
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

    PhiPlacement(Code* code, const std::unordered_map<BB*, Value*>& inputs,
                 const DominanceGraph& dom, const DominanceFrontier&);

    typedef std::unordered_map<BB*, SmallSet<PhiInput>> Phis;

    Phis placement;
    std::unordered_map<BB*, BB*> dominatingPhi;
};

} // namespace pir
} // namespace rir

#endif
