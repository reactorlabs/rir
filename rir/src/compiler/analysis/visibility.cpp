#include "visibility.h"
#include "R/Funtab.h"

namespace rir {
namespace pir {

AbstractResult VisibilityAnalysis::apply(LastVisibilityUpdate& vis,
                                         Instruction* i) const {
    AbstractResult res;
    if (i->effects.contains(Effect::Visibility)) {
        switch (i->visibilityFlag()) {
        case VisibilityFlag::On:
        case VisibilityFlag::Off:
            // Always changes visibility, overrides previous changes
            if (vis.observable.size() != 1 || *vis.observable.begin() != i) {
                vis.observable.clear();
                vis.observable.insert(i);
                res.update();
            }
            break;
        case VisibilityFlag::Unknown:
            // Maybe changes visibility, need to keep previous if it doesn't
            if (!vis.observable.count(i)) {
                vis.observable.insert(i);
                res.update();
            }
            break;
        default:
            assert(false);
        }
    }
    return res;
};

} // namespace rir
} // namespace pir
