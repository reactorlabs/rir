#include "visibility.h"
#include "R/Funtab.h"

namespace rir {
namespace pir {

AbstractResult VisibilityAnalysis::apply(LastVisibilityUpdate& vis,
                                         Instruction* i) const {
    AbstractResult res;
    if (i->alwaysOverridesVisibility()) {
        // Always changes visibility
        if (vis.observable.size() == 1 && *vis.observable.begin() == i)
            return;
        vis.observable.clear();
        vis.observable.insert(i);
        res.update();
    } else if (i->effects.contains(Effect::Visibility)) {
        // Maybe changes visibility
        if (!vis.observable.count(i)) {
            vis.observable.insert(i);
            res.update();
        }
    }
    return res;
};

} // namespace rir
} // namespace pir
