#include "visibility.h"
#include "R/Funtab.h"

namespace rir {
namespace pir {

AbstractResult VisibilityAnalysis::apply(LastVisibilityUpdate& vis,
                                         Instruction* i) const {
    AbstractResult res;
    auto changesVisibility = [&]() {
        if (vis.observable.size() == 1 && *vis.observable.begin() == i)
            return;
        vis.observable.clear();
        vis.observable.insert(i);
        res.update();
    };
    auto maybeChangesVisibility = [&]() {
        if (!vis.observable.count(i)) {
            vis.observable.insert(i);
            res.update();
        }
    };

    switch (i->tag) {
    case Tag::Invisible:
    case Tag::Visible:
        changesVisibility();
        break;
    case Tag::CallBuiltin:
    case Tag::CallSafeBuiltin:
        int builtinId;
        if (auto c = CallBuiltin::Cast(i)) {
            builtinId = c->builtinId;
        } else {
            builtinId = CallSafeBuiltin::Cast(i)->builtinId;
        }

        if (builtinUpdatesVisibility(builtinId))
            changesVisibility();
        break;

    default:
        // This instruction might change visibility, thus it's visibility effect
        // might be observable. But it does not clear previous visibility
        // instructions, because it might also preserve the previous visibility
        // setting.
        if (i->hasVisibility())
            maybeChangesVisibility();
        break;
    }
    return res;
};

} // namespace rir
} // namespace pir
