#include "visibility.h"
#include "R/Funtab.h"

namespace rir {
namespace pir {

AbstractResult VisibilityAnalysis::apply(CurrentVisibility& vis,
                                         Instruction* i) const {
    // Default: visible
    if (!code->entry->isEmpty() && i == *code->entry->begin())
        vis.state = CurrentVisibility::Visible;

    switch (i->tag) {
    case Tag::Invisible:
        if (vis.state != CurrentVisibility::Invisible) {
            vis.state = CurrentVisibility::Invisible;
            return AbstractResult::Updated;
        }
        break;
    case Tag::Visible:
    case Tag::LdVar:
    case Tag::LdVarSuper:
    case Tag::Extract1_1D:
    case Tag::Extract2_1D:
    case Tag::Extract1_2D:
    case Tag::Extract2_2D:
        if (vis.state != CurrentVisibility::Visible) {
            vis.state = CurrentVisibility::Visible;
            return AbstractResult::Updated;
        }
        break;
    case Tag::CallBuiltin:
    case Tag::CallSafeBuiltin:
        int flag;
        if (auto c = CallBuiltin::Cast(i)) {
            flag = getFlag(c->builtinId);
        } else {
            flag = getFlag(CallSafeBuiltin::Cast(i)->builtinId);
        }

        if (flag < 2) {
            bool visible = static_cast<Rboolean>(flag != 1);
            if (visible && vis.state != CurrentVisibility::Visible) {
                vis.state = CurrentVisibility::Visible;
                return AbstractResult::Updated;
            }
            if (!visible && vis.state != CurrentVisibility::Invisible) {
                vis.state = CurrentVisibility::Invisible;
                return AbstractResult::Updated;
            }
        }
        break;
    default:
        if (i->mightChangeVisibility()) {
            if (vis.state != CurrentVisibility::Unknown) {
                vis.state = CurrentVisibility::Unknown;
                return AbstractResult::Updated;
            }
        }
    }
    return AbstractResult::None;
};

} // namespace rir
} // namespace pir
