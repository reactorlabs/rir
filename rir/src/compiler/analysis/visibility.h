#ifndef PIR_VISIBILITY_H
#define PIR_VISIBILITY_H

#include "abstract_value.h"
#include "generic_static_analysis.h"

namespace rir {
namespace pir {

class CurrentVisibility {
  public:
    enum State { Visible, Invisible, Unknown };

    State state = Unknown;

    CurrentVisibility() {}

    AbstractResult merge(const CurrentVisibility& other) {
        if (state != Unknown && state != other.state) {
            state = Unknown;
            return AbstractResult::Updated;
        }
        return AbstractResult::None;
    }

    void print(std::ostream& out, bool tty) const {
        switch (state) {
        case Unknown:
            out << "?";
            break;
        case Visible:
            out << "visible";
            break;
        case Invisible:
            out << "invisible";
            break;
        }
        out << "\n";
    };
};

class VisibilityAnalysis : public StaticAnalysis<CurrentVisibility> {
  public:
    VisibilityAnalysis(ClosureVersion* cls, LogStream& log)
        : StaticAnalysis("VisibilityAnalysis", cls, cls, log) {}

    AbstractResult apply(CurrentVisibility& vis,
                         Instruction* i) const override {
        if (Invisible::Cast(i)) {
            if (vis.state != CurrentVisibility::Invisible) {
                vis.state = CurrentVisibility::Invisible;
                return AbstractResult::Updated;
            }
        } else if (Visible::Cast(i)) {
            if (vis.state != CurrentVisibility::Visible) {
                vis.state = CurrentVisibility::Visible;
                return AbstractResult::Updated;
            }
        } else if (i->mightChangeVisibility()) {
            if (vis.state != CurrentVisibility::Unknown) {
                vis.state = CurrentVisibility::Unknown;
                return AbstractResult::Updated;
            }
        }
        return AbstractResult::None;
    };

    CurrentVisibility::State at(Instruction* i) {
        return StaticAnalysis::at<PositioningStyle::BeforeInstruction>(i).state;
    }
};

} // namespace pir
} // namespace rir

#endif
