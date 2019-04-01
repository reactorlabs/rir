#ifndef PIR_REFERENCE_COUNT_H
#define PIR_REFERENCE_COUNT_H

#include "../pir/pir_impl.h"
#include "generic_static_analysis.h"

namespace rir {
namespace pir {

struct AUses {
    enum Kind {
        None = 0,
        Once = 1,
        Multiple = 2,
        Destructive = 3,
    };

    std::unordered_map<Instruction*, Kind> uses;
    AbstractResult mergeExit(const AUses& other) { return merge(other); }
    AbstractResult merge(const AUses& other) {
        AbstractResult res;
        for (auto u : other.uses) {
            if (!uses.count(u.first)) {
                uses.emplace(u);
                res.update();
            } else if (uses.at(u.first) < u.second) {
                // used in both branches, use count is the maximum
                uses.at(u.first) = u.second;
                res.update();
            }
        }
        return res;
    }
    void print(std::ostream&, bool) const {
        // TODO
    }
};

class StaticReferenceCount : public StaticAnalysis<AUses> {
  public:
    StaticReferenceCount(ClosureVersion* cls, LogStream& log)
        : StaticAnalysis("StaticReferenceCountAnalysis", cls, cls, log) {
        bool changed = true;
        while (changed) {
            changed = false;

            Visitor::run(code->entry, [&](Instruction* i) {
                // Recursively enumerate all actual values a phi might contain
                if (Phi::Cast(i)) {
                    i->eachArg([&](Value* v) {
                        if (auto a = Instruction::Cast(v)) {
                            if (Phi::Cast(a)) {
                                for (auto otherAlias : alias[a]) {
                                    if (otherAlias->needsReferenceCount() &&
                                        !alias[i].includes(otherAlias)) {
                                        changed = true;
                                        alias[i].insert(otherAlias);
                                    }
                                }
                            } else {
                                if (a->needsReferenceCount() &&
                                    !alias[i].includes(a)) {
                                    changed = true;
                                    alias[i].insert(a);
                                }
                            }
                        }
                    });
                }
            });
        }
    }

  protected:
    std::unordered_map<Instruction*, SmallSet<Instruction*>> alias;

    AbstractResult apply(AUses& state, Instruction* i) const override {
        AbstractResult res;
        if (Phi::Cast(i))
            return res;

        if (i->needsReferenceCount()) {
            // This value was used only once in a loop, we can thus
            // reset the count on redefinition.
            if (state.uses.count(i) && state.uses.at(i) == AUses::Once) {
                state.uses[i] = AUses::None;
                res.update();
            }
        }

        auto count = [&](Instruction* i) {
            auto& use = state.uses[i];
            switch (use) {
            case AUses::None:
                use = AUses::Once;
                res.update();
                break;
            case AUses::Once:
                use = AUses::Multiple;
                res.update();
                break;
            default: {}
            }
        };

        auto apply = [&](Value* v) {
            if (auto j = Instruction::Cast(v)) {
                if (!j->needsReferenceCount())
                    return;
                if (alias.count(j))
                    for (auto a : alias.at(j))
                        count(a);
                else
                    count(j);
            }
        };
        i->eachArg(apply);

        switch (i->tag) {
        // Loop sequence needs to stay constant for the whole loop duration.
        case Tag::ForSeqSize:
        // Those instructions -- at the rir level -- are allowed to override
        // inputs, even if the refcount is 1. Therefore if they are used
        // multiple times, we need to bump the refcount even further.
        case Tag::Subassign1_1D:
        case Tag::Subassign2_1D:
        case Tag::Subassign1_2D:
        case Tag::Subassign2_2D:
        case Tag::Inc:
            if (auto input = Instruction::Cast(i->arg(0).val())) {
                if (input->needsReferenceCount() && state.uses.count(input) &&
                    state.uses.at(input) == AUses::Multiple) {
                    state.uses[input] = AUses::Destructive;
                    res.update();
                }
            }
            break;

        default:
            break;
        };

        return res;
    }
};
} // namespace pir
} // namespace rir

#endif
