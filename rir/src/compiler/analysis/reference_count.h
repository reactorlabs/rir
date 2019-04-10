#ifndef PIR_REFERENCE_COUNT_H
#define PIR_REFERENCE_COUNT_H

#include "../pir/pir_impl.h"
#include "generic_static_analysis.h"

namespace rir {
namespace pir {

struct AUses {

    /*
     * Lattice:
     *
     *                    Multiple
     *                     /      \
     *   AlreadyIncremented        Once
     *                     \      /
     *                       None
     *
     * Definitions:
     *   reuse            :=   override if refcount is == 0
     */

    enum Kind {
        // 1) Static refcount is safe
        None,
        Once,               // At least one use, which reuses input
        AlreadyIncremented, // Refcount is known to be 1 (ie. used by stvar)

        // 2) Need an ensureNamed
        Multiple, // At least two uses, at least one (but the last) is reusing
    };

    static Kind merge(Kind a, Kind b) {
        if (a == b)
            return a;

        Kind bigger = a > b ? a : b;
        Kind smaller = a < b ? a : b;
        if (smaller == None)
            return bigger;

        if (bigger == Multiple || bigger == AlreadyIncremented)
            return Multiple;

        assert(false);
        return Multiple;
    }

    bool overflow = false;
    // The merge function needs an ordered map
    std::map<Instruction*, Kind> uses;
    AbstractResult mergeExit(const AUses& other) { return merge(other); }
    AbstractResult merge(const AUses& other) {
        AbstractResult res;

        if (overflow)
            return res;

        if (other.overflow) {
            uses.clear();
            overflow = true;
            res.taint();
            return res;
        }

        auto myPos = uses.begin();
        auto theirPos = other.uses.begin();

        while (theirPos != other.uses.end()) {
            if (myPos == uses.end() || myPos->first > theirPos->first) {
                // we are missing this entry
                auto updated = uses.insert(*theirPos);
                myPos = updated.first;
                res.update();
                myPos++;
                theirPos++;
            } else if (myPos->first == theirPos->first) {
                if (myPos->second != theirPos->second) {
                    auto m = merge(myPos->second, theirPos->second);
                    if (m != myPos->second) {
                        myPos->second = m;
                        res.update();
                    }
                }
                myPos++;
                theirPos++;
            } else {
                assert(myPos->first < theirPos->first);
                // other branch does not have this entry
                myPos++;
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
                        if (auto c = PirCopy::Cast(v))
                            v = c->arg<0>().val();
                        if (auto a = Instruction::Cast(v)) {
                            if (Phi::Cast(a)) {
                                for (auto otherAlias : alias[a]) {
                                    if (!alias[i].includes(otherAlias)) {
                                        changed = true;
                                        alias[i].insert(otherAlias);
                                    }
                                }
                            } else {
                                if (a->minReferenceCount() < 1 &&
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

        if (state.overflow || Phi::Cast(i) || PirCopy::Cast(i) ||
            CastType::Cast(i))
            return res;

        if (i->minReferenceCount() < 1) {
            // This value was used only once in a loop, we can thus
            // reset the count on redefinition.
            auto u = state.uses.find(i);
            if (u != state.uses.end() && u->second == AUses::Once) {
                u->second = AUses::None;
                res.update();
            }
        }

        auto count = [&](Instruction* i, bool constantUse) {
            auto use = state.uses.find(i);
            if (use == state.uses.end()) {
                if (!constantUse) {
                    // duplicate case AUses::None from below. This avoids
                    // creating an entry if not needed
                    state.uses[i] = AUses::Once;
                    res.update();
                }
                return;
            }
            switch (use->second) {
            case AUses::None:
                // multiple non-reusing uses are ok, as long as the are not
                // preceeded by a reusing use (in which case we are at Once)
                if (!constantUse) {
                    use->second = AUses::Once;
                    res.update();
                }
                break;
            case AUses::Once:
                use->second = AUses::Multiple;
                res.update();
                break;
            case AUses::AlreadyIncremented:
            case AUses::Multiple:
                break;
            }
        };

        std::function<void(Value*, bool)> apply = [&](Value* v,
                                                      bool constantUse) {
            if (auto j = Instruction::Cast(v)) {
                if (j->minReferenceCount() >= 1)
                    return;

                if (auto cp = PirCopy::Cast(v))
                    return apply(cp->arg<0>().val(), constantUse);
                if (auto cp = CastType::Cast(v))
                    return apply(cp->arg<0>().val(), constantUse);

                if (alias.count(j))
                    for (auto a : alias.at(j))
                        count(a, constantUse);
                else
                    count(j, constantUse);
            }
        };

        switch (i->tag) {
        // (1) Instructions which never reuse SEXPS
        //
        case Tag::Return:
        case Tag::Length:
        case Tag::Seq:
        case Tag::Colon:
        case Tag::IsObject:
        case Tag::IsEnvStub:
        case Tag::Deopt:
        case Tag::AsTest:
        case Tag::Identical:
        case Tag::Is:
        case Tag::LOr:
        case Tag::LAnd:
        case Tag::MkArg:
        case Tag::MkCls:
        case Tag::MkFunCls:
            i->eachArg([&](Value* v) { apply(v, true); });
            break;

        // (2) Instructions which update the named count
        //
        case Tag::StVar:
        case Tag::StVarSuper:
        case Tag::MkEnv:
            i->eachArg([&](Value* v) {
                if (v == i->env())
                    return;
                if (auto val = Instruction::Cast(v)) {
                    apply(val, false);
                    if (!state.uses.count(val) ||
                        state.uses.at(val) <= AUses::Once) {
                        state.uses[val] = AUses::AlreadyIncremented;
                        res.update();
                    }
                }
            });
            break;

        // (3) Default: instructions which might update in-place, if named
        // count is 0
        //
        default:
            i->eachArg([&](Value* v) { apply(v, false); });
            break;
        };

        // The abstract state is expensive to merge. To lift this limit, we'd
        // need to find a better strategy, or a less expensive analysis...
        if (state.uses.size() > 1000) {
            state.uses.clear();
            state.overflow = true;
            res.taint();
        }

        return res;
    }
};
}
}

#endif
