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
     *                       |
     *                      Once
     *                       |
     *                 AlreadyIncremented
     *                       |
     *                      None
     *
     * Definitions:
     *   reuse            :=   override if refcount is == 0
     */

    enum Kind {
        // 1) Static refcount is safe
        None,
        AlreadyIncremented, // Refcount is known to be 1 (ie. used by stvar)
        Once,               // At least one use, which reuses input

        // 2) Need an ensureNamed
        Multiple, // At least two uses, at least one (but the last) is reusing
    };

    static Kind merge(Kind a, Kind b) {
        if (a == b)
            return a;

        return a > b ? a : b;
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

    void print(std::ostream& out, bool) const {
        for (const auto& use : uses) {
            use.first->print(out);
            out << " = ";
            switch (use.second) {
            case Kind::None:
                out << "0";
                break;
            case Kind::Once:
                out << "1";
                break;
            case Kind::AlreadyIncremented:
                out << "+1";
                break;
            case Kind::Multiple:
                out << "m";
                break;
            }
            out << "\n";
        }
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
                        v = v->followCasts();
                        while (auto cp = PirCopy::Cast(v->followCasts()))
                            v = cp->arg<0>().val();
                        v = v->followCasts();
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

        if (state.overflow || Phi::Cast(i) || PirCopy::Cast(i))
            return res;

        auto count = [&](Value* v, bool constantUse, bool increments) {
            auto vi = Instruction::Cast(v);
            if (!vi)
                return;

            auto use = state.uses.find(vi);
            if (use == state.uses.end()) {
                if (!constantUse && vi != i) {
                    // duplicate case AUses::None from below. This avoids
                    // creating an entry if not needed
                    state.uses[vi] =
                        increments ? AUses::AlreadyIncremented : AUses::Once;
                    res.update();
                }
                return;
            }

            switch (use->second) {
            case AUses::None:
                // multiple non-reusing uses are ok, as long as the are not
                // preceeded by a reusing use (in which case we are at Once)
                if (!constantUse && vi != i) {
                    use->second =
                        increments ? AUses::AlreadyIncremented : AUses::Once;
                    res.update();
                }
                break;
            case AUses::Once:
                // self input = overrides itself...
                if (vi == i)
                    use->second = AUses::None;
                else
                    use->second = AUses::Multiple;
                res.update();
                break;
            case AUses::AlreadyIncremented:
            case AUses::Multiple:
                break;
            }
        };

        std::function<void(Value*, bool, bool)> apply =
            [&](Value* v, bool constantUse, bool increments) {
                v = v->followCasts();
                while (auto cp = PirCopy::Cast(v->followCasts()))
                    v = cp->arg<0>().val();
                v = v->followCasts();

                if (auto j = Instruction::Cast(v)) {
                    if (j->minReferenceCount() >= 1)
                        return;

                    if (alias.count(j))
                        for (auto a : alias.at(j))
                            count(a, constantUse, increments);
                    else
                        count(j, constantUse, increments);
                }
            };

        switch (i->tag) {
        // (1) Instructions which never reuse SEXPS
        //
        case Tag::RecordDeoptReason:
        case Tag::Return:
        case Tag::Length:
        case Tag::Colon:
        case Tag::CastType:
        case Tag::IsType:
        case Tag::IsObject:
        case Tag::IsEnvStub:
        case Tag::ChkMissing:
        case Tag::Deopt:
        case Tag::AsTest:
        case Tag::Identical:
        case Tag::Is:
        case Tag::LOr:
        case Tag::LAnd:
        case Tag::MkArg:
        case Tag::MkCls:
        case Tag::MkFunCls:
            i->eachArg([&](Value* v) { apply(v, true, false); });
            break;

        // (2) Instructions which update the named count
        //
        case Tag::StVar:
        case Tag::StVarSuper:
        case Tag::MkEnv:
            i->eachArg([&](Value* v) {
                if (v == i->env())
                    return;
                apply(v, true, true);
            });
            break;

        // (3) Default: instructions which might update in-place, if named
        // count is 0
        //
        default:
            i->eachArg([&](Value* v) { apply(v, false, false); });
            break;
        };

        if (i->minReferenceCount() < 1) {
            // This value was used only once in a loop, we can thus
            // reset the count on redefinition.
            auto u = state.uses.find(i);
            if (u != state.uses.end() && u->second <= AUses::Once) {
                u->second = AUses::None;
                res.update();
            }
        }

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
