#ifndef PIR_REFERENCE_COUNT_H
#define PIR_REFERENCE_COUNT_H

#include "../pir/pir_impl.h"
#include "dead.h"
#include "generic_static_analysis.h"

namespace rir {
namespace pir {

struct AUses {
    // Linear lattice:
    enum Kind {
        Named,        // Named count is >= 1
        Fresh,        // Named count is >= 0
        Tainted,      // Maybe destructively used
        UseAfterTaint // Use after maybe destructively use
    };

    static Kind merge(Kind a, Kind b) {
        return a > b ? a : b;
    }

    bool overflow = false;
    // The merge function needs an ordered map
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
            case Kind::Fresh:
                out << "+";
                break;
            case Kind::Named:
                out << "1";
                break;
            case Kind::Tainted:
                out << "!";
                break;
            case Kind::UseAfterTaint:
                out << "+!";
                break;
            }
            out << "\n";
        }
    }
    bool needsEnsureNamed(Instruction* i) const {
        auto u = uses.find(i);
        if (u == uses.end())
            return false;
        return u->second == UseAfterTaint;
    }
    void needsEnsureNamed(std::unordered_set<Instruction*>& collect) const {
        for (auto& i : uses)
            if (needsEnsureNamed(i.first))
                collect.insert(i.first);
    }

  private:
    friend class StaticReferenceCount;
    std::map<Instruction*, Kind> uses;
};

class StaticReferenceCount : public StaticAnalysis<AUses> {
  private:
    std::unordered_map<Instruction*, SmallSet<Instruction*>> alias;

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
    AbstractResult apply(AUses& state, Instruction* i) const override {
        AbstractResult res;

        if (state.overflow || Phi::Cast(i) || PirCopy::Cast(i))
            return res;

        auto count = [&](Value* v, bool constantUse, bool increments) {
            auto vi = Instruction::Cast(v);
            if (!vi)
                return;

            // self input is ignored
            if (vi == i)
                return;

            auto use = state.uses.find(vi);
            if (use == state.uses.end()) {
                if (constantUse)
                    return;
                use = state.uses.emplace(vi, AUses::Fresh).first;
            }

            switch (use->second) {
            case AUses::Fresh:
                // multiple non-reusing uses are ok, as long as the are not
                // preceeded by a reusing use (in which case we are at Once)
                if (!constantUse) {
                    use->second = increments ? AUses::Named : AUses::Tainted;
                    res.update();
                }
                break;
            case AUses::Tainted:
                use->second = AUses::UseAfterTaint;
                res.update();
                break;
            case AUses::Named:
            case AUses::UseAfterTaint:
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

                    auto as = alias.find(j);
                    if (as != alias.end()) {
                        for (auto a : as->second)
                            count(a, constantUse, increments);
                    }
                    count(j, constantUse, increments);
                }
            };

        switch (i->tag) {
        // Instructions which never reuse SEXPS
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
        case Tag::AsLogical:
        case Tag::Identical:
        case Tag::Is:
        case Tag::LOr:
        case Tag::LAnd:
        case Tag::MkCls:
        case Tag::MkFunCls:
            i->eachArg([&](Value* v) { apply(v, true, false); });
            break;

        // Instructions which if not overwritten don't reuse SEXPS
        case Tag::Eq:
        case Tag::Neq:
        case Tag::Lt:
        case Tag::Lte:
        case Tag::Gt:
        case Tag::Gte:
        case Tag::Extract1_1D:
        case Tag::Extract1_2D:
        case Tag::Extract1_3D:
        case Tag::Extract2_1D:
        case Tag::Extract2_2D:
        case Tag::Subassign1_1D:
        case Tag::Subassign1_2D:
        case Tag::Subassign1_3D:
        case Tag::Subassign2_1D:
        case Tag::Subassign2_2D:
            i->eachArg([&](Value* v) {
                apply(v, !i->effects.includes(Effect::ExecuteCode), false);
            });
            break;

        // Instructions which update the named count
        case Tag::StVar:
        case Tag::StVarSuper:
        case Tag::MkEnv:
        case Tag::MkArg:
        case Tag::UpdatePromise:
        case Tag::ForSeqSize:
            i->eachArg([&](Value* v) {
                if (i->hasEnv() && v == i->env())
                    return;
                apply(v, true, true);
            });
            break;

        // Default: instructions which might update in-place, if named
        // count is 0
        default:
            i->eachArg([&](Value* v) { apply(v, false, false); });
            break;
        };

        auto cur = state.uses.find(i);
        if (cur != state.uses.end() && cur->second < AUses::UseAfterTaint) {
            if (cur->second >= AUses::Fresh) {
                cur->second = AUses::Fresh;
                res.update();
            }
            // Variables from the environment must be named
            if (cur->second == AUses::Fresh) {
                switch (i->tag) {
                case Tag::LdVar:
                case Tag::LdVarSuper:
                    cur->second = AUses::Named;
                    break;
                default: {}
                }
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
