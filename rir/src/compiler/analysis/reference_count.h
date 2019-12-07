#ifndef PIR_REFERENCE_COUNT_H
#define PIR_REFERENCE_COUNT_H

#include "../pir/pir_impl.h"
#include "dead.h"
#include "generic_static_analysis.h"
#include "utils/Map.h"

namespace rir {
namespace pir {

struct NeedsRefcountAdjustment {
    enum Kind { EnsureNamed, SetShared };

    // Not needed since we do not use the state in the analysis
    bool changed() { return false; }
    void resetChanged() {}

    SmallMap<Instruction*, Kind> atCreation;
    SmallMap<Instruction*, SmallMap<Instruction*, Kind>> beforeUse;

    void print(std::ostream& out, bool) const {
        out << "Adjust at creation: ";
        for (auto r : atCreation) {
            r.first->printRef(out);
            out << " " << r.second << ",  ";
        }
        out << "\n";

        out << "Adjust before use: ";
        for (auto i : beforeUse) {
            i.first->printRef(out);
            out << ": ";
            for (auto r : i.second) {
                r.first->printRef(out);
                out << " " << r.second << ",  ";
            }
            out << "\n";
        }
        out << "\n";
    }
};

struct AbstractValueTaint {
    // The merge function needs an ordered map
    AbstractResult mergeExit(const AbstractValueTaint& other) {
        return merge(other);
    }
    AbstractResult merge(const AbstractValueTaint& other) {
        AbstractResult res;

        auto myPos = tainted.begin();
        auto theirPos = other.tainted.begin();
        auto theirEnd = other.tainted.end();

        while (theirPos != theirEnd) {
            if (myPos == tainted.end() || myPos->first > theirPos->first) {
                // missing entry
                auto upd = tainted.insert(*theirPos);
                myPos = upd.first;
                res.update();
                myPos++;
                theirPos++;
            } else if (myPos->first == theirPos->first) {
                // merge
                auto& t1 = myPos->second;
                auto& t2 = theirPos->second;

                if (t1.kind < t2.kind) {
                    t1.kind = t2.kind;
                    res.update();
                }
                if (t1.origin && t1.origin != t2.origin) {
                    t1.origin = nullptr;
                    res.update();
                }

                myPos++;
                theirPos++;
            } else {
                // we have an entry more
                assert(myPos->first < theirPos->first);
                myPos++;
            }
        }
        return res;
    }

    void print(std::ostream& out, bool) const {
        out << "Taints: ";
        for (auto r : tainted) {
            auto& taint = r.second;
            if (taint.kind == Taint::None)
                continue;
            out << taint.kind << " ";
            r.first->printRef(out);
            if (taint.origin) {
                out << " by ";
                taint.origin->printRef(out);
            }
            out << ",  ";
        }
        out << "\n";
    }

    struct Taint {
        enum Kind {
            None,
            Reuse,
            Override,
        };
        friend std::ostream& operator<<(std::ostream& out, Kind k) {
            switch (k) {
            case None:
                out << "-";
                break;
            case Reuse:
                out << "reused";
                break;
            case Override:
                out << "override";
                break;
            }
            return out;
        }
        Kind kind = Kind::None;
        Instruction* origin = nullptr;
    };

    Taint* isTainted(Instruction* i) {
        auto t = tainted.find(i);
        if (t == tainted.end())
            return nullptr;
        auto& taint = t->second;
        if (taint.kind == Taint::None)
            return nullptr;
        return &taint;
    }

    Taint* taint(Instruction* i) {
        assert(!tainted.count(i) ||
               tainted.at(i).kind == AbstractValueTaint::Taint::None);
        return &tainted[i];
    }

    void taint(Instruction* i, Taint::Kind k, Instruction* origin) {
        auto t = taint(i);
        t->kind = k;
        t->origin = origin;
    }

    bool clear(Instruction* i) {
        auto t = tainted.find(i);
        if (t == tainted.end())
            return false;
        if (t->second.kind == Taint::Kind::None) {
            assert(!t->second.origin);
            return false;
        }
        t->second.kind = Taint::Kind::None;
        t->second.origin = nullptr;
        return true;
    }

  private:
    std::map<Instruction*, Taint> tainted;
};

class StaticReferenceCount
    : public StaticAnalysis<AbstractValueTaint, NeedsRefcountAdjustment, true,
                            AnalysisDebugLevel::None> {
  private:
    DominanceGraph dom;

  public:
    StaticReferenceCount(ClosureVersion* cls, LogStream& log)
        : StaticAnalysis("StaticReferenceCountAnalysis", cls, cls, log),
          dom(cls) {
        globalState = new NeedsRefcountAdjustment;
    }

    ~StaticReferenceCount() { delete globalState; }

  protected:
    AbstractResult apply(AbstractValueTaint& state,
                         Instruction* i) const override {
        AbstractResult res;

        // Check if this instruction uses a tainted value. If so, we need to
        // record the fact that there is a adjustment needed.
        // We collect the result in the global state.
        i->eachArg([&](Value* v) {
            if (auto j = Instruction::Cast(v->followCasts())) {
                assert(!PirCopy::Cast(j));
                if (i == j || j->minReferenceCount() > 1)
                    return;
                if (auto taint = state.isTainted(j)) {
                    NeedsRefcountAdjustment::Kind k =
                        NeedsRefcountAdjustment::EnsureNamed;
                    if (taint->kind == AbstractValueTaint::Taint::Override)
                        k = NeedsRefcountAdjustment::SetShared;
                    else
                        assert(taint->kind == AbstractValueTaint::Taint::Reuse);

                    if (taint->origin) {
                        if (globalState->beforeUse[taint->origin][j] < k) {
                            globalState->beforeUse[taint->origin][j] = k;
                        }
                    } else {
                        auto exists = globalState->atCreation.find(j);
                        if (exists != globalState->atCreation.end()) {
                            if (exists->second < k)
                                exists->second = k;
                        } else {
                            globalState->atCreation[j] = k;
                        }
                    }
                }
            }
        });

        switch (i->tag) {

        // A phi gets tainted if any of it's inputs are
        case Tag::Phi: {
            auto p = Phi::Cast(i);
            auto phiTaint = state.isTainted(p);
            if (phiTaint) {
                phiTaint->kind = AbstractValueTaint::Taint::None;
                phiTaint->origin = nullptr;
                res.update();
            }
            p->eachArg([&](BB*, Value* v) {
                if (auto j = Instruction::Cast(v->followCasts())) {
                    auto inputTaint = state.isTainted(j);
                    if (!phiTaint && inputTaint) {
                        phiTaint = state.taint(p);
                        phiTaint->kind = inputTaint->kind;
                        res.update();
                    } else if (phiTaint && inputTaint) {
                        if (phiTaint->kind < inputTaint->kind) {
                            phiTaint->kind = inputTaint->kind;
                            res.update();
                        }
                        // Origin of the taint is ambiguous. We'll need to fix
                        // it here.
                        if (phiTaint->origin != inputTaint->origin) {
                            phiTaint->origin = nullptr;
                            res.update();
                        }
                    }
                }
            });
            break;
        }

        // Instructions which never reuse SEXPS can be ignored
        case Tag::PirCopy:
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
        case Tag::Eq:
        case Tag::Neq:
        case Tag::Lt:
        case Tag::Lte:
        case Tag::Gt:
        case Tag::Gte:
        case Tag::Not:
        case Tag::Extract1_1D:
        case Tag::Extract1_2D:
        case Tag::Extract1_3D:
        case Tag::StVar:
        case Tag::StVarSuper:
        case Tag::MkEnv:
        case Tag::MkArg:
        case Tag::UpdatePromise:
        case Tag::ForSeqSize:
        case Tag::ScheduledDeopt:
        case Tag::PopContext:
        case Tag::Extract2_2D:
            break;

        // Those may override the vector (which is arg 1)
        case Tag::Subassign1_1D:
        case Tag::Subassign1_2D:
        case Tag::Subassign1_3D:
        case Tag::Subassign2_1D:
        case Tag::Subassign2_2D:
            if (auto j = Instruction::Cast(i->arg(1).val()->followCasts())) {
                if (j->minReferenceCount() < 2) {
                    auto taint = state.isTainted(j);
                    if (taint &&
                        taint->kind < AbstractValueTaint::Taint::Override) {
                        taint->kind = AbstractValueTaint::Taint::Override;
                        res.update();
                    } else if (!taint) {
                        state.taint(j, AbstractValueTaint::Taint::Override, i);
                        res.update();
                    }
                }
            }
            break;

        // Default: instructions which might update in-place, if named
        // count is 0
        case Tag::Extract2_1D:
        default:
            i->eachArg([&](Value* v) {
                if (auto j = Instruction::Cast(v->followCasts())) {
                    if (j->minReferenceCount() < 1) {
                        auto taint = state.isTainted(j);
                        assert(!taint ||
                               taint->kind >= AbstractValueTaint::Taint::Reuse);
                        if (!taint) {
                            state.taint(j, AbstractValueTaint::Taint::Reuse, i);
                            res.update();
                        }
                    }
                }
            });
            break;
        };

        // Executing an instruction un-taints its result value.
        if (!Phi::Cast(i)) {
            if (state.clear(i))
                res.update();
        }

        return res;
    }
};
}
}

#endif
