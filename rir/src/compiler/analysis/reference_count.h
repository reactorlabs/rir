#ifndef PIR_REFERENCE_COUNT_H
#define PIR_REFERENCE_COUNT_H

#include "../pir/pir_impl.h"
#include "generic_static_analysis.h"

namespace rir {
namespace pir {

struct AUses {
    enum Kind {
        None,
        Once,
        AlreadyIncremented,
        Multiple,
        Destructive,
        MultipleWithDestructive,
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
                                    if (otherAlias->minReferenceCount() <
                                            Value::MAX_REFCOUNT &&
                                        !alias[i].includes(otherAlias)) {
                                        changed = true;
                                        alias[i].insert(otherAlias);
                                    }
                                }
                            } else {
                                if (a->minReferenceCount() <
                                        Value::MAX_REFCOUNT &&
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

        if (i->minReferenceCount() < Value::MAX_REFCOUNT) {
            // This value was used only once in a loop, we can thus
            // reset the count on redefinition.
            if (state.uses.count(i) &&
                state.uses.at(i) <= AUses::AlreadyIncremented) {
                state.uses[i] = AUses::None;
                res.update();
            }
        }

        auto count = [&](Instruction* i, bool constantUse) {
            auto& use = state.uses[i];
            switch (use) {
            case AUses::None:
                use = AUses::Once;
                res.update();
                break;
            case AUses::Once:
                if (!constantUse) {
                    use = AUses::Multiple;
                    res.update();
                }
                break;
            case AUses::Destructive:
                use = AUses::MultipleWithDestructive;
                res.update();
                break;
            case AUses::AlreadyIncremented:
            case AUses::Multiple:
            case AUses::MultipleWithDestructive:
                break;
            }
        };

        auto apply = [&](Value* v, bool constantUse) {
            if (auto j = Instruction::Cast(v)) {
                if (j->minReferenceCount() == Value::MAX_REFCOUNT)
                    return;
                if (alias.count(j))
                    for (auto a : alias.at(j))
                        count(a, constantUse);
                else
                    count(j, constantUse);
            }
        };

        auto applyDestructive = [&](Instruction* i, size_t index,
                                    bool constantUse) {
            if (auto input = Instruction::Cast(i->arg(index).val())) {
                if (input->minReferenceCount() < Value::MAX_REFCOUNT) {
                    if (state.uses.count(input) &&
                        state.uses.at(input) < AUses::Destructive) {
                        state.uses[input] = AUses::Destructive;
                        res.update();
                    }
                }
            }
            i->eachArg([&](Value* v) {
                if (v != i->arg(index).val())
                    apply(v, constantUse);
            });
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
        case Tag::FrameState:
        case Tag::MkEnv:
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
            if (auto val = Instruction::Cast(i->arg(0).val())) {
                apply(val, false);
                if (state.uses.count(val) &&
                    state.uses.at(val) == AUses::Once) {
                    state.uses.at(val) = AUses::AlreadyIncremented;
                    res.update();
                }
            }
            break;

        // (3) Loop sequence needs to stay constant for the whole loop duration.
        //
        case Tag::ForSeqSize:
            applyDestructive(i, 0, false);
            break;

        // (4) Instructions which update in-place, even if named count is 1
        //
        // Those instructions -- at the rir level -- are allowed to override
        // inputs, even if the refcount is 1. Therefore if they are used
        // multiple times, we need to bump the refcount even further.
        case Tag::Subassign1_1D:
        case Tag::Subassign2_1D:
        case Tag::Subassign1_2D:
        case Tag::Subassign2_2D:
            applyDestructive(i, 1, false);
            break;

        // (5) Default: instructions which might update in-place, if named
        // count is 0
        //
        default:
            i->eachArg([&](Value* v) { apply(v, false); });
            break;
        };

        return res;
    }
};
} // namespace pir
} // namespace rir

#endif
