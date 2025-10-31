#include "value.h"

#include "compiler/util/visitor.h"
#include "instruction.h"
#include "pir_impl.h"

#include <unordered_map>

namespace rir {
namespace pir {

void Value::callArgTypeToContext(Context& assumptions, unsigned i) const {
    // this is for function arguments
    auto arg = this;
    assert(!arg->type.maybePromiseWrapped());

    if (auto mk = MkArg::Cast(arg)) {
        if (mk->isEager() || mk->noReflection)
            assumptions.setNonRefl(i);

        if (mk->isEager()) {
            assumptions.setEager(i);
            arg = mk->eagerArg();
        } else {
            return;
        }
    } else {
        assumptions.setNonRefl(i);
        assumptions.setEager(i);
    }

    if (arg == MissingArg::instance()) {
        assumptions.remove(Assumption::NoExplicitlyMissingArgs);
        return;
    }

    assert(arg != UnboundValue::instance());

    auto check = [&](const Value* arg) {
        if (!arg->type.maybeLazy())
            assumptions.setEager(i);
        if (!arg->type.maybeObj()) {
            assumptions.setNotObj(i);
            if (arg->type.isSimpleScalar()) {
                if (arg->type.isRType(RType::real))
                    assumptions.setSimpleReal(i);
                if (arg->type.isRType(RType::integer))
                    assumptions.setSimpleInt(i);
            }
        }
    };
    check(arg);
    arg = arg->cFollowCasts();
    if (!MkArg::Cast(arg))
        check(arg);
    arg = arg->cFollowCastsAndForce();
    if (!MkArg::Cast(arg))
        check(arg);
}

void Value::checkReplace(Value* replace) const {
    if (replace->type.isRType() != type.isRType() ||
        (replace->type.maybePromiseWrapped() && !type.maybePromiseWrapped())) {
        std::cerr << "Trying to replace a ";
        type.print(std::cerr);
        std::cerr << " with a ";
        replace->type.print(std::cerr);
        std::cerr << "\n";
        if (const auto i = Instruction::Cast(this))
            i->bb()->owner->printCode(std::cout, true, false);
        assert(false);
    }
}

// Find the transitive closure of all casts and forces
void collectMaybeAssumedOn(Value* val, SmallSet<Instruction*>& maybeAssumedOn) {
    auto i = Instruction::Cast(val);
    if (!i) {
        return;
    }

    if (maybeAssumedOn.count(i)) {
        return;
    }

    maybeAssumedOn.insert(i);

    if (!Instruction::Cast(val))
        return;

    // From `cFollowCastsAndForce`
    if (auto cast = PirCopy::Cast(val))
        return collectMaybeAssumedOn(cast->arg<0>().val(), maybeAssumedOn);
    if (auto cast = CastType::Cast(val))
        return collectMaybeAssumedOn(cast->arg<0>().val(), maybeAssumedOn);
    if (auto force = Force::Cast(val))
        return collectMaybeAssumedOn(force->input(), maybeAssumedOn);
    if (auto mkarg = MkArg::Cast(val))
        if (mkarg->isEager())
            return collectMaybeAssumedOn(mkarg->eagerArg(), maybeAssumedOn);
    if (auto chk = ChkFunction::Cast(val))
        return collectMaybeAssumedOn(chk->arg<0>().val(), maybeAssumedOn);
    if (auto chk = ChkMissing::Cast(val))
        return collectMaybeAssumedOn(chk->arg<0>().val(), maybeAssumedOn);
}

void checkSubsumed(IsType* tt) {
    if (!report::CollectStats::value) {
        return;
    }

    auto testedValue = tt->arg<0>().val();
    if (!Instruction::Cast(testedValue)) {
        return;
    }

    std::vector<Assume*> subsumedAssumes;

    // First check which assumptions is `tt` input to
    Visitor::run(tt->bb(), [&](BB* bb) {
        for (auto i : *bb) {
            if (auto assume = Assume::Cast(i)) {
                if (assume->condition() == tt) {
                    subsumedAssumes.push_back(assume);
                }
            }
        }
    });

    if (subsumedAssumes.size() == 0) {
        return;
    }

    std::vector<Assume*> subsumedBy;
    auto code = tt->bb()->owner;
    auto entry = code->entry;

    // Find if `testedValue` is assumed on; if yes change subsumed
    {
        SmallSet<Instruction*> maybeAssumedOn;
        collectMaybeAssumedOn(testedValue, maybeAssumedOn);

        Visitor::run(entry, [&](BB* bb) {
            for (auto i : *bb) {
                // There is an IsType on one of the values value
                if (auto tt = IsType::Cast(i)) {
                    auto ttArg = tt->arg<0>().val();
                    if (auto ttArgI = Instruction::Cast(ttArg)) {
                        if (maybeAssumedOn.includes(ttArgI)) {

                            // and the IsType (tt) is an input to some other
                            // assume
                            Visitor::run(bb, [&](BB* bb) {
                                for (auto i : *bb) {
                                    if (auto assume = Assume::Cast(i)) {
                                        if (assume->condition() == tt) {
                                            subsumedBy.push_back(assume);
                                        }
                                    }
                                }
                            });
                        }
                    }
                }
            }
        });
    }

    // Register the subsumed assumes
    for (const auto& assume : subsumedAssumes) {
        code->getClosureVersion()->registerSubsumedAssumption(assume,
                                                              subsumedBy);
    }
}

void Value::replaceUsesIn(
    Value* replace, BB* start,
    const std::function<void(Instruction*, size_t)>& postAction,
    const std::function<bool(Instruction*)>& replaceOnly) {
    checkReplace(replace);

    if (replace == True::instance()) {
        if (auto tt = IsType::Cast(this)) {
            checkSubsumed(tt);
        }
    }

    Visitor::run(start, [&](BB* bb) {
        for (auto& i : *bb) {
            std::vector<size_t> changed;
            size_t pos = 0;
            if (!replaceOnly(i))
                continue;
            i->eachArg([&](InstrArg& arg) {
                if (arg.val() == this) {
                    arg.val() = replace;
                    changed.push_back(pos);
                }
                pos++;
            });
            if (!changed.empty()) {
                for (auto c : changed)
                    postAction(i, c);
                i->updateTypeAndEffects();
            }
        }
    });
}
} // namespace pir
} // namespace rir
