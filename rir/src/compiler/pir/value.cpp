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

void Value::replaceUsesIn(
    Value* replace, BB* start,
    const std::function<void(Instruction*, size_t)>& postAction,
    const std::function<bool(Instruction*)>& replaceOnly) {
    checkReplace(replace);
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
}
}
