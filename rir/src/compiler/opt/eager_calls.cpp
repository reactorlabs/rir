#include "../pir/pir_impl.h"
#include "../transform/bb.h"
#include "../transform/replace.h"
#include "../translations/rir_2_pir/rir_2_pir_compiler.h"
#include "../util/cfg.h"
#include "../util/safe_builtins_list.h"
#include "../util/visitor.h"
#include "R/r.h"
#include "pass_definitions.h"

#include <unordered_map>

namespace rir {
namespace pir {

void EagerCalls::apply(RirCompiler& cmp, ClosureVersion* closure,
                       LogStream&) const {
    std::unordered_set<MkArg*> todo;
    auto code = closure->entry;

    Visitor::run(code, [&](BB* bb) {
        for (auto ip = bb->begin(); ip != bb->end(); ++ip) {
            auto call = StaticCall::Cast(*ip);
            if (!call)
                continue;

            Closure* cls = call->cls();
            ClosureVersion* version = call->tryDispatch();

            if (!version ||
                call->nCallArgs() != cls->nargs())
                continue;

            bool allEager =
                version->properties.includes(ClosureVersion::Property::IsEager);
            if (!allEager && version->properties.argumentForceOrder.empty())
                continue;

            auto isEager = [&](size_t i) {
                if (allEager)
                    return true;
                for (auto a : version->properties.argumentForceOrder) {
                    // We know that a is forced before i, therefore we are not
                    // in left-to-right order
                    // TODO: support reordering of the evaluation
                    if (a > i)
                        return false;
                    // We found the argument in the list of certainly forced
                    // promises
                    if (a == i)
                        return true;
                }
                return false;
            };

            std::unordered_set<MkArg*> args;
            bool noMissing = true;
            size_t i = 0;
            call->eachCallArg([&](Value* v) {
                if (auto mk = MkArg::Cast(v)) {
                    if (!mk->isEager() && isEager(i))
                        args.insert(mk);
                } else {
                    noMissing = false;
                }
                i++;
            });
            if (!noMissing)
                continue;
            todo.insert(args.begin(), args.end());

            Assumptions newAssumptions = call->inferAvailableAssumptions();
            for (size_t i = 0; i < call->nCallArgs(); ++i) {
                if (!newAssumptions.isEager(i) && isEager(i))
                    newAssumptions.setEager(i);
            }
            // This might fire back, since we don't know if we really have no
            // objects... We should have some profiling. It's still sound, since
            // static_call_ will check the assumptions
            for (size_t i = 0; i < call->nCallArgs(); ++i)
                if (!newAssumptions.notObj(i) && newAssumptions.isEager(i))
                    newAssumptions.setNotObj(i);

            auto newVersion = cls->cloneWithAssumptions(
                version, newAssumptions, [&](ClosureVersion* newCls) {
                    Visitor::run(newCls->entry, [&](Instruction* i) {
                        if (auto ld = LdArg::Cast(i)) {
                            if (isEager(ld->id)) {
                                ld->type = PirType::promiseWrappedVal()
                                               .notObject()
                                               .notMissing();
                            }
                        }
                    });
                });
            call->hint = newVersion;
        }
    });
    if (todo.empty())
        return;

    Visitor::run(code, [&](BB* bb) {
        for (auto ip = bb->begin(); ip != bb->end(); ++ip) {
            if (auto mk = MkArg::Cast(*ip)) {
                if (!todo.count(mk))
                    continue;

                BB* split =
                    BBTransform::split(closure->nextBBId++, bb, ip, closure);

                auto prom = mk->prom();
                BB* prom_copy =
                    BBTransform::clone(prom->entry, closure, closure);
                bb->overrideNext(prom_copy);

                LdFunctionEnv* e = LdFunctionEnv::Cast(*prom_copy->begin());
                assert(e);
                Replace::usesOfValue(prom_copy, e, mk->promEnv());
                prom_copy->remove(prom_copy->begin());

                Value* promRes = BBTransform::forInline(prom_copy, split);
                mk->eagerArg(promRes);

                bb = split;
                ip = bb->begin();
            } else if (auto call = StaticCall::Cast(*ip)) {
                auto version = call->tryDispatch();
                if (version && version->properties.includes(
                                   ClosureVersion::Property::NoReflection)) {
                    call->eachCallArg([&](InstrArg& arg) {
                        if (auto mk = MkArg::Cast(arg.val())) {
                            if (mk->isEager()) {
                                arg.val() = mk->eagerArg();
                            }
                        }
                    });
                }
            }
        }
    });
}
} // namespace pir
} // namespace rir
