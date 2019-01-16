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

void EagerCalls::apply(RirCompiler& cmp, Closure* closure, LogStream&) const {
    std::unordered_set<MkArg*> todo;
    auto code = closure->entry;

    Visitor::run(code, [&](BB* bb) {
        for (auto ip = bb->begin(); ip != bb->end(); ++ip) {
            auto call = StaticCall::Cast(*ip);
            if (!call)
                continue;

            Closure* cls = call->cls();

            if (!cls->properties.includes(Closure::Property::IsEager) ||
                call->nCallArgs() != cls->nargs())
                continue;

            std::unordered_set<MkArg*> args;
            bool noMissing = true;
            call->eachCallArg([&](Value* v) {
                if (auto mk = MkArg::Cast(v)) {
                    if (mk->eagerArg() == Missing::instance())
                        args.insert(mk);
                } else {
                    noMissing = false;
                }
            });
            if (!noMissing)
                continue;
            todo.insert(args.begin(), args.end());

            if (cls->assumptions.includes(Assumption::EagerArgs))
                continue;

            Assumptions newAssumptions;
            newAssumptions.set(Assumption::EagerArgs);
            newAssumptions.set(Assumption::NoMissingArguments);
            // This might fire back, since we don't know if we really have no
            // objects... We should have some profiling. It's still sound, since
            // static_call_ will check the assumptions
            newAssumptions.set(Assumption::NonObjectArgs);
            auto withEagerArgs = cmp.cloneWithAssumptions(
                cls, newAssumptions, [&](Closure* newCls) {
                    Visitor::run(newCls->entry, [&](Instruction* i) {
                        if (auto ld = LdArg::Cast(i)) {
                            ld->type = PirType::val();
                            ld->type.setNotObject();
                        }
                    });
                });
            call->cls(withEagerArgs);
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
                if (call->cls()->properties.includes(
                        Closure::Property::NoReflection)) {
                    call->eachCallArg([&](InstrArg& arg) {
                        if (auto mk = MkArg::Cast(arg.val())) {
                            mk->ifEager(
                                [&](Value* eagerVal) { arg.val() = eagerVal; });
                        }
                    });
                }
            }
        }
    });
}
} // namespace pir
} // namespace rir
