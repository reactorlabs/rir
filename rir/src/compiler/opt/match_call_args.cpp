#include "compiler/pir/pir_impl.h"
#include "compiler/util/arg_match.h"
#include "compiler/util/visitor.h"
#include "pass_definitions.h"

namespace rir {
namespace pir {

// Try to match callsite arguments to formals
bool MatchCallArgs::apply(Compiler& cmp, ClosureVersion* cls, Code* code,
                          LogStream& log) const {
    bool anyChange = false;
    Visitor::run(code->entry, [&](BB* bb) {
        auto ip = bb->begin();
        while (ip != bb->end()) {
            auto next = ip + 1;

            if (auto calli = CallInstruction::CastCall(*ip)) {
                if (!Call::Cast(*ip) && !NamedCall::Cast(*ip)) {
                    ip = next;
                    continue;
                }

                if (auto cls = calli->tryGetCls()) {
                    if (auto f = calli->tryDispatch(cls)) {
                        std::vector<Value*> matchedArgs;
                        ArglistOrder::CallArglistOrder argOrderOrig;
                        auto i = Instruction::Cast(*ip);
                        auto call = Call::Cast(*ip);
                        auto namedCall = NamedCall::Cast(*ip);
                        if (ArgumentMatcher::reorder(
                                [](DotsList* d) { /* nothing */ },
                                cls->formals().original(),
                                {[&]() { return calli->nCallArgs(); },
                                 [&](size_t i) {
                                     return calli->callArg(i).val();
                                 },
                                 [&](size_t i) {
                                     SLOWASSERT(!namedCall ||
                                                i < namedCall->names.size());
                                     return namedCall ? namedCall->names[i]
                                                      : R_NilValue;
                                 }},
                                matchedArgs, argOrderOrig)) {
                            anyChange = true;
                            if (auto c = call) {
                                auto nc = new StaticCall(
                                    c->env(), f->owner(),
                                    c->inferAvailableAssumptions(), matchedArgs,
                                    std::move(argOrderOrig),
                                    c->frameStateOrTs(), c->srcIdx,
                                    c->cls()->followCastsAndForce());
                                i->replaceUsesAndSwapWith(nc, ip);
                            } else if (auto c = namedCall) {
                                auto nc = new StaticCall(
                                    c->env(), f->owner(),
                                    c->inferAvailableAssumptions(), matchedArgs,
                                    std::move(argOrderOrig),
                                    c->frameStateOrTs(), c->srcIdx,
                                    c->cls()->followCastsAndForce());
                                i->replaceUsesAndSwapWith(nc, ip);
                            } else {
                                assert(false);
                            }
                        }
                    }
                }
            }

            ip = next;
        }
    });
    return anyChange;
}

} // namespace pir
} // namespace rir
