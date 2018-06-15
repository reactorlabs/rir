#include "inline.h"
#include "../pir/pir_impl.h"
#include "../transform/bb.h"
#include "../transform/replace.h"
#include "../util/cfg.h"
#include "../util/visitor.h"
#include "R/r.h"

#include <algorithm>
#include <unordered_map>

namespace {

using namespace rir::pir;

class TheInliner {
  public:
    Closure* function;
    TheInliner(Closure* function) : function(function) {}

    void operator()() {

        Visitor::run(function->entry, [&](BB* bb) {
            // Dangerous iterater usage, works since we do only update it in
            // one place.
            for (auto it = bb->begin(); it != bb->end(); it++) {
                auto call = CallInstruction::Cast(*it);
                if (!call)
                    continue;

                Closure* inlinee = nullptr;
                Value* staticEnv = nullptr;

                if (Call::Cast(*it)) {
                    Call* call = Call::Cast(*it);
                    auto cls = MkFunCls::Cast(call->cls());
                    if (!cls)
                        continue;
                    inlinee = cls->fun;
                    if (inlinee->argNames.size() != call->nCallArgs())
                        continue;
                    assert(cls->fun->closureEnv() == Env::notClosed());
                    staticEnv = cls->env();
                } else if (StaticCall::Cast(*it)) {
                    StaticCall* call = StaticCall::Cast(*it);
                    inlinee = call->cls();
                    staticEnv = inlinee->closureEnv();
                } else if (StaticEagerCall::Cast(*it)) {
                    StaticEagerCall* call = StaticEagerCall::Cast(*it);
                    inlinee = call->cls();
                    staticEnv = inlinee->closureEnv();
                } else {
                    assert(false);
                }

                BB* split =
                    BBTransform::split(function->nextBBId++, bb, it, function);

                auto theCall = *split->begin();
                auto theCallInstruction = CallInstruction::Cast(theCall);
                std::vector<Value*> arguments;
                theCallInstruction->eachCallArg(
                    [&](Value* v) { arguments.push_back(v); });

                // Clone the function
                BB* copy = BBTransform::clone(inlinee->entry, function);

                // Link all inner environments to the outer one
                Visitor::run(copy, [&](BB* bb) {
                    auto ip = bb->begin();
                    while (ip != bb->end()) {
                        auto next = ip + 1;
                        auto ld = LdArg::Cast(*ip);
                        Instruction* i = *ip;
                        if (i->hasEnv() && i->env() == Env::notClosed()) {
                            i->env(staticEnv);
                        }
                        if (ld) {
                            Value* a = arguments[ld->id];
                            if (MkArg::Cast(a)) {
                                // We need to cast from a promise to a lazy
                                // value
                                auto cast = new CastType(a, RType::prom,
                                                         PirType::any());
                                next = bb->insert(ip + 1, cast);
                                a = cast;
                            }
                            ld->replaceUsesWith(a);
                            next = bb->remove(ip);
                        }
                        ip = next;
                    }
                });

                bb->next0 = copy;

                // Copy over promises used by the inner function
                std::vector<bool> copiedPromise;
                std::vector<size_t> newPromId;
                copiedPromise.resize(inlinee->promises.size(), false);
                newPromId.resize(inlinee->promises.size());
                Visitor::run(copy, [&](BB* bb) {
                    auto it = bb->begin();
                    while (it != bb->end()) {
                        MkArg* mk = MkArg::Cast(*it);
                        it++;
                        if (!mk)
                            continue;

                        size_t id = mk->prom->id;
                        if (mk->prom->fun == inlinee) {
                            assert(id < copiedPromise.size());
                            if (copiedPromise[id]) {
                                mk->prom = function->promises[newPromId[id]];
                            } else {
                                Promise* clone = function->createProm();
                                BB* promCopy =
                                    BBTransform::clone(mk->prom->entry, clone);
                                clone->entry = promCopy;
                                newPromId[id] = clone->id;
                                copiedPromise[id] = true;
                                mk->prom = clone;
                            }
                        }
                    }
                });

                Value* inlineeRes = BBTransform::forInline(copy, split);
                theCall->replaceUsesWith(inlineeRes);

                // Remove the call instruction
                split->remove(split->begin());

                bb = split;
                it = split->begin();

                // Can happen if split only contained the call instruction
                if (it == split->end())
                    break;
            }
        });
    }
};
}

namespace rir {
namespace pir {

void Inline::apply(Closure* function) {
    TheInliner s(function);
    s();
}
}
}
