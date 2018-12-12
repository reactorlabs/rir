#include "../pir/pir_impl.h"
#include "../transform/bb.h"
#include "../util/cfg.h"
#include "../util/visitor.h"
#include "R/Funtab.h"
#include "R/Symbols.h"
#include "R/r.h"
#include "pass_definitions.h"

#include <algorithm>
#include <unordered_map>

namespace {

using namespace rir::pir;

class TheInliner {
  public:
    Closure* function;
    explicit TheInliner(Closure* function) : function(function) {}

    static constexpr size_t MAX_SIZE = 10000;
    static constexpr size_t MAX_INLINEE_SIZE = 200;
    static constexpr size_t MAX_FUEL = 5;

    void operator()() {
        size_t fuel = MAX_FUEL;

        if (function->size() > MAX_SIZE)
            return;

        std::unordered_set<Closure*> skip;

        Visitor::run(function->entry, [&](BB* bb) {
            // Dangerous iterater usage, works since we do only update it in
            // one place.
            for (auto it = bb->begin(); it != bb->end() && fuel; it++) {
                if (!CallInstruction::CastCall(*it))
                    continue;

                Closure* inlinee = nullptr;
                Value* staticEnv = nullptr;

                FrameState* callerFrameState = nullptr;
                if (auto call = Call::Cast(*it)) {
                    auto mkcls =
                        MkFunCls::Cast(call->cls()->followCastsAndForce());
                    if (!mkcls)
                        continue;
                    inlinee = mkcls->fun;
                    if (inlinee->argNames.size() != call->nCallArgs())
                        continue;
                    staticEnv = mkcls->lexicalEnv();
                    callerFrameState = call->frameState();
                } else if (auto call = StaticCall::Cast(*it)) {
                    inlinee = call->cls();
                    // if we don't know the closure of the inlinee, we can't
                    // inline.
                    if (inlinee->closureEnv() == Env::notClosed() &&
                        inlinee != function)
                        continue;
                    assert(inlinee->argNames.size() == call->nCallArgs());
                    staticEnv = inlinee->closureEnv();
                    callerFrameState = call->frameState();
                } else {
                    continue;
                }

                if (skip.count(inlinee))
                    continue;

                // Recursive inline only once
                if (inlinee == function)
                    skip.insert(inlinee);

                if (inlinee->size() > MAX_INLINEE_SIZE) {
                    skip.insert(inlinee);
                    continue;
                }

                fuel--;

                BB* split =
                    BBTransform::split(function->nextBBId++, bb, it, function);
                auto theCall = *split->begin();
                auto theCallInstruction = CallInstruction::CastCall(theCall);
                std::vector<Value*> arguments;
                theCallInstruction->eachCallArg(
                    [&](Value* v) { arguments.push_back(v); });

                // Clone the function
                BB* copy =
                    BBTransform::clone(inlinee->entry, function, function);

                bool needsEnvPatching = inlinee->closureEnv() != staticEnv;

                bool fail = false;
                Visitor::run(copy, [&](BB* bb) {
                    auto ip = bb->begin();
                    while (!fail && ip != bb->end()) {
                        auto next = ip + 1;
                        auto ld = LdArg::Cast(*ip);
                        Instruction* i = *ip;
                        // We should never inline UseMethod
                        if (auto ld = LdFun::Cast(i)) {
                            if (ld->varName == rir::symbol::UseMethod ||
                                ld->varName == rir::symbol::standardGeneric) {
                                fail = true;
                                return;
                            }
                        }
                        if (auto sp = FrameState::Cast(i)) {
                            if (!callerFrameState) {
                                fail = true;
                                return;
                            }

                            // When inlining a frameState we need to chain it
                            // with the frameStates after the call to the
                            // inlinee
                            if (!sp->next()) {
                                auto copyFromFs = callerFrameState;
                                auto cloneSp =
                                    FrameState::Cast(copyFromFs->clone());

                                ip = bb->insert(ip, cloneSp);
                                sp->next(cloneSp);

                                size_t created = 1;
                                while (copyFromFs->next()) {
                                    assert(copyFromFs->next() ==
                                           cloneSp->next());
                                    copyFromFs = copyFromFs->next();
                                    auto prevClone = cloneSp;
                                    cloneSp =
                                        FrameState::Cast(copyFromFs->clone());

                                    ip = bb->insert(ip, cloneSp);
                                    created++;

                                    prevClone->updateNext(cloneSp);
                                }

                                next = ip + created + 1;
                            }
                        }
                        // If the inlining resolved some env, we need to
                        // update. For example this happens if we inline an
                        // inner function. Then the lexical env is the current
                        // functions env.
                        if (needsEnvPatching && i->hasEnv() &&
                            i->env() == inlinee->closureEnv()) {
                            i->env(staticEnv);
                        }
                        if (ld) {
                            Value* a = arguments[ld->id];
                            if (MkArg::Cast(a)) {
                                // We need to cast from a promise to a lazy
                                // value
                                auto cast = new CastType(a, RType::prom,
                                                         PirType::any());
                                ip = bb->insert(ip + 1, cast);
                                ip--;
                                a = cast;
                            }
                            ld->replaceUsesWith(a);
                            next = bb->remove(ip);
                        }
                        ip = next;
                    }
                });

                if (fail) {
                    delete copy;
                    bb->overrideNext(split);

                } else {

                    bb->overrideNext(copy);

                    // Copy over promises used by the inner function
                    std::vector<bool> copiedPromise(false);
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

                            size_t id = mk->prom()->id;
                            if (mk->prom()->fun == inlinee) {
                                assert(id < copiedPromise.size());
                                if (copiedPromise[id]) {
                                    mk->updatePromise(
                                        function->promises.at(newPromId[id]));
                                } else {
                                    Promise* clone = function->createProm(
                                        mk->prom()->srcPoolIdx());
                                    BB* promCopy = BBTransform::clone(
                                        mk->prom()->entry, clone, function);
                                    clone->entry = promCopy;
                                    newPromId[id] = clone->id;
                                    copiedPromise[id] = true;
                                    mk->updatePromise(clone);
                                }
                            }
                        }
                    });

                    Value* inlineeRes = BBTransform::forInline(copy, split);
                    theCall->replaceUsesWith(inlineeRes);

                    // Remove the call instruction
                    split->remove(split->begin());
                }

                bb = split;
                it = split->begin();

                // Can happen if split only contained the call instruction
                if (it == split->end())
                    break;
            }
        });
    }
};
} // namespace

namespace rir {
namespace pir {

void Inline::apply(RirCompiler&, Closure* function, LogStream&) const {
    TheInliner s(function);
    s();
}
} // namespace pir
} // namespace rir
