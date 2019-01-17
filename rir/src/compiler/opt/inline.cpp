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
    ClosureVersion* version;
    explicit TheInliner(ClosureVersion* version) : version(version) {}

    static const size_t MAX_SIZE;
    static const size_t MAX_INLINEE_SIZE;
    static const size_t INITIAL_FUEL;

    void operator()() {
        size_t fuel = INITIAL_FUEL;

        if (version->size() > MAX_SIZE)
            return;

        std::unordered_set<ClosureVersion*> skip;

        Visitor::run(version->entry, [&](BB* bb) {
            // Dangerous iterater usage, works since we do only update it in
            // one place.
            for (auto it = bb->begin(); it != bb->end() && fuel; it++) {
                if (!CallInstruction::CastCall(*it))
                    continue;

                Closure* inlineeCls = nullptr;
                ClosureVersion* inlinee = nullptr;
                Value* staticEnv = nullptr;

                const FrameState* callerFrameState = nullptr;
                if (auto call = Call::Cast(*it)) {
                    auto mkcls =
                        MkFunCls::Cast(call->cls()->followCastsAndForce());
                    if (!mkcls)
                        continue;
                    inlineeCls = mkcls->cls;
                    inlinee = call->dispatch(inlineeCls);
                    if (inlineeCls->nargs() != call->nCallArgs())
                        continue;
                    staticEnv = mkcls->lexicalEnv();
                    callerFrameState = call->frameState();
                } else if (auto call = StaticCall::Cast(*it)) {
                    inlineeCls = call->cls();
                    inlinee = call->dispatch();
                    // if we don't know the closure of the inlinee, we can't
                    // inline.
                    if (inlineeCls->closureEnv() == Env::notClosed() &&
                        inlinee != version)
                        continue;
                    assert(inlineeCls->nargs() == call->nCallArgs());
                    staticEnv = inlineeCls->closureEnv();
                    callerFrameState = call->frameState();
                } else {
                    continue;
                }

                if (skip.count(inlinee))
                    continue;

                // No recursive inlining
                if (inlinee->owner() == version->owner()) {
                    skip.insert(inlinee);
                    continue;
                }

                if (inlinee->size() > MAX_INLINEE_SIZE) {
                    skip.insert(inlinee);
                    continue;
                }

                fuel--;

                BB* split =
                    BBTransform::split(version->nextBBId++, bb, it, version);
                auto theCall = *split->begin();
                auto theCallInstruction = CallInstruction::CastCall(theCall);
                std::vector<Value*> arguments;
                theCallInstruction->eachCallArg(
                    [&](Value* v) { arguments.push_back(v); });

                // Clone the version
                BB* copy = BBTransform::clone(inlinee->entry, version, version);

                bool needsEnvPatching = inlineeCls->closureEnv() != staticEnv;

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
                        // inner version. Then the lexical env is the current
                        // versions env.
                        if (needsEnvPatching && i->hasEnv() &&
                            i->env() == inlineeCls->closureEnv()) {
                            i->env(staticEnv);
                        }
                        if (ld) {
                            Value* a = arguments[ld->id];
                            if (auto mk = MkArg::Cast(a)) {
                                // We need to cast from a promise to a lazy
                                // value
                                auto cast = new CastType(
                                    a, RType::prom,
                                    mk->eagerArg() == Missing::instance()
                                        ? PirType::any()
                                        : PirType::val());
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

                    // Copy over promises used by the inner version
                    std::vector<bool> copiedPromise(false);
                    std::vector<size_t> newPromId;
                    copiedPromise.resize(inlinee->promises().size(), false);
                    newPromId.resize(inlinee->promises().size());
                    Visitor::run(copy, [&](BB* bb) {
                        auto it = bb->begin();
                        while (it != bb->end()) {
                            MkArg* mk = MkArg::Cast(*it);
                            it++;
                            if (!mk)
                                continue;

                            size_t id = mk->prom()->id;
                            if (mk->prom()->owner == inlinee) {
                                assert(id < copiedPromise.size());
                                if (copiedPromise[id]) {
                                    mk->updatePromise(
                                        version->promises().at(newPromId[id]));
                                } else {
                                    Promise* clone = version->createProm(
                                        mk->prom()->srcPoolIdx());
                                    BB* promCopy = BBTransform::clone(
                                        mk->prom()->entry, clone, version);
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

// TODO: maybe implement something more resonable to pass in those constants.
// For now it seems a simple env variable is just fine.
const size_t TheInliner::MAX_SIZE = getenv("PIR_INLINER_MAX_SIZE")
                                        ? atoi(getenv("PIR_INLINER_MAX_SIZE"))
                                        : 10000;
const size_t TheInliner::MAX_INLINEE_SIZE =
    getenv("PIR_INLINER_MAX_INLINEE_SIZE")
        ? atoi(getenv("PIR_INLINER_MAX_INLINEE_SIZE"))
        : 200;
const size_t TheInliner::INITIAL_FUEL =
    getenv("PIR_INLINER_INITIAL_FUEL")
        ? atoi(getenv("PIR_INLINER_INITIAL_FUEL"))
        : 5;

} // namespace

namespace rir {
namespace pir {

void Inline::apply(RirCompiler&, ClosureVersion* version, LogStream&) const {
    TheInliner s(version);
    s();
}
} // namespace pir
} // namespace rir
