#include "../analysis/query.h"
#include "../pir/pir_impl.h"
#include "../transform/bb.h"
#include "../util/cfg.h"
#include "../util/safe_builtins_list.h"
#include "../util/visitor.h"
#include "R/BuiltinIds.h"
#include "R/Funtab.h"
#include "R/Symbols.h"
#include "R/r.h"
#include "compiler/parameter.h"
#include "pass_definitions.h"
#include "utils/Pool.h"

#include <algorithm>
#include <unordered_map>

namespace {

using namespace rir::pir;

class TheInliner {
  public:
    ClosureVersion* version;
    explicit TheInliner(ClosureVersion* version) : version(version) {}

    void operator()() {
        size_t fuel = Parameter::INLINER_INITIAL_FUEL;

        if (version->size() > Parameter::INLINER_MAX_SIZE)
            return;

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
                    if (inlineeCls->rirFunction()->uninlinable)
                        continue;
                    inlinee = call->tryDispatch(inlineeCls);
                    if (!inlinee)
                        continue;
                    if (inlinee->nargs() -
                            inlinee->assumptions().numMissing() !=
                        call->nCallArgs())
                        continue;
                    bool hasDotArgs = false;
                    call->eachCallArg([&](Value* v) {
                        if (ExpandDots::Cast(v))
                            hasDotArgs = true;
                    });
                    // TODO do some argument matching
                    if (hasDotArgs)
                        continue;
                    staticEnv = mkcls->lexicalEnv();
                    callerFrameState = call->frameState();
                } else if (auto call = StaticCall::Cast(*it)) {
                    inlineeCls = call->cls();
                    if (inlineeCls->rirFunction()->uninlinable)
                        continue;
                    inlinee = call->tryDispatch();
                    if (!inlinee)
                        continue;
                    if (inlinee->nargs() -
                            inlinee->assumptions().numMissing() !=
                        call->nCallArgs())
                        continue;
                    // if we don't know the closure of the inlinee, we can't
                    // inline.
                    staticEnv = inlineeCls->closureEnv();
                    if (inlineeCls->closureEnv() == Env::notClosed() &&
                        inlinee != version) {
                        if (Query::noParentEnv(inlinee)) {
                        } else if (auto mk =
                                       MkFunCls::Cast(call->runtimeClosure())) {
                            staticEnv = mk->lexicalEnv();
                        } else if (auto mk =
                                       MkCls::Cast(call->runtimeClosure())) {
                            staticEnv = mk->lexicalEnv();
                        } else if (call->runtimeClosure() !=
                                   Tombstone::closure()) {
                            static SEXP b = nullptr;
                            if (!b) {
                                auto idx = rir::blt("environment");
                                b = Rf_allocSExp(BUILTINSXP);
                                b->u.primsxp.offset = idx;
                                R_PreserveObject(b);
                            }
                            auto e = new CallSafeBuiltin(
                                b, {call->runtimeClosure()}, 0);
                            e->type = RType::env;
                            e->effects.reset();
                            it = bb->insert(it, e);
                            it++;
                            staticEnv = e;
                        } else {
                            continue;
                        }
                    }
                    call->eachCallArg(
                        [&](Value* v) { assert(!ExpandDots::Cast(v)); });
                    callerFrameState = call->frameState();
                } else {
                    continue;
                }

                if (inlineeCls->rirFunction()->uninlinable)
                    continue;

                enum SafeToInline {
                    Yes,
                    NeedsContext,
                    No,
                };

                // TODO: instead of blacklisting those, we could also create
                // contexts for inlined functions.
                SafeToInline allowInline = SafeToInline::Yes;
                std::function<void(Code*)> updateAllowInline = [&](Code* code) {
                    Visitor::check(code->entry, [&](Instruction* i) {
                        if (LdFun::Cast(i) || LdVar::Cast(i)) {
                            auto n = LdFun::Cast(i) ? LdFun::Cast(i)->varName
                                                    : LdVar::Cast(i)->varName;
                            if (!SafeBuiltinsList::forInlineByName(n)) {
                                allowInline = SafeToInline::No;
                                return false;
                            }
                        }
                        if (auto call = CallBuiltin::Cast(i)) {
                            if (!SafeBuiltinsList::forInline(call->builtinId)) {
                                allowInline = SafeToInline::No;
                                return false;
                            }
                        }
                        if (allowInline == SafeToInline::Yes &&
                            i->mayObserveContext()) {
                            allowInline = SafeToInline::NeedsContext;
                        }
                        if (auto mk = MkArg::Cast(i)) {
                            updateAllowInline(mk->prom());
                        }
                        return true;
                    });
                };

                size_t weight = inlinee->size();
                // The taken information of the call instruction tells us how
                // many times a call was executed relative to function
                // invocation. 0 means never, 1 means on every call, above 1
                // means more than once per call, ie. in a loop.
                if (auto c = CallInstruction::CastCall(*it)) {
                    if (c->taken != CallInstruction::UnknownTaken &&
                        !Parameter::INLINER_INLINE_UNLIKELY) {
                        // Policy: for calls taken about 80% the time the weight
                        // stays unchanged. Below it's increased and above it
                        // is decreased, but not more than 4x
                        double adjust = 1.25 * c->taken;
                        if (adjust > 3)
                            adjust = 3;
                        if (adjust < 0.25)
                            adjust = 0.25;
                        weight = (double)weight / adjust;
                        // Inline only small methods if we are getting close to
                        // the limit.
                        auto limit = (double)inlinee->size() /
                                     (double)Parameter::INLINER_MAX_SIZE;
                        limit = (limit * 4) + 1;
                        weight *= limit;
                    }
                    if (auto c = StaticCall::Cast(*it)) {
                        if (auto env = Env::Cast(c->cls()->closureEnv())) {
                            if (env->rho && R_IsNamespaceEnv(env->rho)) {
                                weight *= 0.8;
                                auto expr = BODY_EXPR(c->cls()->rirClosure());
                                // Closure wrappers for internals
                                if (CAR(expr) == rir::symbol::Internal)
                                    weight *= 0.1;
                                // those usually strongly benefit type
                                // inference, since they have a lot of case
                                // distinctions
                                static auto profitable =
                                    std::unordered_set<std::string>(
                                        {"matrix", "array", "vector", "colSums",
                                         "colMeans", "set.seed"});
                                if (profitable.count(c->cls()->name())) {
                                    weight *= 0.1;
                                }
                            }
                        }
                    }
                }

                // No recursive inlining
                if (inlinee->owner() == version->owner()) {
                    continue;
                } else if (weight > Parameter::INLINER_MAX_INLINEE_SIZE) {
                    inlineeCls->rirFunction()->uninlinable = true;
                    continue;
                } else {
                    updateAllowInline(inlinee);
                    inlinee->eachPromise(
                        [&](Promise* p) { updateAllowInline(p); });
                    if (allowInline == SafeToInline::No) {
                        inlineeCls->rirFunction()->uninlinable = true;
                        continue;
                    }
                }

                fuel--;
                version->inlinees++;

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

                bool failedToInline = false;
                Visitor::run(copy, [&](BB* bb) {
                    auto ip = bb->begin();
                    while (!failedToInline && ip != bb->end()) {
                        auto next = ip + 1;
                        auto ld = LdArg::Cast(*ip);
                        Instruction* i = *ip;

                        if (auto sp = FrameState::Cast(i)) {
                            if (!callerFrameState) {
                                failedToInline = true;
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

                        // If we inline without context, then we need to update
                        // the mkEnv instructions in the inlinee, such that
                        // they do not update the (non-existing) context.
                        if (allowInline != SafeToInline::NeedsContext) {
                            if (auto mk = MkEnv::Cast(i)) {
                                mk->context--;
                            }
                        }

                        if (ld) {
                            Value* a = arguments[ld->id];
                            if (auto mk = MkArg::Cast(a)) {
                                if (!ld->type.maybePromiseWrapped()) {
                                    // This load already expects to load an
                                    // eager value. We can just discard the
                                    // promise altogether.
                                    assert(mk->isEager());
                                    a = mk->eagerArg();
                                } else {
                                    // We need to cast from a promise to a lazy
                                    // value
                                    auto type = mk->isEager()
                                                    ? mk->eagerArg()
                                                          ->type.forced()
                                                          .orPromiseWrapped()
                                                    : ld->type;
                                    auto cast = new CastType(
                                        a, CastType::Upcast, RType::prom,
                                        type.notMissing());
                                    ip = bb->insert(ip + 1, cast);
                                    ip--;
                                    a = cast;
                                }
                            }
                            if (a == MissingArg::instance()) {
                                ld->replaceUsesWith(
                                    a, [&](Instruction* usage, size_t arg) {
                                        if (auto mk = MkEnv::Cast(usage))
                                            mk->missing[arg] = true;
                                    });
                            } else {
                                ld->replaceUsesWith(a);
                            }
                            next = bb->remove(ip);
                        }
                        ip = next;
                    }
                });

                if (failedToInline) {
                    delete copy;
                    bb->overrideNext(split);
                    inlineeCls->rirFunction()->uninlinable = true;
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
                                        mk->prom()->rirSrc());
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

                    auto inlineeRet = BBTransform::forInline(copy, split);
                    Value* inlineeRes = inlineeRet.first;
                    BB* inlineeReturnblock = inlineeRet.second;
                    if (allowInline == SafeToInline::NeedsContext) {
                        size_t insertPos = 0;
                        Value* op = nullptr;
                        if (auto call = Call::Cast(theCall)) {
                            op = call->cls();
                        } else if (auto call = StaticCall::Cast(theCall)) {
                            if (call->runtimeClosure() !=
                                Tombstone::closure()) {
                                op = call->runtimeClosure();
                            } else {
                                auto ld =
                                    new LdConst(call->cls()->rirClosure());
                                copy->insert(copy->begin(), ld);
                                op = ld;
                                insertPos++;
                            }
                        }
                        assert(op);
                        auto ast = new LdConst(rir::Pool::get(theCall->srcIdx));
                        auto ctx = new PushContext(ast, op, theCall->env());
                        copy->insert(copy->begin() + insertPos, ctx);
                        copy->insert(copy->begin() + insertPos, ast);
                        auto popc = new PopContext(inlineeRes, ctx);
                        inlineeReturnblock->append(popc);
                        popc->updateTypeAndEffects();
                        inlineeRes = popc;
                    }

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

// TODO: maybe implement something more resonable to pass in those constants.
// For now it seems a simple env variable is just fine.
size_t Parameter::INLINER_MAX_SIZE = getenv("PIR_INLINER_MAX_SIZE")
                                         ? atoi(getenv("PIR_INLINER_MAX_SIZE"))
                                         : 4000;
size_t Parameter::INLINER_MAX_INLINEE_SIZE =
    getenv("PIR_INLINER_MAX_INLINEE_SIZE")
        ? atoi(getenv("PIR_INLINER_MAX_INLINEE_SIZE"))
        : 350;
size_t Parameter::INLINER_INITIAL_FUEL =
    getenv("PIR_INLINER_INITIAL_FUEL")
        ? atoi(getenv("PIR_INLINER_INITIAL_FUEL"))
        : 8;
size_t Parameter::INLINER_INLINE_UNLIKELY =
    getenv("PIR_INLINER_INLINE_UNLIKELY")
        ? atoi(getenv("PIR_INLINER_INLINE_UNLIKELY"))
        : 0;

void Inline::apply(RirCompiler&, ClosureVersion* version, LogStream&) const {
    TheInliner s(version);
    s();
}
} // namespace pir
} // namespace rir
