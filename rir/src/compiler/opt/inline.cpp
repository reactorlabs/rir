#include "../analysis/query.h"
#include "../pir/pir_impl.h"
#include "../util/safe_builtins_list.h"
#include "R/BuiltinIds.h"
#include "R/Funtab.h"
#include "R/Symbols.h"
#include "R/r.h"
#include "compiler/analysis/cfg.h"
#include "compiler/parameter.h"
#include "compiler/util/bb_transform.h"
#include "compiler/util/visitor.h"
#include "pass_definitions.h"
#include "utils/Pool.h"

#include <algorithm>
#include <unordered_map>

namespace rir {
namespace pir {

bool Inline::apply(Compiler&, ClosureVersion* cls, Code* code,
                   LogStream& log) const {
    bool anyChange = false;
    size_t fuel = Parameter::INLINER_INITIAL_FUEL;

    if (cls->size() > Parameter::INLINER_MAX_SIZE)
        return false;

    auto dontInline = [](Closure* cls) {
        if (cls->rirFunction()->flags.contains(rir::Function::DisableInline))
            return true;
        if (cls->rirFunction()->flags.contains(rir::Function::ForceInline))
            return false;
        return cls->rirFunction()->flags.contains(rir::Function::NotInlineable);
    };

    std::unordered_set<BB*> dead;
    Visitor::run(
        code->entry, [&](BB* bb) {
            // Dangerous iterater usage, works since we do only update it in
            // one place.
            for (auto it = bb->begin(); it != bb->end() && fuel; it++) {
                if (!CallInstruction::CastCall(*it))
                    continue;

                Closure* inlineeCls = nullptr;
                ClosureVersion* inlinee = nullptr;
                Value* staticEnv = nullptr;

                bool hasDotslistArg = false;
                const FrameState* callerFrameState = nullptr;
                if (auto call = Call::Cast(*it)) {
                    auto mkcls =
                        MkFunCls::Cast(call->cls()->followCastsAndForce());
                    if (!mkcls)
                        continue;
                    inlineeCls = mkcls->cls;
                    if (dontInline(inlineeCls))
                        continue;
                    inlinee = call->tryDispatch(inlineeCls);
                    if (!inlinee)
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
                    if (dontInline(inlineeCls))
                        continue;
                    inlinee = call->tryDispatch();
                    if (!inlinee)
                        continue;
                    // if we don't know the closure of the inlinee, we can't
                    // inline.
                    staticEnv = inlineeCls->closureEnv();
                    if (inlineeCls->closureEnv() == Env::notClosed() &&
                        inlinee != cls) {
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
                    call->eachCallArg([&](Value* v) {
                        assert(!ExpandDots::Cast(v));
                        if (DotsList::Cast(v))
                            hasDotslistArg = true;
                    });
                    callerFrameState = call->frameState();
                } else {
                    continue;
                }

                // TODO: wtf is going on here????
                if (inlineeCls->name().size() >= 7 &&
                    inlineeCls->name().substr(0, 7) == "allTrue")
                    continue;

                if (dontInline(inlineeCls))
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
                    auto env = Env::Cast(inlineeCls->closureEnv());
                    if (env && env->rho && R_IsNamespaceEnv(env->rho)) {
                        auto expr = BODY_EXPR(inlineeCls->rirClosure());
                        // Closure wrappers for internals
                        if (CAR(expr) == rir::symbol::Internal)
                            weight *= 0.6;
                        // those usually strongly benefit type
                        // inference, since they have a lot of case
                        // distinctions
                        static auto profitable =
                            std::unordered_set<std::string>(
                                {"matrix", "array", "vector", "cat"});
                        if (profitable.count(inlineeCls->name()))
                            weight *= 0.4;
                    }
                    if (hasDotslistArg)
                        weight *= 0.4;
                }

                // No recursive inlining
                if (inlinee->owner() == cls->owner()) {
                    continue;
                } else if (weight > Parameter::INLINER_MAX_INLINEE_SIZE) {
                    if (!inlineeCls->rirFunction()->flags.contains(
                            rir::Function::ForceInline))
                        inlineeCls->rirFunction()->flags.set(
                            rir::Function::NotInlineable);
                    continue;
                } else {
                    updateAllowInline(inlinee);
                    inlinee->eachPromise(
                        [&](Promise* p) { updateAllowInline(p); });
                    if (allowInline == SafeToInline::No) {
                        inlineeCls->rirFunction()->flags.set(
                            rir::Function::NotInlineable);
                        continue;
                    }
                }

                if (!inlineeCls->rirFunction()->flags.contains(
                        rir::Function::ForceInline))
                    fuel--;

                cls->inlinees++;
                auto context = (*it)->env();

                BB* split = BBTransform::split(cls->nextBBId++, bb, it, cls);
                auto theCall = *split->begin();
                auto theCallInstruction = CallInstruction::CastCall(theCall);
                std::vector<Value*> arguments;
                theCallInstruction->eachCallArg(
                    [&](Value* v) { arguments.push_back(v); });

                // Clone the version
                BB* copy = BBTransform::clone(inlinee->entry, code, cls);

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
                            Value* a = (ld->id < arguments.size())
                                           ? arguments[ld->id]
                                           : MissingArg::instance();
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
                    inlineeCls->rirFunction()->flags.set(
                        rir::Function::NotInlineable);
                } else {
                    anyChange = true;
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
                                        cls->promises().at(newPromId[id]));
                                } else {
                                    Promise* clone =
                                        cls->createProm(mk->prom()->rirSrc());
                                    BB* promCopy = BBTransform::clone(
                                        mk->prom()->entry, clone, cls);
                                    clone->entry = promCopy;
                                    newPromId[id] = clone->id;
                                    copiedPromise[id] = true;
                                    mk->updatePromise(clone);
                                }
                            }
                        }
                    });

                    auto inlineeRet =
                        BBTransform::forInline(copy, split, context);
                    if (inlineeRet.second->isNonLocalReturn())
                        dead.insert(inlineeRet.second);
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
                        auto ctx = new PushContext(ast, op, theCallInstruction,
                                                   theCall->env());
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

    BBTransform::removeDeadBlocks(code, dead);
    return anyChange;
    }

// TODO: maybe implement something more resonable to pass in those constants.
// For now it seems a simple env variable is just fine.
size_t Parameter::INLINER_MAX_SIZE = getenv("PIR_INLINER_MAX_SIZE")
                                         ? atoi(getenv("PIR_INLINER_MAX_SIZE"))
                                         : 4000;
size_t Parameter::INLINER_MAX_INLINEE_SIZE =
    getenv("PIR_INLINER_MAX_INLINEE_SIZE")
        ? atoi(getenv("PIR_INLINER_MAX_INLINEE_SIZE"))
        : 200;
size_t Parameter::INLINER_INITIAL_FUEL =
    getenv("PIR_INLINER_INITIAL_FUEL")
        ? atoi(getenv("PIR_INLINER_INITIAL_FUEL"))
        : 15;
size_t Parameter::INLINER_INLINE_UNLIKELY =
    getenv("PIR_INLINER_INLINE_UNLIKELY")
        ? atoi(getenv("PIR_INLINER_INLINE_UNLIKELY"))
        : 0;

} // namespace pir
} // namespace rir
