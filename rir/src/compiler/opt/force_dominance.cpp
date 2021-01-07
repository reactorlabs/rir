#include "../analysis/force_dominance.h"
#include "../analysis/available_checkpoints.h"
#include "../analysis/reachability.h"
#include "../parameter.h"
#include "../pir/pir_impl.h"
#include "compiler/util/bb_transform.h"
#include "compiler/util/safe_builtins_list.h"
#include "pass_definitions.h"
#include "utils/Map.h"
#include "utils/Set.h"
#include <unordered_set>

namespace rir {
namespace pir {

/*
 *  In General this is the strategy here:
 *
 *    a  = MkArg(exp)
 *    e  = MkEnv(x=a)   // leak a
 *    a2 = Cast(a)
 *    f  = Force(a2)    // force a
 *    PushContext(a)    // use a for promargs
 *    use(a)            // use a
 *    use(f)
 *
 *  === Inline Promise ==>
 *
 *    a   = MkArg(exp)
 *    e   = MkEnv(x=a)      // leak a
 *    f'  = eval(exp)       // inlinee
 *    a'  = MkArg(exp, f')  // synthesized updated promise
 *    StVar(x=a', e)        // update to ensure leaked a is fixed
 *    PushContext(a)        // push context needs unmodified a
 *    use(a')               // normal uses get the synthesized version
 *    use(f')               // uses of the value get the result
 *
 *  updatePromise and the duplication of mkarg only happens if needed.
 *
 */

bool ForceDominance::apply(Compiler&, ClosureVersion* cls, Code* code,
                           LogStream& log) const {
    SmallSet<Force*> toInline;
    SmallMap<Force*, Force*> dominatedBy;
    SmallMap<Force*, SmallSet<MkEnv*>> needsUpdate;
    bool anyChange = false;

    bool isHuge = code->size() > Parameter::PROMISE_INLINER_MAX_SIZE;
    {
        ForceDominanceAnalysis analysis(cls, code, log);
        AvailableCheckpoints cp(cls, code, log);
        Reachability reachable(analysis.cfg, cp);
        analysis();

        auto result = analysis.result();
        if (code == cls) {
            if (result.eagerLikeFunction(cls))
                cls->properties.set(ClosureVersion::Property::IsEager);
            cls->properties.argumentForceOrder = result.argumentForceOrder;
        }

        VisitorNoDeoptBranch::run(code->entry, [&](BB* bb) {
            auto ip = bb->begin();
            while (ip != bb->end()) {
                auto next = ip + 1;
                auto i = *ip;

                if (auto f = Force::Cast(i)) {
                    auto a =
                        analysis.resultIgnoringUnreachableExits(f, reachable);
                    if (a.isDominatingForce(f)) {
                        f->strict = true;
                        if (auto mk = MkArg::Cast(f->followCastsAndForce())) {
                            if (!mk->isEager()) {
                                if (!isHuge || mk->prom()->size() < 10) {
                                    // We need to know if the promise escaped
                                    // before the force. After the force the
                                    // analysis deletes escape information.
                                    auto b = analysis.before(i);
                                    auto inl = b.isSafeToInline(mk, f);
                                    if (inl != ForcedBy::NotSafeToInline) {
                                        toInline.insert(f);
                                        if (inl ==
                                            ForcedBy::SafeToInlineWithUpdate) {
                                            needsUpdate[f] = b.escaped.at(mk);
                                            assert(!needsUpdate[f].empty());
                                        }
                                    }
                                }
                            }
                        }
                    } else if (auto dom = a.getDominatingForce(f)) {
                        if (f != dom)
                            dominatedBy[f] = dom;
                    }
                }
                ip = next;
            }
        });
    }

    std::unordered_map<Force*, Value*> inlinedPromise;
    std::unordered_map<MkArg*, std::pair<MkArg*, CastType*>> forcedMkArg;
    std::unordered_set<BB*> dead;

    // 1. Inline dominating promises
    Visitor::runPostChange(code->entry, [&](BB* bb) {
        auto ip = bb->begin();
        while (ip != bb->end()) {
            auto next = ip + 1;
            if (auto f = Force::Cast(*ip)) {
                if (auto mkarg = MkArg::Cast(f->followCastsAndForce())) {
                    if (mkarg->isEager()) {
                        anyChange = true;
                        Value* eager = mkarg->eagerArg();
                        f->replaceUsesWith(eager);
                        next = bb->remove(ip);
                    } else if (toInline.count(f)) {
                        anyChange = true;
                        Promise* prom = mkarg->prom();
                        BB* split =
                            BBTransform::split(code->nextBBId++, bb, ip, code);

                        // Note that the entry block is empty and jumps to the
                        // next block; this is to ensure that it has no
                        // predecessors.
                        auto entry = prom->entry;
                        assert(entry->isEmpty() && entry->isJmp() &&
                               !entry->next()->isEmpty());
                        BB* prom_copy =
                            BBTransform::clone(prom->entry->next(), code, cls);
                        bb->overrideNext(prom_copy);

                        // Patch framestates
                        Visitor::runPostChange(prom_copy, [&](BB* bb) {
                            auto it = bb->begin();
                            while (it != bb->end()) {
                                auto next = it + 1;
                                if (f->frameState()) {
                                    if (auto sp = FrameState::Cast(*it)) {
                                        if (!sp->next()) {
                                            auto copyFromFs = f->frameState();
                                            auto cloneSp = FrameState::Cast(
                                                copyFromFs->clone());

                                            it = bb->insert(it, cloneSp);
                                            sp->next(cloneSp);

                                            size_t created = 1;
                                            while (copyFromFs->next()) {
                                                assert(copyFromFs->next() ==
                                                       cloneSp->next());
                                                copyFromFs = copyFromFs->next();
                                                auto prevClone = cloneSp;
                                                cloneSp = FrameState::Cast(
                                                    copyFromFs->clone());

                                                it = bb->insert(it, cloneSp);
                                                created++;

                                                prevClone->updateNext(cloneSp);
                                            }

                                            next = it + created + 1;
                                        }
                                    }
                                } else {
                                    if (FrameState::Cast(*it))
                                        next = bb->remove(it);
                                    // TODO: don't copy this to start with
                                    if ((*it)->frameState())
                                        (*it)->clearFrameState();
                                    if (auto cp = Checkpoint::Cast(*it)) {
                                        auto n = cp->nextBB();
                                        auto d = cp->deoptBranch();
                                        bb->eraseLast();
                                        bb->overrideSuccessors({n});
                                        delete d;
                                        next = bb->end();
                                    }
                                }
                                it = next;
                            }
                        });

                        // Search for the env instruction of the promise. We
                        // replace its usages with the caller environment.
                        BB::Instrs::iterator promenv;
                        BB* promenvBB = nullptr;
                        Visitor::run(prom_copy, [&](BB* bb) {
#ifndef ENABLE_SLOWASSERT
                            if (promenvBB)
                                return;
#endif
                            for (auto it = bb->begin(); it != bb->end(); ++it) {
                                if (LdFunctionEnv::Cast(*it)) {
                                    assert(!promenvBB);
                                    promenv = it;
                                    promenvBB = (*it)->bb();
                                }
                            }
                        });

                        if (promenvBB) {
                            prom_copy->replaceUsesOfValue(*promenv,
                                                          mkarg->promEnv());
                            promenvBB->remove(promenv);
                        }

                        // Update environment dependency of inlined forces:
                        // the inlined forces can see local env of this
                        // function if it is stored on the context.
                        if (auto mkenv = MkEnv::Cast(f->env())) {
                            if (mkenv->context) {
                                Visitor::run(prom_copy, [&](Instruction* i) {
                                    if (auto fi = Force::Cast(i)) {
                                        if (fi->hasEnv()) {
                                            fi->env(f->env());
                                        }
                                    }
                                });
                            }
                        }

                        // Create a return value phi of the promise
                        auto promRet =
                            BBTransform::forInline(prom_copy, split, f->env());
                        auto promRes = promRet.first;

                        assert(!promRes->type.maybePromiseWrapped());
                        f = Force::Cast(*split->begin());
                        // Ensure we don't loose inferred type information
                        promRes->type = promRes->type & f->type;
                        assert(f);
                        f->replaceUsesWith(promRes);
                        split->remove(split->begin());

                        auto pos = split->begin();
                        MkArg* fixedMkArg =
                            new MkArg(mkarg->prom(), promRes, mkarg->promEnv());
                        pos = split->insert(pos, fixedMkArg);
                        pos++;
                        CastType* upcast = new CastType(
                            fixedMkArg, CastType::Upcast, RType::prom,
                            promRes->type.orPromiseWrapped());
                        pos = split->insert(pos, upcast);
                        pos++;
                        forcedMkArg[mkarg] = {fixedMkArg, upcast};

                        auto u = needsUpdate.find(f);
                        if (u != needsUpdate.end()) {
                            for (auto m : u->second) {
                                m->eachLocalVar([&](SEXP name, Value* a, bool) {
                                    if (a->followCasts() == mkarg) {
                                        pos = split->insert(
                                            pos, new StVar(name, upcast, m,
                                                           PirType::any()));
                                        pos++;
                                    }
                                });
                            }
                        }
                        next = pos;

                        inlinedPromise[f] = promRes;

                        if (promRet.second->isNonLocalReturn())
                            dead.insert(split);
                        break;
                    }
                }
            } else if (auto cast = CastType::Cast(*ip)) {
                // Only replace upcasts, or we loose information
                if (cast->kind == CastType::Upcast) {
                    if (auto mk = MkArg::Cast(cast->arg<0>().val())) {
                        if (mk->isEager()) {
                            anyChange = true;
                            auto eager = mk->eagerArg();
                            auto allowedToReplace = [&](Instruction* i) {
                                if (Force::Cast(i) || LdFun::Cast(i))
                                    return true;
                                int builtinId = -1;
                                if (auto b = CallBuiltin::Cast(i))
                                    builtinId = b->builtinId;
                                if (auto b = CallSafeBuiltin::Cast(i))
                                    builtinId = b->builtinId;
                                if (builtinId) {
                                    if (eager->type.maybeObj())
                                        if (SafeBuiltinsList::always(builtinId))
                                            return true;
                                    if (SafeBuiltinsList::nonObject(builtinId))
                                        return true;
                                }
                                if (i->effects.includes(Effect::LeakArg) ||
                                    i->effects.includes(Effect::Reflection)) {
                                    return false;
                                }
                                // Depromised and promised missing do not behave
                                // the same (promised missing does not cause
                                // default arguments to be used)! So let's only
                                // replace this for instructions which are
                                // stripping the promise anyway.
                                if (eager == MissingArg::instance())
                                    return false;
                                return true;
                            };
                            cast->replaceUsesIn(eager, bb,
                                                [](Instruction*, size_t) {},
                                                allowedToReplace);
                        }
                    }
                }
            }
            ip = next;
        }
    });

    // 2. replace dominated promises
    Visitor::run(code->entry, [&](BB* bb) {
        auto ip = bb->begin();
        while (ip != bb->end()) {
            auto next = ip + 1;
            if (auto f = Force::Cast(*ip)) {
                // If this force instruction is dominated by another force
                // we can replace it with the dominating instruction
                auto dom = dominatedBy.find(f);
                if (dom != dominatedBy.end()) {
                    assert(f != dom->second);
                    if (inlinedPromise.count(dom->second)) {
                        f->replaceUsesWith(inlinedPromise.at(dom->second));
                    } else {
                        dom->second->type = dom->second->type & f->type;
                        f->replaceUsesWith(dom->second);
                    }
                    next = bb->remove(ip);
                }
            }
            ip = next;
        }
    });

    // 3. replace remaining uses of the mkarg itself
    if (!forcedMkArg.empty()) {
        DominanceGraph dom(code);
        for (auto m : forcedMkArg) {
            m.first->replaceDominatedUses(m.second.first, dom,
                                          {Tag::PushContext});
        }
        Visitor::run(code->entry, [&](Instruction* i) {
            if (auto c = CastType::Cast(i)) {
                if (auto m = MkArg::Cast(c->arg(0).val())) {
                    auto r = forcedMkArg.find(m);
                    if (r != forcedMkArg.end()) {
                        auto repl = r->second.second;
                        repl->type = repl->type & c->type;
                        c->replaceDominatedUses(repl, dom);
                        SLOWASSERT(c->usesAreOnly(repl->bb(), {Tag::MkEnv}));
                    }
                }
            }
        });
    }

    // 4. remove BB's that became dead due to non local return
    BBTransform::removeDeadBlocks(code, dead);
    return anyChange;
}

size_t Parameter::PROMISE_INLINER_MAX_SIZE =
    getenv("PIR_PROMISE_INLINER_MAX_SIZE")
        ? atoi(getenv("PIR_PROMISE_INLINER_MAX_SIZE"))
        : 3000;
} // namespace pir
} // namespace rir
