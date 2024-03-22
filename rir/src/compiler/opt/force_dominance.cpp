#include "../analysis/force_dominance.h"
#include "../analysis/available_checkpoints.h"
#include "../parameter.h"
#include "../pir/pir_impl.h"
#include "compiler/util/bb_transform.h"
#include "compiler/util/safe_builtins_list.h"
#include "pass_definitions.h"
#include "utils/Map.h"
#include "utils/Set.h"
#include <unordered_map>
#include <unordered_set>

#include <bits/stdc++.h>

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
 *    StVar(x=a', e)        // update to ensure leaked a is fixed.
 *                          // (if the leak isn't just mkenv, use UpdatePromise)
 *    use(a')               // normal uses get the synthesized version
 *    use(f')               // uses of the value get the result
 *
 *  updatePromise and the duplication of mkarg only happens if needed.
 *
 */

bool ForceDominance::apply(Compiler& cmp, ClosureVersion* cls, Code* code,
                           AbstractLog& log, size_t iteration) const {


    bool anyChange = false;

    // Do this first so dead code elimination will remove the dependencies
    // Do not depromise trivial MissingArg promises, as promised and depromised
    // missing do not behave the same (see comment below)
    Visitor::run(code->entry, [&](Instruction* i) {
        // these are their own ast-expression, so substitute and
        // similar will not give us trouble
        if (auto c = CastType::Cast(i)) {
            if (c->kind == CastType::Upcast) {
                if (auto mk = MkArg::Cast(c->arg(0).val())) {
                    if (mk->isEager() && mk->prom()->trivial()) {
                        auto eager = mk->eagerArg();
                        if (eager != MissingArg::instance()) {
                            c->replaceUsesWith(eager,
                                               [&](Instruction*, size_t) {
                                                   anyChange = true;
                                               });
                        }
                    }
                }
            }
        }
        if (auto mk = MkArg::Cast(i)) {
            if (mk->isEager() && mk->prom()->trivial()) {
                auto eager = mk->eagerArg();
                if (eager != MissingArg::instance()) {
                    i->replaceUsesWith(
                        eager,
                        [&](Instruction* j, size_t a) {
                            if (j->arg(a).type().isA(RType::prom)) {
                                j->arg(a).type() = eager->type;
                                anyChange = true;
                            }
                        },
                        [&](Instruction* j) {
                            return j->tag != Tag::CastType;
                        });
                }
            }
        }
    });

    std::unordered_map<Force*, ForcedBy::PromiseInlineable> toInline;
    SmallMap<Force*, Instruction*> dominatingForceForForcee;

    bool isHuge =
        cls->numNonDeoptInstrs() > Parameter::PROMISE_INLINER_MAX_SIZE;
    {
        ForceDominanceAnalysis analysis(cls, code, log);

        auto result = analysis.result();
        if (code == cls) {
            if (result.eagerLikeFunction(cls))
                cls->properties.set(ClosureVersion::Property::IsEager);
            cls->properties.argumentForceOrder = result.argumentForceOrder;
        }

        // Break up as many promise dependencies as possible
        Visitor::run(code->entry, [&](BB* bb) {
            if (!bb->isExit())
                return;

            auto it = bb->begin();
            while (it != bb->end()) {
                auto next = it + 1;
                auto i = *it;
                int argnum = 0;

                i->eachArg([&](InstrArg& arg) {
                    if (auto mk = MkArg::Cast(arg.val())) {

                        auto a = analysis.resultIgnoringUnreachableExits(
                            i, analysis.cfg); // TODO move up?

                        if (a.isUnused(mk)) {

                            auto withNewPromDo =
                                [&](std::function<void(Instruction*)> action) {
                                    auto repl = mk->clone();
                                    action(repl);
                                    arg.val() = repl;
                                    anyChange = true;
                                };

                            if (auto phi = Phi::Cast(i)) {

                                auto inp = phi->inputAt(argnum);
                                assert(inp != bb);

                                if (inp != mk->bb()) {
                                    assert(false && "ForceDominance. Do we "
                                                    "ever hit this case?");

                                    withNewPromDo([&](Instruction* repl) {
                                        inp->append(repl);
                                    });
                                }

                            } else if (mk->bb() != bb) {

                                withNewPromDo([&](Instruction* repl) {
                                    it = bb->insert(it, repl);
                                    it++;
                                });
                            }

                            next = it + 1;
                        }
                    }
                    argnum++;
                });
                it = next;
            }
        });

        VisitorNoDeoptBranch::run(code->entry, [&](BB* bb) {
            auto ip = bb->begin();
            while (ip != bb->end()) {
                auto next = ip + 1;
                auto i = *ip;

                if (auto f = Force::Cast(i)) {
                    auto a = analysis.resultIgnoringUnreachableExits(
                        f, analysis.cfg);
                    if (a.isDominatingForce(f)) {
                        f->strict = true;
                        if (auto mk = MkArg::Cast(f->followCastsAndForce())) {
                            if (!mk->isEager()) {
                                if (!isHuge || mk->prom()->numInstrs() < 10) {
                                    // We need to know if the promise escaped
                                    // before the force. After the force the
                                    // analysis deletes escape information.
                                    auto inl = a.isSafeToInline(mk, f);
                                    if (inl.kind !=
                                        ForcedBy::PromiseInlineable::
                                            NotSafeToInline) {
                                        toInline.emplace(f, inl);
                                    }
                                }
                            }
                        }
                    } else {
                        auto forcee = f->arg<0>().val()->followCasts();
                        if (auto dom = a.getDominatingForce(forcee)) {
                            assert(f != dom &&
                                   "isDominatingForce should hold!");
                            dominatingForceForForcee[f] = dom;
                        }
                    }
                } else if (auto u = UpdatePromise::Cast(i)) {
                    if (auto mkarg = MkArg::Cast(u->arg(0).val())) {
                        auto a = analysis.resultIgnoringUnreachableExits(
                            mkarg, analysis.cfg);
                        if (!a.escaped.count(mkarg)) {
                            next = bb->remove(ip);
                            anyChange = true;
                        }
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
                                    // TODO: don't copy this to start with
                                    if ((*it)->frameState())
                                        (*it)->clearFrameState();
                                    if (FrameState::Cast(*it)) {
                                        next = bb->remove(it);
                                    } else if (auto cp =
                                                   Checkpoint::Cast(*it)) {
                                        auto n = cp->nextBB();
                                        auto d = cp->deoptBranch();
                                        next = bb->remove(it);
                                        bb->overrideSuccessors({n});
                                        if (d->predecessors().size() == 0) {
                                            assert(d->successors().size() == 0);
                                            delete d;
                                        }
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
                        auto promRes = BBTransform::forInline(prom_copy, split,
                                                              nullptr, nullptr);

                        assert(!promRes->type.maybePromiseWrapped());
                        f = Force::Cast(*split->begin());
                        // Ensure we don't lose inferred type information
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
                            promRes->type.orFullyPromiseWrapped());
                        pos = split->insert(pos, upcast);
                        pos++;

                        auto u = toInline.at(f);
                        if (u.kind == ForcedBy::PromiseInlineable::
                                          SafeToInlineWithUpdate) {
                            if (auto m = MkEnv::Cast(u.escapedAt())) {
                                if (!m->bb()->isDeopt())
                                    m->eachLocalVar(
                                        [&](SEXP name, Value* a, bool) {
                                            if (a->followCasts() == mkarg) {
                                                pos = split->insert(
                                                    pos,
                                                    new StArg(name, upcast, m));
                                                pos++;
                                            }
                                        });
                            } else if (PushContext::Cast(u.escapedAt())) {
                                // Escapes to non-environment, let's update
                                // existing promise
                                pos = split->insert(
                                    pos, new UpdatePromise(mkarg, promRes));
                                pos++;
                            } else {
                                assert(false && "Cannot deal with this leak");
                            }
                        }
                        next = pos;

                        inlinedPromise[f] = promRes;

                        if (promRes == Tombstone::unreachable())
                            dead.insert(split);
                        break;
                    }
                }
            } else if (auto cast = CastType::Cast(*ip)) {
                // Only replace upcasts, or we lose information
                if (cast->kind == CastType::Upcast) {
                    if (auto mk = MkArg::Cast(cast->arg<0>().val())) {
                        if (mk->isEager()) {
                            auto eager = mk->eagerArg();
                            auto allowedToReplace = [&](Instruction* i) {
                                if (Force::Cast(i) || LdFun::Cast(i))
                                    return true;
                                int builtinId = -1;
                                if (auto b = CallBuiltin::Cast(i))
                                    builtinId = b->builtinId;
                                if (auto b = CallSafeBuiltin::Cast(i))
                                    builtinId = b->builtinId;
                                if (builtinId != -1) {
                                    if (eager->type.maybeObj(true, "forcedom"))
                                        if (SafeBuiltinsList::always(builtinId))
                                            return true;
                                    if (SafeBuiltinsList::nonObject(builtinId))
                                        return true;
                                }
                                if (i->leaksArg() || i->mayUseReflection()) {
                                    return false;
                                }
                                // Depromised and promised missing do not behave
                                // the same (promised missing does not cause
                                // default arguments to be used)! So let's only
                                // replace this for instructions which are
                                // stripping the promise anyway.
                                if (eager == MissingArg::instance())
                                    return false;
                                if (ChkMissing::Cast(i) &&
                                    eager->type.maybeMissing())
                                    return false;
                                return true;
                            };
                            cast->replaceUsesIn(
                                eager, bb,
                                [&](Instruction*, size_t) { anyChange = true; },
                                allowedToReplace);
                        }
                    }
                }
            }
            ip = next;
        }
    });

    DominanceGraph dom(code);

    // 2. replace dominated promises
    Visitor::run(code->entry, [&](BB* bb) {
        auto ip = bb->begin();
        while (ip != bb->end()) {
            auto next = ip + 1;
            if (auto f = Force::Cast(*ip)) {
                // If this force instruction is dominated by another force
                // we can replace it with the dominating instruction
                auto domF = dominatingForceForForcee.find(f);
                if (domF != dominatingForceForForcee.end() &&
                    dom.dominates(domF->second, f)) {
                    anyChange = true;
                    assert(f != domF->second);
                    if (auto otherForce = Force::Cast(domF->second)) {
                        if (inlinedPromise.count(otherForce)) {
                            f->replaceUsesWith(inlinedPromise.at(otherForce));
                        } else {
                            f->replaceUsesWith(domF->second);
                        }
                        next = bb->remove(ip);
                    } else if (auto otherUpdate =
                                   UpdatePromise::Cast(domF->second)) {
                        f->replaceUsesWith(otherUpdate->arg(1).val());
                        next = bb->remove(ip);
                    }
                }
            }
            ip = next;
        }
    });

    // 3. replace remaining uses of the mkarg itself
    if (!forcedMkArg.empty()) {
        for (auto m : forcedMkArg) {
            anyChange = true;
            m.first->replaceDominatedUses(m.second.first, dom);
        }
        Visitor::run(code->entry, [&](Instruction* i) {
            if (auto c = CastType::Cast(i)) {
                if (auto m = MkArg::Cast(c->arg(0).val())) {
                    auto r = forcedMkArg.find(m);
                    if (r != forcedMkArg.end()) {
                        anyChange = true;
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
        : 1800;
} // namespace pir
} // namespace rir
