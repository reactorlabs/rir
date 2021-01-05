#include "../analysis/generic_static_analysis.h"
#include "../analysis/query.h"
#include "../parameter.h"
#include "../pir/pir_impl.h"
#include "compiler/util/bb_transform.h"
#include "pass_definitions.h"
#include "utils/Map.h"
#include "utils/Set.h"

namespace {

using namespace rir::pir;

/* This optimization removes redundant force instructions:
 *
 * b = force(a)
 * c = force(b)
 *
 * For that we need to compute a dominance graph of forces.
 *
 * Additionally, if we know the promise being forced, we try to inline it. For
 * example:
 *
 * a = mkArg(prom(0))
 * b = force(a)
 *
 * will be translated to:
 *
 * b = <inlined prom(0)>
 *
 * But, in the case of promises with side-effects we can only inline them iff
 * there is a unique dominating force instruction.
 *
 * For example in the following case:
 *
 *      Branch
 *   /          \
 * force         |
 *   \         /
 *     \     /
 *        |
 *      force
 *
 * we don't know at the second force if the promise was forced (by the left
 * branch) or not. Thus we cannot inline it.
 */

struct ForcedBy {
    rir::SmallMap<Value*, Force*> forcedBy;
    rir::SmallSet<Value*> inScope;
    rir::SmallSet<Value*> escaped;

    std::vector<size_t> argumentForceOrder;
    bool ambiguousForceOrder = false;

    static Force* ambiguous() {
        static Force f(Nil::instance(), Env::nil(), Tombstone::framestate());
        return &f;
    }

    bool declare(Value* arg) {
        bool changed = false;
        if (!inScope.count(arg)) {
            inScope.insert(arg);
            changed = true;
        }
        auto f = forcedBy.find(arg);
        if (f != forcedBy.end() && f->second) {
            f->second = nullptr;
            changed = true;
        }
        auto e = escaped.find(arg);
        if (e != escaped.end()) {
            escaped.erase(e);
            changed = true;
        }
        return changed;
    }

    bool sideeffect() {
        bool changed = false;
        // when we execute an instruction that could force promises as a
        // sideeffect, we have to assume that all escaped promises might have
        // been forced

        for (auto& e : escaped) {
            auto f = forcedBy.find(e);
            if (f == forcedBy.end()) {
                forcedBy.insert(e, ambiguous());
                changed = true;
            } else if (!f->second) {
                f->second = ambiguous();
                changed = true;
            }
        }

        return changed;
    }

    bool forcedAt(Value* val, Force* force) {
        rir::SmallSet<Phi*> seen;
        std::function<bool(Value*, bool)> apply = [&](Value* val, bool phiArg) {
            bool res = false;
            auto p = Phi::Cast(val);
            if (p) {
                if (seen.includes(p))
                    return false;
                seen.insert(p);
                p->eachArg([&](BB*, Value* v) { res = apply(v, true) || res; });
            }
            if (phiArg)
                force = ambiguous();
            auto f = forcedBy.find(val);
            if (f == forcedBy.end()) {
                forcedBy.insert(val, force);
                return true;
            } else if (!f->second) {
                f->second = force;
                return true;
            }
            return res;
        };
        return apply(val, false);
    }

    bool escape(Value* val) {
        auto f = forcedBy.find(val);
        if ((f == forcedBy.end() || !f->second) && !escaped.count(val)) {
            escaped.insert(val);
            return true;
        }

        return false;
    }

    AbstractResult mergeExit(const ForcedBy& other) {
        return merge(other, true);
    }

    AbstractResult merge(const ForcedBy& other, bool exitMerge = false) {
        AbstractResult res;

        for (auto& e : forcedBy) {
            if (!e.second)
                continue;
            auto v = e.first;
            auto f = e.second;
            if (f == ambiguous())
                continue;
            auto o = other.forcedBy.find(v);
            if (o == other.forcedBy.end() || !o->second) {
                if (!exitMerge && other.inScope.count(v)) {
                    e.second = ambiguous();
                    res.lostPrecision();
                }
            } else if (o->second) {
                if (f != o->second) {
                    e.second = ambiguous();
                    res.lostPrecision();
                }
            }
        }
        for (auto& e : other.forcedBy) {
            auto v = e.first;
            auto o = forcedBy.find(v);
            if (o == forcedBy.end() || !o->second) {
                auto m = exitMerge ? e.second : ambiguous();
                if (exitMerge || inScope.count(v)) {
                    if (o == forcedBy.end())
                        forcedBy.insert(v, m);
                    else
                        o->second = m;
                    res.lostPrecision();
                }
            }
        }

        for (auto& e : other.escaped) {
            if (!escaped.count(e)) {
                escaped.insert(e);
                res.update();
            }
        }

        if (!ambiguousForceOrder && other.ambiguousForceOrder) {
            ambiguousForceOrder = true;
            res.update();
        }

        if (argumentForceOrder != other.argumentForceOrder) {
            auto mySize = argumentForceOrder.size();
            auto otherSize = other.argumentForceOrder.size();
            auto common = mySize;

            if (mySize > otherSize) {
                argumentForceOrder.resize(otherSize);
                ambiguousForceOrder = true;
                common = otherSize;
                res.update();
            } else if (!ambiguousForceOrder && otherSize > mySize) {
                ambiguousForceOrder = true;
                res.update();
            }

            for (size_t i = 0; i < common; ++i) {
                if (argumentForceOrder[i] != other.argumentForceOrder[i]) {
                    argumentForceOrder.resize(i);
                    ambiguousForceOrder = true;
                    res.update();
                    break;
                }
            }
        }

        return res;
    }

    bool maybeForced(size_t i) const {
        // Scan the list of unambiguously forced arguments to see if we know if
        // this one was forced
        for (auto f : argumentForceOrder) {
            if (f == i)
                return true;
        }
        return ambiguousForceOrder;
    }

    bool eagerLikeFunction(ClosureVersion* fun) const {
        if (ambiguousForceOrder ||
            argumentForceOrder.size() < fun->effectiveNArgs())
            return false;
        for (size_t i = 0; i < fun->effectiveNArgs(); ++i)
            if (argumentForceOrder[i] != i)
                return false;
        return true;
    }

    bool isDominatingForce(Force* f) const {
        return f == getDominatingForce(f);
    }

    Force* getDominatingForce(Force* f) const {
        auto a = f->arg<0>().val()->followCasts();
        auto res = forcedBy.find(a);
        if (res == forcedBy.end())
            return nullptr;
        if (res->second == ambiguous()) {
            return nullptr;
        }
        return res->second;
    }

    enum PromiseInlineable {
        SafeToInline,
        SafeToInlineWithUpdate,
        NotSafeToInline
    };

    PromiseInlineable isSafeToInline(MkArg* a, Force* f) const {
        return escaped.count(a) ? SafeToInlineWithUpdate : SafeToInline;
    }

    void print(std::ostream& out, bool tty) const {
        out << "Known proms: ";
        for (auto& p : inScope) {
            p->printRef(out);
            out << " ";
        }
        out << "\n";
        out << "Escaped proms: ";
        for (auto& p : escaped) {
            p->printRef(out);
            out << " ";
        }
        out << "\n";
        for (auto& e : forcedBy) {
            if (!e.second)
                continue;
            e.first->printRef(out);
            if (e.second == ambiguous()) {
                out << " force is ambiguous\n";
            } else {
                out << " is forced by ";
                e.second->printRef(out);
                out << "\n";
            }
        }
    }
};

//, DummyState, true, AnalysisDebugLevel::Taint
class ForceDominanceAnalysis : public StaticAnalysis<ForcedBy> {
  public:
    using StaticAnalysis::PositioningStyle;
    const CFG cfg;
    explicit ForceDominanceAnalysis(ClosureVersion* cls, Code* code,
                                    LogStream& log)
        : StaticAnalysis("ForceDominance", cls, code, log), cfg(code) {}

    AbstractResult apply(ForcedBy& state, Instruction* i) const override {
        AbstractResult res;
        auto apply = [&](Instruction* i) {
            if (!i->effects.includes(Effect::LeakArg))
                return;
            auto pc = PushContext::Cast(i);
            i->eachArg([&](Value* v_) {
                auto v = v_->followCasts();
                auto instruction = Instruction::Cast(v);
                if (MkArg::Cast(v) || LdArg::Cast(v) ||
                    (instruction && instruction->type.maybeLazy())) {
                    // Pushcontext captures the arglist, which contains the
                    // originally passed arguments. These must not be
                    // updated!
                    if (pc && pc->ast() != v_ && pc->op() != v_)
                        return;
                    if (state.escape(instruction))
                        res.update();
                }
            });
        };
        if (auto phi = Phi::Cast(i)) {
            if (phi->type.maybeLazy()) {
                if (state.forcedBy.count(phi) == 0 && state.declare(phi)) {
                    res.update();
                }
            }
            apply(i);
        } else if (auto f = Force::Cast(i)) {
            if (LdArg* arg = LdArg::Cast(f->arg<0>().val()->followCasts())) {
                if (arg->type.maybeLazy()) {
                    if (state.forcedAt(arg, f))
                        res.update();
                }
                if (!state.ambiguousForceOrder && !state.maybeForced(arg->id)) {
                    state.argumentForceOrder.push_back(arg->id);
                    res.update();
                }
            } else {
                auto instruction =
                    Instruction::Cast(f->arg<0>().val()->followCasts());
                if (MkArg::Cast(f->arg<0>().val()->followCasts()) ||
                    (instruction && instruction->type.maybeLazy())) {
                    if (state.forcedAt(instruction, f))
                        res.update();
                }
            }
        } else if (auto mk = MkArg::Cast(i)) {
            if (state.declare(mk))
                res.update();
        } else if (auto e = MkEnv::Cast(i)) {
            if (!e->stub)
                apply(e);
        } else if (CastType::Cast(i) || Deopt::Cast(i)) { /* do nothing */ 
        } else {
            if (i->type.maybeLazy()) {
                if (state.declare(i))
                    res.update();
            }
            apply(i);

            if (i->effects.contains(Effect::Force)) {
                if (state.sideeffect())
                    res.taint();
            }

            if (i->effects.includes(Effect::Force) &&
                !state.ambiguousForceOrder &&
                state.argumentForceOrder.size() < closure->effectiveNArgs()) {
                // After the first effect we give up on recording force order,
                // since we can't use it to turn the arguments into eager ones
                // anyway. Otherwise we would reorder effects.
                state.ambiguousForceOrder = true;
                res.taint();
            }
        }
        return res;
    }
};

} // namespace

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
 *    a'  = MkArg(exp)
 *    e   = MkEnv(x=a)      // leak a
 *    f'  = eval(exp)       // inlinee
 *    a'' = MkArg(exp, a)   // synthesized updated promise
 *    UpdatePromise(a, f')  // update to ensure leaked a is correct
 *    PushContext(a')       // push context needs unmodified a
 *    use(a'')              // normal uses get the synthesized version
 *    use(f')               // uses of the value get the result
 *
 *  updatePromise and the duplication of mkarg only happens if needed.
 *
 */

bool ForceDominance::apply(Compiler&, ClosureVersion* cls, Code* code,
                           LogStream& log) const {
    SmallSet<Force*> toInline;
    SmallSet<Force*> needsUpdate;
    SmallMap<Force*, Force*> dominatedBy;
    bool anyChange = false;

    bool isHuge = code->size() > Parameter::PROMISE_INLINER_MAX_SIZE;
    {
        ForceDominanceAnalysis analysis(cls, code, log);
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
                    auto a = analysis.resultIgnoringUnreachableExits(
                        i, analysis.cfg);
                    if (a.isDominatingForce(f)) {
                        f->strict = true;
                        if (auto mk = MkArg::Cast(f->followCastsAndForce())) {
                            if (!mk->isEager()) {
                                if (!isHuge || mk->prom()->size() < 10) {
                                    auto b = analysis.before(i);
                                    auto inl = b.isSafeToInline(mk, f);
                                    if (inl == ForcedBy::SafeToInline) {
                                        toInline.insert(f);
                                        // if (inl ==
                                        //     ForcedBy::SafeToInlineWithUpdate)
                                        //     needsUpdate.insert(f);
                                    }
                                }
                            }
                        }
                    } else if (auto dom = a.getDominatingForce(f)) {
                        if (f != dom)
                            dominatedBy[f] = dom;
                    }
                } else if (auto u = UpdatePromise::Cast(i)) {
                    if (auto mkarg = MkArg::Cast(u->arg(0).val())) {
                        if (!analysis.before(i).escaped.count(mkarg))
                            next = bb->remove(ip);
                    }
                }
                ip = next;
            }
        });
    }

    std::unordered_map<Force*, Value*> inlinedPromise;
    std::unordered_map<Instruction*, MkArg*> forcedMkArg;
    std::unordered_set<BB*> dead;
    std::unordered_set<MkArg*> updated;

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

                        MkArg* fixedMkArg =
                            new MkArg(mkarg->prom(), promRes, mkarg->promEnv());
                        next = split->insert(split->begin(), fixedMkArg);
                        forcedMkArg[mkarg] = fixedMkArg;

                        inlinedPromise[f] = promRes;
                        if (needsUpdate.count(f)) {
                            next = split->insert(
                                next, new UpdatePromise(mkarg, promRes));
                            updated.insert(mkarg);
                        }

                        if (promRet.second->isNonLocalReturn())
                            dead.insert(split);
                        break;
                    }
                }
            } else if (auto cast = CastType::Cast(*ip)) {
                if (auto mk = MkArg::Cast(cast->arg<0>().val())) {
                    if (mk->isEager()) {
                        anyChange = true;
                        auto eager = mk->eagerArg();
                        auto nonReflective = [](Instruction* i) {
                            return !i->effects.includes(Effect::Reflection);
                        };
                        cast->replaceUsesIn(eager, bb,
                                            [](Instruction*, size_t) {},
                                            nonReflective);
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
            } else if (auto p = PushContext::Cast(*ip)) {
                // Promargs need unchanged argument promise. If we updated a
                // promise we need to duplicate it.
                p->eachArg([&](InstrArg& arg) {
                    if (auto a = MkArg::Cast(arg.val())) {
                        if (updated.count(a)) {
                            auto n = MkArg::Cast(a->clone());
                            // This is to prevent GVN from collapsing the two
                            // promises again
                            n->usedInPromargsList = true;
                            ip = bb->insert(ip, n) + 1;
                            arg.val() = n;
                            next = ip + 1;
                        }
                    }
                });
            }
            ip = next;
        }
    });

    // 3. replace remaining uses of the mkarg itself
    for (auto m : forcedMkArg) {
        m.first->replaceDominatedUses(m.second, {Tag::PushContext});
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
