#ifndef PIR_FORCED_BY_H
#define PIR_FORCED_BY_H

#include "../analysis/generic_static_analysis.h"
#include "utils/Map.h"
#include "utils/Set.h"
#include <unordered_set>

namespace rir {
namespace pir {

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
    rir::SmallMap<Value*, Instruction*> forcedBy;
    rir::SmallSet<Value*> inScope;
    std::unordered_map<MkArg*, rir::SmallSet<MkEnv*>> escaped;

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
        auto mk = MkArg::Cast(arg);
        if (!mk)
            return changed;

        auto e = escaped.find(mk);
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
            bool stubbed = !e.second.empty();
            for (auto env : e.second) {
                if (!env->stub)
                    stubbed = false;
            }
            if (stubbed)
                continue;
            auto f = forcedBy.find(e.first);
            if (f == forcedBy.end()) {
                forcedBy.insert(e.first, ambiguous());
                e.second.clear();
                changed = true;
            } else if (!f->second) {
                f->second = ambiguous();
                e.second.clear();
                changed = true;
            }
        }

        return changed;
    }

    bool forcedAt(Value* val, Instruction* force) {
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

            // Don't keep taps on forced proms anymore
            if (auto mk = MkArg::Cast(val)) {
                auto e = escaped.find(mk);
                if (e != escaped.end()) {
                    escaped.erase(e);
                    res = true;
                }
            }

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

    bool escape(MkArg* val, Value* where) {
        auto f = forcedBy.find(val);
        if ((f == forcedBy.end() || !f->second)) {
            auto existing = escaped.find(val);
            if (auto st = StVar::Cast(where))
                where = st->env();
            if (auto mk = MkEnv::Cast(where)) {
                if (existing == escaped.end()) {
                    escaped[val].insert(mk);
                } else if (!existing->second.empty()) { // empty set = ambiguous
                    if (existing->second.count(mk))
                        return false;
                    existing->second.insert(mk);
                }
            } else {
                if (existing != escaped.end() && existing->second.empty())
                    return false;
                escaped[val].clear();
            }
            return true;
        }

        return false;
    }

    AbstractResult mergeExit(const ForcedBy& other) {
        return merge(other, true);
    }

    AbstractResult merge(const ForcedBy& other, bool exitMerge = false) {
        AbstractResult res;

        rir::SmallSet<MkArg*> gotAmbiguous;
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
                    if (auto m = MkArg::Cast(e.first))
                        gotAmbiguous.insert(m);
                    res.lostPrecision();
                }
            } else if (o->second) {
                if (f != o->second) {
                    e.second = ambiguous();
                    if (auto m = MkArg::Cast(e.first))
                        gotAmbiguous.insert(m);
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

        for (auto& e : escaped) {
            if (e.second.empty())
                continue;
            auto o = other.escaped.find(e.first);
            if (o == other.escaped.end() || o->second.empty() ||
                e.second != o->second || gotAmbiguous.count(e.first)) {
                e.second.clear();
                res.update();
            }
        }
        for (auto& o : other.escaped) {
            if (!escaped.count(o.first)) {
                escaped[o.first];
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

    bool isUnused(MkArg* a) const {
        auto force = forcedBy.find(a);
        if (force == forcedBy.end() || !force->second) {
            auto e = escaped.find(a);
            if (e == escaped.end())
                return true;
        }
        return false;
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
        auto e = escaped.find(a);
        if (e == escaped.end())
            return SafeToInline;
        if (e->second.empty())
            return NotSafeToInline;
        return SafeToInlineWithUpdate;
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
            p.first->printRef(out);
            out << " (";
            if (p.second.empty()) {
                out << "ambiguous";
            } else {
                for (auto m : p.second) {
                    m->printRef(out);
                    if (m != *(p.second.end() - 1))
                        out << ",";
                }
            }
            out << ") ";
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

        // 1. Keep track of when a prom is forced
        if (auto f = Force::Cast(i)) {
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
        } else if (auto f = UpdatePromise::Cast(i)) {
            if (auto mk = MkArg::Cast(f->arg<0>().val()->followCasts()))
                if (state.forcedAt(mk, f))
                    res.update();
        }


        // 2. If this instruction can force, taint all escaped, unevaluated
        // proms. Here we (slightly unsoundly) assume that forces do not
        // reflectively force other, unrelated promises.
        if (i->effects.contains(Effect::Force) && !LdFun::Cast(i) &&
            !Force::Cast(i)) {
            if (state.sideeffect()) {
                res.taint();
            }
        }

        // 3. If this instruction accesses an environment, taint all escape
        // information, because after a random env access we cannot rely on the
        // information where the promises escaped to.
        if (i->effects.includes(Effect::ReadsEnv) && !Deopt::Cast(i)) {
            // StVar overriding promise means the promise does not count as
            // escaped anymore
            bool handledEscapeTaint = false;
            if (auto st = StVar::Cast(i)) {
                if (auto mk = MkEnv::Cast(st->env())) {
                    mk->eachLocalVar([&](SEXP name, Value* a, bool) {
                        if (name != st->varName)
                            return;
                        if (auto mka = MkArg::Cast(a->followCasts())) {
                            handledEscapeTaint = true;
                            auto e = state.escaped.find(mka);
                            if (e == state.escaped.end())
                                return;
                            state.escaped.erase(e);
                            res.update();
                        }
                    });
                }
            } else if (auto ld = LdVar::Cast(i)) {
                if (auto mk = MkEnv::Cast(ld->env())) {
                    mk->eachLocalVar([&](SEXP name, Value* a, bool) {
                        if (name != ld->varName)
                            return;
                        // Accessing an escaped promise, leaks it further and we
                        // need abandon escape analysis...
                        if (auto mka = MkArg::Cast(a->followCasts())) {
                            handledEscapeTaint = true;
                            auto e = state.escaped.find(mka);
                            if (e == state.escaped.end())
                                return;
                            if (e->second.empty())
                                return;
                            res.update();
                            e->second.clear();
                        }
                    });
                    if (!handledEscapeTaint) {
                        // Access goes past local envs, no need to worry
                        // TODO: maybe do this recursive?
                        if (!MkEnv::Cast(mk->lexicalEnv()))
                            handledEscapeTaint = true;
                    }
                }
            } else if (IsEnvStub::Cast(i) || MkEnv::Cast(i) ||
                       FrameState::Cast(i) || Deopt::Cast(i)) {
                // These instructions do not leak the env further
                handledEscapeTaint = true;
            }

            // In case there is an environment access we loose track of which
            // proms are escaped to where.
            if (!handledEscapeTaint) {
                for (auto& e : state.escaped) {
                    if (!e.second.empty()) {
                        res.taint();
                        e.second.clear();
                    }
                }
            }
        }

        // 3. Figure out where promises escape to
        std::function<void(Instruction*)> traceEscapes = [&](Instruction* i) {
            if (!i->effects.includes(Effect::LeakArg) && !MkEnv::Cast(i))
                return;
            i->eachArg([&](Value* v) {
                if (auto m = MkArg::Cast(v->followCasts()))
                    if (state.escape(m, i))
                        res.update();
                if (auto fs = FrameState::Cast(v->followCasts()))
                    traceEscapes(fs);
            });
        };
        if (auto phi = Phi::Cast(i)) {
            if (phi->type.maybeLazy()) {
                if (state.forcedBy.count(phi) == 0 && state.declare(phi)) {
                    res.update();
                }
            }
            traceEscapes(i);
        } else if (auto mk = MkArg::Cast(i)) {
            if (state.declare(mk))
                res.update();
        } else if (CastType::Cast(i) || FrameState::Cast(i)) {
            // Do nothing...
            // Pushcontext captures the arglist, which contains the
            // originally passed arguments. These must not be
            // updated!
            // FrameState is handled when used (see traceEscapes)
        } else {
            if (i->type.maybeLazy()) {
                if (state.declare(i))
                    res.update();
            }
            traceEscapes(i);
        }

        // 4. Side analysis: check if force order gets tainted
        if (i->effects.includes(Effect::Force) && !state.ambiguousForceOrder &&
            state.argumentForceOrder.size() < closure->effectiveNArgs()) {
            // After the first effect we give up on recording force order,
            // since we can't use it to turn the arguments into eager ones
            // anyway. Otherwise we would reorder effects.
            state.ambiguousForceOrder = true;
            res.taint();
        }
        return res;
    }
};

} // namespace pir
} // namespace rir

#endif
