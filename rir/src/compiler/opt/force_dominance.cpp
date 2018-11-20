#include "../analysis/generic_static_analysis.h"
#include "../analysis/query.h"
#include "../pir/pir_impl.h"
#include "../transform/bb.h"
#include "../transform/replace.h"
#include "pass_definitions.h"

namespace {

using namespace rir::pir;

/* This optimization removes redundant force instructions:
 *
 * b = force(a)
 * c = force(b)
 *
 * For that we need to compute a dominance graph of forces.
 *
 * Additionally, if know the promise being forced, we try to inline it. For
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
    std::unordered_map<Value*, Force*> forcedBy;
    std::unordered_set<Value*> declared;
    std::unordered_set<Value*> escaped;

    static Force* ambiguous() {
        static Force f(Nil::instance(), Env::nil());
        return &f;
    }

    bool declare(Value* arg) {
        if (!declared.count(arg)) {
            declared.insert(arg);
            return true;
        }
        return false;
    }

    bool sideeffect() {
        bool changed = false;
        // when we execute an instruction that could force promises as a
        // sideeffect, we have to assume that all escaped promises might have
        // been forced
        for (auto& e : escaped)
            if (!forcedBy.count(e)) {
                forcedBy[e] = ambiguous();
                changed = true;
            }
        return changed;
    }

    bool forcedAt(Value* val, Force* force) {
        if (!forcedBy.count(val)) {
            forcedBy[val] = force;
            return true;
        }
        return false;
    }

    bool escape(Value* val) {
        if (!escaped.count(val)) {
            escaped.insert(val);
            return true;
        }
        return false;
    }

    AbstractResult merge(const ForcedBy& other) {
        AbstractResult res;

        for (auto& e : forcedBy) {
            auto v = e.first;
            auto f = e.second;
            if (other.forcedBy.count(v)) {
                if (f != other.forcedBy.at(v)) {
                    if (e.second != ambiguous()) {
                        e.second = ambiguous();
                        res.lostPrecision();
                    }
                }
            } else if (other.declared.count(v)) {
                if (e.second != ambiguous()) {
                    e.second = ambiguous();
                    res.lostPrecision();
                }
            }
        }
        for (auto& e : other.forcedBy) {
            if (!forcedBy.count(e.first)) {
                if (declared.count(e.first)) {
                    forcedBy[e.first] = ambiguous();
                    res.lostPrecision();
                } else {
                    declared.insert(e.first);
                    forcedBy[e.first] = e.second;
                    res.update();
                }
            }
        }
        for (auto& e : other.escaped) {
            if (!escaped.count(e)) {
                escaped.insert(e);
                res.update();
            }
        }
        return res;
    }

    bool isDominatingForce(Force* f) const {
        return f == getDominatingForce(f);
    }

    Force* getDominatingForce(Force* f) const {
        auto a = f->arg<0>().val()->followCasts();
        if (!forcedBy.count(a))
            return nullptr;
        auto res = forcedBy.at(a);
        if (res == ambiguous())
            return nullptr;
        return res;
    }

    bool isSafeToInline(MkArg* a) const {
        // To inline promises with a deopt instruction we need to be able to
        // synthesize promises and promise call framse.
        auto prom = a->prom();
        if (hasDeopt.count(prom)) {
            if (hasDeopt.at(prom))
                return false;
        } else {
            auto deopt = !Query::noDeopt(prom);
            const_cast<ForcedBy*>(this)->hasDeopt[prom] = deopt;
            if (deopt)
                return false;
        }
        // We cannot inline escaped promises, since we have currently no way of
        // updating the promise value slot
        return !escaped.count(a);
    }
    std::unordered_map<Promise*, bool> hasDeopt;

    void print(std::ostream& out, bool tty) {
        out << "Known proms: ";
        for (auto& p : declared) {
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

class ForceDominanceAnalysis : public StaticAnalysis<ForcedBy> {
  public:
    explicit ForceDominanceAnalysis(Closure* cls, Code* code, LogStream& log,
                                    DebugLevel debug = DebugLevel::None)
        : StaticAnalysis("ForceDominance", cls, code, log, debug) {}

    AbstractResult apply(ForcedBy& d, Instruction* i) const override {
        bool changed = false;
        if (auto f = Force::Cast(i)) {
            if (MkArg* arg = MkArg::Cast(f->arg<0>().val()->followCasts()))
                changed = d.forcedAt(arg, f) || changed;
            if (LdArg* arg = LdArg::Cast(f->arg<0>().val()->followCasts()))
                changed = d.forcedAt(arg, f) || changed;
        } else if (auto mk = MkArg::Cast(i)) {
            changed = d.declare(mk) || changed;
        } else if (auto ld = LdArg::Cast(i)) {
            changed = d.declare(ld) || changed;
        } else if (!CastType::Cast(i)) {
            i->eachArg([&](Value* v) {
                v = v->followCasts();
                if (auto arg = MkArg::Cast(v))
                    changed = d.escape(arg) || changed;
                if (auto arg = LdArg::Cast(v))
                    changed = d.escape(arg) || changed;
            });
            if (i->mayForcePromises())
                changed = d.sideeffect() || changed;
        }
        return changed ? AbstractResult::Updated : AbstractResult::None;
    }
};

} // namespace

namespace rir {
namespace pir {

void ForceDominance::apply(RirCompiler&, Closure* cls, LogStream& log) const {
    auto apply = [&](Code* code) {
        ForceDominanceAnalysis analysis(
            cls, code,
            log /* , ForceDominanceAnalysis::DebugLevel::Instruction */);
        analysis();
        auto& result = analysis.result();

        std::unordered_map<Force*, Value*> inlinedPromise;
        std::unordered_map<Instruction*, MkArg*> forcedMkArg;

        // 1. Inline dominating promises
        Visitor::run(code->entry, [&](BB* bb) {
            auto ip = bb->begin();
            while (ip != bb->end()) {
                auto next = ip + 1;
                if (auto f = Force::Cast(*ip)) {
                    if (result.isDominatingForce(f)) {
                        f->strict = true;
                        if (auto mkarg =
                                MkArg::Cast(f->followCastsAndForce())) {
                            Value* strict = mkarg->eagerArg();
                            if (strict != Missing::instance()) {
                                f->replaceUsesWith(strict);
                                next = bb->remove(ip);
                                inlinedPromise[f] = strict;
                            } else if (result.isSafeToInline(mkarg)) {
                                Promise* prom = mkarg->prom();
                                BB* split = BBTransform::split(code->nextBBId++,
                                                               bb, ip, code);
                                BB* prom_copy =
                                    BBTransform::clone(prom->entry, code);
                                bb->overrideNext(prom_copy);

                                // For now we assume every promise starts with a
                                // LdFunctionEnv instruction. We replace it's
                                // usages with the caller environment.
                                LdFunctionEnv* e =
                                    LdFunctionEnv::Cast(*prom_copy->begin());
                                assert(e);
                                Replace::usesOfValue(prom_copy, e,
                                                     mkarg->promEnv());
                                prom_copy->remove(prom_copy->begin());

                                // Create a return value phi of the promise
                                Value* promRes =
                                    BBTransform::forInline(prom_copy, split);

                                f = Force::Cast(*split->begin());
                                assert(f);
                                f->replaceUsesWith(promRes);
                                split->remove(split->begin());

                                MkArg* fixedMkArg = new MkArg(
                                    mkarg->prom(), promRes, mkarg->promEnv());
                                next =
                                    split->insert(split->begin(), fixedMkArg);
                                forcedMkArg[mkarg] = fixedMkArg;

                                inlinedPromise[f] = promRes;
                                bb = split;
                            }
                        }
                    }
                } else if (auto cast = CastType::Cast(*ip)) {
                    if (auto mk = MkArg::Cast(cast->arg<0>().val())) {
                        mk->ifEager([&](Value* val) {
                            cast->replaceUsesWith(val);
                            next = bb->remove(ip);
                        });
                    }
                }
                ip = next;
            }
        });

        // 2. replace dominated promises
        Visitor::run(code->entry, [&](BB* bb) {
            auto ip = bb->begin();
            while (ip != bb->end()) {
                auto f = Force::Cast(*ip);
                auto cast = CastType::Cast(*ip);
                auto next = ip + 1;
                if (f) {
                    // If this force instruction is dominated by another force
                    // we can replace it with the dominating instruction
                    if (auto dom = result.getDominatingForce(f)) {
                        if (f != dom) {
                            if (inlinedPromise.count(dom))
                                f->replaceUsesWith(inlinedPromise.at(dom));
                            else
                                f->replaceUsesWith(dom);
                            next = bb->remove(ip);
                        }
                    }
                }
                if (cast) {
                    // Collect aliases of promises for step 3 bellow
                    auto in = Instruction::Cast(cast->arg<0>().val());
                    if (in && forcedMkArg.count(in))
                        forcedMkArg[cast] = forcedMkArg.at(in);
                }
                ip = next;
            }
        });

        // 3. replace remaining uses of the mkarg itself
        for (auto m : forcedMkArg) {
            m.first->replaceUsesIn(m.second, m.second->bb());
            m.first->eraseAndRemove();
        }
    };
    apply(cls);
    cls->eachPromise([&](Promise* p) { apply(p); });
}
} // namespace pir
} // namespace rir
