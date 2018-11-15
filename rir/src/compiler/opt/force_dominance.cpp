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

    static Force* ambiguous() {
        static Force f(Nil::instance(), Env::nil());
        return &f;
    }

    void declare(MkArg* arg) { declared.insert(arg); }
    void forcedAt(MkArg* val, Force* force) {
        if (!forcedBy.count(val))
            forcedBy[val] = force;
    }
    void used(MkArg* val) {
        // Used before force, this means we need to keep the promise as is
        if (!forcedBy.count(val))
            forcedBy[val] = ambiguous();
    }
    bool merge(const ForcedBy& other) {
        bool changed = false;
        for (auto& e : forcedBy) {
            auto v = e.first;
            auto f = e.second;
            if (other.forcedBy.count(v)) {
                if (f != other.forcedBy.at(v)) {
                    e.second = ambiguous();
                    changed = true;
                }
            } else {
                if (other.declared.count(v)) {
                    e.second = ambiguous();
                    changed = true;
                }
            }
        }
        for (auto& e : other.forcedBy) {
            if (!forcedBy.count(e.first) && declared.count(e.first)) {
                forcedBy[e.first] = ambiguous();
                changed = true;
            }
        }
        return changed;
    }
    void print(std::ostream& out, bool tty) {
        out << "Known proms: ";
        for (auto& p : declared) {
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
    explicit ForceDominanceAnalysis(Closure* cls, LogStream& log,
                                    DebugLevel debug = DebugLevel::None)
        : StaticAnalysis("ForceDominance", cls, log, debug) {}

    void apply(ForcedBy& d, Instruction* i) const override {
        if (auto f = Force::Cast(i)) {
            if (MkArg* arg = MkArg::Cast(i->baseValue()))
                d.forcedAt(arg, f);
        } else if (auto mk = MkArg::Cast(i)) {
            d.declare(mk);
        } else if (!CastType::Cast(i)) {
            i->eachArg([&](Value* v) {
                if (auto arg = MkArg::Cast(v->baseValue()))
                    d.used(arg);
            });
        }
    }
};

class ForceDominanceAnalysisResult {
  public:
    explicit ForceDominanceAnalysisResult(Closure* cls, LogStream& log) {
        ForceDominanceAnalysis analysis(
            cls, log /* , ForceDominanceAnalysis::DebugLevel::BB */);
        analysis();
        analysis.foreach<PositioningStyle::AfterInstruction>(
            [&](const ForcedBy& p, Instruction* i) {
                if (auto f = Force::Cast(i)) {
                    auto base = f->baseValue();
                    if (p.forcedBy.count(base)) {
                        auto o = p.forcedBy.at(base);
                        if (o != ForcedBy::ambiguous()) {
                            if (f != o)
                                domBy[f] = o;
                            else
                                dom.insert(f);
                        }
                    }
                }
            });
        exit = std::move(analysis.result());
    }

    bool isSafeToInline(MkArg* a) {
        // To inline promises with a deopt instruction we need to be able to
        // synthesize promises and promise call framse.
        auto prom = a->prom();
        if (hasDeopt.count(prom)) {
            if (hasDeopt.at(prom))
                return false;
        } else {
            auto deopt = !Query::noDeopt(prom);
            hasDeopt[prom] = deopt;
            if (deopt)
                return false;
        }
        return true;
    }
    bool isDominating(Force* f) { return dom.find(f) != dom.end(); }
    void mapToDominator(Force* f, std::function<void(Force* f)> action) {
        if (domBy.count(f))
            action(domBy.at(f));
    }

  private:
    ForcedBy exit;
    std::unordered_map<Force*, Force*> domBy;
    std::unordered_set<Force*> dom;
    std::unordered_map<Promise*, bool> hasDeopt;
};
} // namespace

namespace rir {
namespace pir {

void ForceDominance::apply(RirCompiler&, Closure* cls, LogStream& log) const {
    ForceDominanceAnalysisResult analysis(cls, log);

    std::unordered_map<Force*, Value*> inlinedPromise;
    std::unordered_map<Instruction*, MkArg*> forcedMkArg;

    // 1. Inline dominating promises
    Visitor::run(cls->entry, [&](BB* bb) {
        auto ip = bb->begin();
        while (ip != bb->end()) {
            auto next = ip + 1;
            if (auto f = Force::Cast(*ip)) {
                if (auto mkarg = MkArg::Cast(f->baseValue())) {
                    if (analysis.isDominating(f)) {
                        Value* strict = mkarg->eagerArg();
                        if (strict != Missing::instance()) {
                            f->replaceUsesWith(strict);
                            next = bb->remove(ip);
                            inlinedPromise[f] = strict;
                        } else if (analysis.isSafeToInline(mkarg)) {
                            Promise* prom = mkarg->prom();
                            BB* split = BBTransform::split(cls->nextBBId++, bb,
                                                           ip, cls);
                            BB* prom_copy =
                                BBTransform::clone(prom->entry, cls);
                            bb->overrideNext(prom_copy);

                            // For now we assume every promise starts with a
                            // LdFunctionEnv instruction. We replace it's usages
                            // with the caller environment.
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
                            next = split->insert(split->begin(), fixedMkArg);
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
    Visitor::run(cls->entry, [&](BB* bb) {
        auto ip = bb->begin();
        while (ip != bb->end()) {
            auto f = Force::Cast(*ip);
            auto cast = CastType::Cast(*ip);
            auto next = ip + 1;
            if (f) {
                // If this force instruction is dominated by another force we
                // can replace it with the dominating instruction
                analysis.mapToDominator(f, [&](Force* r) {
                    if (inlinedPromise.count(r))
                        f->replaceUsesWith(inlinedPromise.at(r));
                    else
                        f->replaceUsesWith(r);
                    next = bb->remove(ip);
                });
            }
            if (cast) {
                // Collect aliases of promises for step 3 bellow
                auto in = Instruction::Cast(cast->arg<0>().val());
                if (in && forcedMkArg.count(in)) {
                    forcedMkArg[cast] = forcedMkArg.at(in);
                }
            }
            ip = next;
        }
    });

    // 3. replace remaining uses of the mkarg itself
    for (auto m : forcedMkArg)
        m.first->replaceUsesIn(m.second, m.second->bb());
}
} // namespace pir
} // namespace rir
