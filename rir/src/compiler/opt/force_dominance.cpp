#include "force_dominance.h"
#include "../analysis/generic_static_analysis.h"
#include "../pir/pir_impl.h"
#include "../transform/bb.h"
#include "../transform/replace.h"

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

struct ForcedBy : public std::unordered_map<Value*, Force*> {
    static Force* ambiguous() {
        static Force f(nullptr);
        return &f;
    }

    void forcedAt(MkArg* val, Force* force) {
        if (!count(val))
            (*this)[val] = force;
    }
    void usedAt(MkArg* val, Force* force) {
        // Used before force, this means we need to keep the promise as is
        if (!count(val))
            (*this)[val] = ambiguous();
    }
    bool merge(ForcedBy& other) {
        bool changed = false;
        for (auto& e : *this) {
            auto v = e.first;
            auto f = e.second;
            if (f != ambiguous() && (!other.count(v) || f != other.at(v))) {
                e.second = ambiguous();
                changed = true;
            }
        }
        for (auto& e : other) {
            if (!count(e.first)) {
                (*this)[e.first] = ambiguous();
                changed = true;
            }
        }
        return changed;
    }
};

static Value* getValue(Force* f) {
    Value* cur = f;
    while (true) {
        if (Force::Cast(cur)) {
            cur = Force::Cast(cur)->arg<0>().val();
        } else if (CastType::Cast(cur)) {
            cur = CastType::Cast(cur)->arg<0>().val();
        } else {
            break;
        }
    }
    return cur;
}

class ForceDominanceAnalysis : public StaticAnalysis<ForcedBy> {
  public:
    ForceDominanceAnalysis(Closure* cls) : StaticAnalysis(cls) {}

    void apply(ForcedBy& d, Instruction* i) const override {
        auto f = Force::Cast(i);
        if (f) {
            MkArg* arg = MkArg::Cast(getValue(f));
            if (arg)
                d.forcedAt(arg, f);
        } else if (!CastType::Cast(i)) {
            i->eachArg([&](Value* v) {
                MkArg* arg = MkArg::Cast(v);
                if (arg)
                    d.usedAt(arg, f);
            });
        }
    }
};

class ForceDominanceAnalysisResult {
  public:
    ForceDominanceAnalysisResult(Closure* cls) {
        ForceDominanceAnalysis analysis(cls);
        analysis();
        analysis.foreach<PositioningStyle::AfterInstruction>(
            [&](const ForcedBy& p, Instruction* i) {
                auto f = Force::Cast(i);
                if (f) {
                    if (p.find(getValue(f)) != p.end()) {
                        auto o = p.at(getValue(f));
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
        return exit.count(a) && exit.at(a) != ForcedBy::ambiguous();
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
};
}

namespace rir {
namespace pir {

void ForceDominance::apply(Closure* cls) {
    ForceDominanceAnalysisResult analysis(cls);

    std::unordered_map<Force*, Value*> inlinedPromise;
    std::unordered_map<Instruction*, MkArg*> forcedMkArg;

    // 1. Inline dominating promises
    Visitor::run(cls->entry, [&](BB* bb) {
        auto ip = bb->begin();
        while (ip != bb->end()) {
            auto f = Force::Cast(*ip);
            auto next = ip + 1;
            if (f) {
                auto mkarg = MkArg::Cast(getValue(f));
                if (mkarg) {
                    if (analysis.isDominating(f)) {
                        Value* strict = mkarg->eagerArg();
                        if (strict != Missing::instance()) {
                            f->replaceUsesWith(strict);
                            next = bb->remove(ip);
                            inlinedPromise[f] = strict;
                        } else if (analysis.isSafeToInline(mkarg)) {
                            Promise* prom = mkarg->prom;
                            BB* split = BBTransform::split(cls->nextBBId++, bb,
                                                           ip, cls);
                            BB* prom_copy =
                                BBTransform::clone(prom->entry, cls);
                            bb->next0 = prom_copy;

                            // For now we assume every promise starts with a
                            // LdFunctionEnv instruction. We replace it's usages
                            // with the caller environment.
                            LdFunctionEnv* e =
                                LdFunctionEnv::Cast(*prom_copy->begin());
                            assert(e);
                            Replace::usesOfValue(prom_copy, e, mkarg->env());
                            prom_copy->remove(prom_copy->begin());

                            // Create a return value phi of the promise
                            Value* promRes =
                                BBTransform::forInline(prom_copy, split);

                            f = Force::Cast(*split->begin());
                            assert(f);
                            f->replaceUsesWith(promRes);
                            split->remove(split->begin());

                            MkArg* fixedMkArg =
                                new MkArg(mkarg->prom, promRes, mkarg->env());
                            next = split->insert(split->begin(), fixedMkArg);
                            forcedMkArg[mkarg] = fixedMkArg;

                            inlinedPromise[f] = promRes;
                            bb = split;
                        }
                    }
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
}
}
