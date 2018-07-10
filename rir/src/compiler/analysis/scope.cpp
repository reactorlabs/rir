#include "scope.h"
#include "../pir/pir_impl.h"
#include "query.h"

namespace {
using namespace rir::pir;

class TheScopeAnalysis : public StaticAnalysis<AbstractREnvironmentHierarchy> {
  public:
    typedef AbstractREnvironmentHierarchy AS;
    typedef StaticAnalysis<AS> Super;

    const std::vector<SEXP>& args;

    static constexpr size_t maxDepth = 5;
    size_t depth;
    Value* staticClosureEnv = Env::notClosed();

    TheScopeAnalysis(Closure* cls, const std::vector<SEXP>& args)
        : Super(cls), args(args), depth(0) {}
    TheScopeAnalysis(Closure* cls, const std::vector<SEXP>& args,
                     Value* staticClosureEnv, BB* bb, const AS& initialState,
                     size_t depth)
        : Super(cls, initialState), args(args), depth(depth),
          staticClosureEnv(staticClosureEnv) {}

    void apply(AS& envs, Instruction* i) const override;

    typedef std::function<void(AbstractLoad)> LoadMaybe;
    void tryLoad(const AS& envs, Instruction* i, LoadMaybe) const;

    void print(std::ostream& out = std::cout);
};

void TheScopeAnalysis::tryLoad(const AS& envs, Instruction* i,
                               LoadMaybe aLoad) const {
    LdVar* ld = LdVar::Cast(i);
    LdVarSuper* sld = LdVarSuper::Cast(i);
    LdFun* ldf = LdFun::Cast(i);
    StVarSuper* sts = StVarSuper::Cast(i);

    if (ld) {
        aLoad(envs.get(ld->env(), ld->varName));
    } else if (sld) {
        aLoad(envs.superGet(sld->env(), sld->varName));
    } else if (ldf) {
        aLoad(envs.get(ldf->env(), ldf->varName));
    } else if (sts) {
        aLoad(envs.superGet(sts->env(), sts->varName));
    }
}

void TheScopeAnalysis::apply(AS& envs, Instruction* i) const {
    StVar* s = StVar::Cast(i);
    StVarSuper* ss = StVarSuper::Cast(i);
    MkEnv* mk = MkEnv::Cast(i);

    bool handled = false;

    if (mk) {
        Value* parentEnv = mk->env();
        // If we know the caller, we can fill in the parent env
        if (parentEnv == Env::notClosed() &&
            staticClosureEnv != Env::notClosed()) {
            parentEnv = staticClosureEnv;
        }
        envs[mk].parentEnv(parentEnv);
        mk->eachLocalVar(
            [&](SEXP name, Value* val) { envs[mk].set(name, val, mk); });
        handled = true;
    } else if (s) {
        envs[s->env()].set(s->varName, s->val(), s);
        handled = true;
    } else if (ss) {
        auto superEnv = envs[ss->env()].parentEnv();
        if (superEnv != AbstractREnvironment::UnknownParent) {
            envs[superEnv].set(ss->varName, ss->val(), ss);
            handled = true;
        }
    } else if (CallInstruction::CastCall(i) && depth < maxDepth) {
        auto calli = CallInstruction::CastCall(i);
        if (auto call = Call::Cast(i)) {
            if (auto trg = call->cls()) {
                MkFunCls* cls = envs.findClosure(i->env(), trg);
                if (cls != AbstractREnvironment::UnknownClosure) {
                    if (cls->fun->argNames.size() == calli->nCallArgs()) {
                        TheScopeAnalysis nextFun(cls->fun, cls->fun->argNames,
                                                 cls->env(), cls->fun->entry,
                                                 envs, depth + 1);
                        nextFun();
                        envs.merge(nextFun.result());
                        handled = true;
                    }
                }
            }
        } else if (auto call = StaticCall::Cast(i)) {
            auto trg = call->cls();
            if (trg && trg->argNames.size() == calli->nCallArgs()) {
                TheScopeAnalysis nextFun(trg, trg->argNames, trg->closureEnv(),
                                         trg->entry, envs, depth + 1);
                nextFun();
                envs.merge(nextFun.result());
                handled = true;
            }
        }
    }

    // Keep track of closures
    auto mkfun = MkFunCls::Cast(i);
    tryLoad(envs, i,
            [&](AbstractLoad load) {
                handled = true;
                // let's check if we loaded a closure
                if (!mkfun)
                    load.result.ifSingleValue(
                        [&](Value* val) { mkfun = MkFunCls::Cast(val); });
            });
    if (mkfun)
        envs[i->env()].mkClosures[i] = mkfun;

    if (!handled) {
        if (i->leaksEnv()) {
            envs[i->env()].leaked = true;
        }
        if (i->changesEnv()) {
            envs[i->env()].taint();
        }
    }
}

void TheScopeAnalysis::print(std::ostream& out) {
    for (size_t id = 0; id < getMergepoints().size(); ++id) {
        auto& m = getMergepoints()[id];
        if (!m.empty()) {
            out << "---- BB_" << id << " -----------------------------\n";
            size_t segment = 0;
            for (auto& e : m) {
                out << "    segm -- " << segment++ << "\n";
                for (auto& entry : e) {
                    auto ptr = entry.first;
                    auto env = entry.second;
                    std::cout << "Env(" << ptr << "), leaked " << env.leaked
                              << ":\n";
                    env.print(out);
                }
            }
            out << "-------------------------------------\n";
        }
    }
    out << "---- exit -----------------------------\n";
    for (auto& entry : result()) {
        auto ptr = entry.first;
        auto env = entry.second;
        std::cout << "Env(" << ptr << "), leaked " << env.leaked << ":\n";
        env.print(out);
    }
    out << "-------------------------------------\n";
}
}

namespace rir {
namespace pir {

ScopeAnalysis::ScopeAnalysis(Closure* function) {
    TheScopeAnalysis analysis(function, function->argNames);
    analysis();
    if (false)
        analysis.print();

    // Collect all abstract values of all loads
    analysis.foreach<PositioningStyle::BeforeInstruction>(
        [&](const AbstractREnvironmentHierarchy& envs, Instruction* i) {
            analysis.tryLoad(envs, i,
                             [&](AbstractLoad load) {
                                 loads.emplace(i, load);
                                 if (load.result.isUnknown()) {
                                     allStoresObserved.insert(i->env());
                                 } else {
                                     load.result.eachSource([&](ValOrig& src) {
                                         observedStores.insert(src.origin);
                                     });
                                 }
                             });
            if (i->leaksEnv()) {
                allStoresObserved.insert(i->env());
            }
        });
}
}
}
