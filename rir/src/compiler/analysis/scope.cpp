#include "scope.h"
#include "../pir/pir_impl.h"
#include "query.h"

namespace {
using namespace rir::pir;

class TheScopeAnalysis : public StaticAnalysis<AbstractREnvironmentHierarchy> {
  public:
    typedef AbstractREnvironmentHierarchy AS;
    typedef StaticAnalysis<AS> Super;

    Closure* origin;
    const std::vector<SEXP>& args;

    static constexpr size_t maxDepth = 5;
    size_t depth;
    Value* staticClosureEnv = nullptr;

    TheScopeAnalysis(Closure* origin, const std::vector<SEXP>& args, BB* bb)
        : Super(bb), origin(origin), args(args), depth(0) {}
    TheScopeAnalysis(Closure* origin, const std::vector<SEXP>& args,
                     Value* staticClosureEnv, BB* bb, const AS& initialState,
                     size_t depth)
        : Super(bb, initialState), origin(origin), args(args), depth(depth),
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

    Value* env = nullptr;
    SEXP name = nullptr;
    if (ld) {
        name = ld->varName;
        env = ld->env();
    } else if (sld) {
        if (envs.count(sld->env())) {
            auto superEnv = envs.at(sld->env()).parentEnv;
            if (superEnv != AbstractREnvironment::UnknownParent) {
                name = sld->varName;
                env = sld->env();
            }
        }
    } else if (ldf) {
        name = ldf->varName;
        env = ldf->env();
    } else if (sts) {
        env = Env::parentEnv(sts->env());
        name = sts->varName;
    }
    if (name) {
        auto res = envs.get(env, name);
        aLoad(res);
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
        envs[mk].parentEnv = parentEnv;
        mk->eachLocalVar(
            [&](SEXP name, Value* val) { envs[mk].set(name, val, mk); });
        handled = true;
    } else if (s) {
        envs[s->env()].set(s->varName, s->val(), s);
        handled = true;
    } else if (ss) {
        auto superEnv = envs[ss->env()].parentEnv;
        if (superEnv != AbstractREnvironment::UnknownParent) {
            envs[superEnv].set(ss->varName, ss->val(), ss);
            handled = true;
        }
    } else if (CallInstruction::Cast(i) && depth < maxDepth) {
        auto calli = CallInstruction::Cast(i);
        if (Call::Cast(i)) {
            auto call = Call::Cast(i);
            Value* trg = call->cls();
            MkFunCls* cls = envs.findClosure(i->env(), trg);
            if (cls != AbstractREnvironment::UnknownClosure) {
                if (cls->fun->argNames.size() == call->nCallArgs()) {
                    TheScopeAnalysis nextFun(cls->fun, cls->fun->argNames,
                                             cls->env(), cls->fun->entry, envs,
                                             depth + 1);
                    nextFun();
                    envs.merge(nextFun.result());
                    handled = true;
                }
            }
        } else {
            Closure* trg = nullptr;
            if (StaticCall::Cast(i)) {
                trg = StaticCall::Cast(i)->cls();
            } else if (StaticEagerCall::Cast(i)) {
                trg = StaticEagerCall::Cast(i)->cls();
            }
            assert(trg);
            if (trg->argNames.size() == calli->nCallArgs()) {
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
    size_t id = 0;
    for (auto& m : getMergepoints()) {
        if (!m.empty()) {
            out << "---- BB_" << id++ << " -----------------------------\n";
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
    TheScopeAnalysis analysis(function, function->argNames, function->entry);
    analysis();
    if (false)
        analysis.print();

    // Collect all abstract values of all loads
    analysis.foreach<PositioningStyle::BeforeInstruction>(
        [&](const AbstractREnvironmentHierarchy& envs, Instruction* i) {
            analysis.tryLoad(envs, i,
                             [&](AbstractLoad load) {
                                 loads.emplace(i, load);
                                 load.result.eachSource([&](ValOrig& src) {
                                     observedStores.insert(src.origin);
                                 });
                             });
            if (i->leaksEnv()) {
                for (auto e : envs) {
                    for (auto load : e.second.entries) {
                        load.second.eachSource([&](ValOrig& src) {
                            observedStores.insert(src.origin);
                        });
                    }
                }
            }
        });
}
}
}
