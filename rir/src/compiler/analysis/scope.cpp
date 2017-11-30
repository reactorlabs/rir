#include "scope.h"
#include "../pir/pir_impl.h"
#include "query.h"

namespace {
using namespace rir::pir;

class TheScopeAnalysis : public StaticAnalysis<ScopeAnalysis::AbstractState> {
  public:
    typedef StaticAnalysis<ScopeAnalysis::AbstractState> Super;

    Function* origin;
    const std::vector<SEXP>& args;

    static constexpr size_t maxDepth = 5;
    size_t depth;
    Call* invocation = nullptr;

    std::unordered_map<Instruction*, ScopeAnalysis::AbstractLoadVal> loads;
    std::unordered_map<Value*, Function*> functions;
    std::set<Instruction*> observedStores;

    TheScopeAnalysis(Function* origin, const std::vector<SEXP>& args, BB* bb)
        : Super(bb), origin(origin), args(args), depth(0) {}
    TheScopeAnalysis(Function* origin, const std::vector<SEXP>& args, BB* bb,
                     const ScopeAnalysis::AbstractState& initialState,
                     Call* invocation, size_t depth)
        : Super(bb, initialState), origin(origin), args(args), depth(depth),
          invocation(invocation) {}

    void apply(ScopeAnalysis::AbstractState& envs,
               Instruction* i) const override;

    void tryLoad(const ScopeAnalysis::AbstractState& envs, Instruction* i,
                 std::function<void(ScopeAnalysis::AbstractLoadVal)>) const;
    void operator()() {
        // Compute Fixedpoint
        Super::operator()();

        // Collect all abstract values of all loads
        collect([&](const ScopeAnalysis::AbstractState& env, Instruction* i) {
            tryLoad(env, i, [&](ScopeAnalysis::AbstractLoadVal a) {
                loads[i] = a;
                observedStores.insert(a.second.origin.begin(),
                                      a.second.origin.end());
            });
            if (i->leaksEnv()) {
                for (auto e : env) {
                    for (auto a : e.second.entries) {
                        observedStores.insert(a.second.origin.begin(),
                                              a.second.origin.end());
                    }
                }
            }
        });
    }

    void print(std::ostream& out = std::cout);
};

void TheScopeAnalysis::tryLoad(
    const ScopeAnalysis::AbstractState& envs, Instruction* i,
    std::function<void(ScopeAnalysis::AbstractLoadVal)> success) const {
    LdVar* ld = LdVar::Cast(i);
    LdVarSuper* sld = LdVarSuper::Cast(i);
    LdFun* ldf = LdFun::Cast(i);

    if (ld) {
        success(envs.get(ld->env(), ld->varName));
    } else if (sld) {
        if (envs.count(sld->env())) {
            auto superEnv = envs.at(sld->env()).parentEnv;
            if (superEnv != UnknownParent)
                success(envs.get(sld->env(), sld->varName));
        }
    } else if (ldf) {
        success(envs.get(ldf->env(), ldf->varName));
    }
}

void TheScopeAnalysis::apply(ScopeAnalysis::AbstractState& envs,
                             Instruction* i) const {
    StVar* s = StVar::Cast(i);
    StVarSuper* ss = StVarSuper::Cast(i);
    MkEnv* mk = MkEnv::Cast(i);
    Call* call = Call::Cast(i);

    bool handled = false;

    if (mk) {
        Value* parentEnv = mk->env();
        // If we know the caller, we can fill in the parent env
        if (parentEnv == Env::theContext() && invocation)
            parentEnv = invocation->env();
        envs[mk].parentEnv = parentEnv;
        mk->eachLocalVar(
            [&](SEXP name, Value* val) { envs[mk].set(name, val, mk); });
        handled = true;
    } else if (s) {
        envs[s->env()].set(s->varName, s->val(), s);
        handled = true;
    } else if (ss) {
        auto superEnv = envs[ss->env()].parentEnv;
        if (superEnv != UnknownParent) {
            envs[superEnv].set(ss->varName, ss->val(), ss);
            handled = true;
        }
    } else if (call && depth < maxDepth) {
        Value* trg = call->cls();
        Function* fun = envs.findFunction(i->env(), trg);
        if (fun != UnknownFunction) {
            if (fun->arg_name.size() == call->nCallArgs()) {
                TheScopeAnalysis nextFun(fun, fun->arg_name, fun->entry, envs,
                                         call, depth + 1);
                nextFun();
                envs.merge(nextFun.exitpoint);
                handled = true;
            }
        }
    }

    // Keep track of closures
    MkClsFun* mkfun = MkClsFun::Cast(i);
    tryLoad(envs, i, [&](ScopeAnalysis::AbstractLoadVal a) {
        handled = true;
        // let's check if we loaded a closure
        if (!mkfun && a.second.singleValue())
            mkfun = MkClsFun::Cast(*a.second.vals.begin());
    });
    if (mkfun)
        envs[i->env()].functionPointers[i] = mkfun->fun;

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
    for (auto& m : this->mergepoint) {
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
    for (auto& entry : exitpoint) {
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

ScopeAnalysis::ScopeAnalysis(Function* function) {
    TheScopeAnalysis analysis(function, function->arg_name, function->entry);
    analysis();
    if (false)
        analysis.print();
    loads = std::move(analysis.loads);
    finalState = std::move(analysis.exitpoint);
    observedStores = std::move(analysis.observedStores);
}
}
}
