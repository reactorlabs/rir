#include "scope.h"
#include "../pir/pir_impl.h"
#include "query.h"

namespace {
using namespace rir::pir;

class TheScopeAnalysis : public StaticAnalysis<AbstractREnvironmentHierarchy> {
  public:
    typedef AbstractREnvironmentHierarchy AS;
    typedef StaticAnalysis<AS> Super;

    Function* origin;
    const std::vector<SEXP>& args;

    static constexpr size_t maxDepth = 5;
    size_t depth;
    Call* invocation = nullptr;

    TheScopeAnalysis(Function* origin, const std::vector<SEXP>& args, BB* bb)
        : Super(bb), origin(origin), args(args), depth(0) {}
    TheScopeAnalysis(Function* origin, const std::vector<SEXP>& args, BB* bb,
                     const AS& initialState, Call* invocation, size_t depth)
        : Super(bb, initialState), origin(origin), args(args), depth(depth),
          invocation(invocation) {}

    void apply(AS& envs, Instruction* i) const override;

    typedef std::function<void(AbstractLoad)> LoadMaybe;
    void tryLoad(const AS& envs, Instruction* i, LoadMaybe) const;

    void print(std::ostream& out = std::cout);
};

void TheScopeAnalysis::tryLoad(const AS& envs, Instruction* i,
                               LoadMaybe success) const {
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
        auto res = envs.get(ldf->env(), ldf->varName);
        success(res);
    }
}

void TheScopeAnalysis::apply(AS& envs, Instruction* i) const {
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
            if (fun->argNames.size() == call->nCallArgs()) {
                TheScopeAnalysis nextFun(fun, fun->argNames, fun->entry, envs,
                                         call, depth + 1);
                nextFun();
                envs.merge(nextFun.result());
                handled = true;
            }
        }
    }

    // Keep track of closures
    auto mkfun = MkFunCls::Cast(i);
    tryLoad(envs, i, [&](AbstractLoad load) {
        handled = true;
        // let's check if we loaded a closure
        if (!mkfun)
            load.result.ifSingleValue(
                [&](Value* val) { mkfun = MkFunCls::Cast(val); });
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

ScopeAnalysis::ScopeAnalysis(Function* function) {
    TheScopeAnalysis analysis(function, function->argNames, function->entry);
    analysis();
    if (false)
        analysis.print();

    // Collect all abstract values of all loads
    analysis.foreach<PositioningStyle::BeforeInstruction>(
        [&](const AbstractREnvironmentHierarchy& envs, Instruction* i) {
            analysis.tryLoad(envs, i, [&](AbstractLoad load) {
                loads.emplace(i, load);
                load.result.eachSource(
                    [&](ValOrig& src) { observedStores.insert(src.origin); });
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
