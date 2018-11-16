#include "scope.h"
#include "../pir/pir_impl.h"
#include "query.h"

namespace {
using namespace rir::pir;

struct ScopeAnalysisState {
    AbstractREnvironmentHierarchy envs;
    std::unordered_map<Closure*, PirType> funTypes;
    bool merge(const ScopeAnalysisState& other) {
        bool changed = false;
        std::unordered_set<Closure*> ks;
        for (const auto& f : other.funTypes)
            changed = funTypes[f.first].merge(f.second) || changed;
        return envs.merge(other.envs) || changed;
    }
    void print(std::ostream& out, bool tty) {
        envs.print(out, tty);
        if (funTypes.size() > 0) {
            out << "== Infered function types:\n";
            for (auto& t : funTypes) {
                out << "* " << t.first->name << " : " << t.second << "\n";
            }
        }
    }
};

class TheScopeAnalysis : public StaticAnalysis<ScopeAnalysisState> {
  public:
    typedef StaticAnalysis<ScopeAnalysisState> Super;

    static const std::vector<SEXP> noArgs;
    const std::vector<SEXP>& args = noArgs;

    static constexpr size_t maxDepth = 5;
    size_t depth;
    Value* staticClosureEnv = Env::notClosed();
    Value* promiseEnv = nullptr;

    TheScopeAnalysis(Closure* cls, const std::vector<SEXP>& args,
                     LogStream& log, DebugLevel debug = DebugLevel::None)
        : Super("Scope", cls, cls, log, debug), args(args), depth(0) {}
    TheScopeAnalysis(Closure* cls, const std::vector<SEXP>& args,
                     Value* staticClosureEnv,
                     const ScopeAnalysisState& initialState, size_t depth,
                     LogStream& log, DebugLevel debug)
        : Super("Scope", cls, cls, initialState, log, debug), args(args),
          depth(depth), staticClosureEnv(staticClosureEnv) {}
    TheScopeAnalysis(Closure* cls, Promise* prom, Value* promEnv,
                     const ScopeAnalysisState& initialState, size_t depth,
                     LogStream& log, DebugLevel debug)
        : Super("Scope", cls, prom, initialState, log, debug), depth(depth),
          promiseEnv(promEnv) {}

    void apply(ScopeAnalysisState& state, Instruction* i) const override;

    typedef std::function<void(AbstractLoad)> LoadMaybe;
    void tryLoad(const ScopeAnalysisState& envs, Instruction* i,
                 LoadMaybe) const;
};
const std::vector<SEXP> TheScopeAnalysis::noArgs;

void TheScopeAnalysis::tryLoad(const ScopeAnalysisState& s, Instruction* i,
                               LoadMaybe aLoad) const {
    if (auto ld = LdVar::Cast(i)) {
        aLoad(s.envs.get(ld->env(), ld->varName));
    } else if (auto sld = LdVarSuper::Cast(i)) {
        aLoad(s.envs.superGet(sld->env(), sld->varName));
    } else if (auto ldf = LdFun::Cast(i)) {
        aLoad(s.envs.get(ldf->env(), ldf->varName));
    } else if (auto sts = StVarSuper::Cast(i)) {
        aLoad(s.envs.superGet(sts->env(), sts->varName));
    }
}

void TheScopeAnalysis::apply(ScopeAnalysisState& state, Instruction* i) const {
    bool handled = false;

    if (auto ret = Return::Cast(i)) {
        if (closure == code) {
            auto t = ret->arg<0>().val()->type;
            if (state.funTypes.count(closure))
                state.funTypes.at(closure).merge(t);
            else
                state.funTypes.emplace(closure, t);
            handled = true;
        }
    } else if (auto mk = MkEnv::Cast(i)) {
        Value* lexicalEnv = mk->lexicalEnv();
        // If we know the caller, we can fill in the parent env
        if (lexicalEnv == Env::notClosed() &&
            staticClosureEnv != Env::notClosed()) {
            lexicalEnv = staticClosureEnv;
        }
        state.envs[mk].parentEnv(lexicalEnv);
        mk->eachLocalVar(
            [&](SEXP name, Value* val) { state.envs[mk].set(name, val, mk); });
        handled = true;
    } else if (auto le = LdFunctionEnv::Cast(i)) {
        assert(promiseEnv);
        assert(!state.envs.aliases.count(le) ||
               state.envs.aliases.at(le) == promiseEnv);
        state.envs.aliases[le] = promiseEnv;
    } else if (auto s = StVar::Cast(i)) {
        state.envs[s->env()].set(s->varName, s->val(), s);
        handled = true;
    } else if (auto ss = StVarSuper::Cast(i)) {
        auto superEnv = state.envs[ss->env()].parentEnv();
        if (superEnv != AbstractREnvironment::UnknownParent) {
            state.envs[superEnv].set(ss->varName, ss->val(), ss);
            handled = true;
        }
    } else if (Force::Cast(i) && depth < maxDepth) {
        auto force = Force::Cast(i);
        if (force->strict) {
            if (auto arg = MkArg::Cast(force->arg<0>().val()->baseValue())) {
                TheScopeAnalysis prom(closure, arg->prom(), arg->env(), state,
                                      depth + 1, log, debug);
                prom();
                state.merge(prom.result());
                handled = true;
            }
        }
    } else if (CallInstruction::CastCall(i) && depth < maxDepth) {
        auto calli = CallInstruction::CastCall(i);
        if (auto call = Call::Cast(i)) {
            auto trg = call->cls()->baseValue();
            assert(trg);
            MkFunCls* cls = state.envs.findClosure(call->callerEnv(), trg);
            if (cls != AbstractREnvironment::UnknownClosure) {
                if (cls->fun->argNames.size() == calli->nCallArgs()) {
                    TheScopeAnalysis nextFun(cls->fun, cls->fun->argNames,
                                             cls->lexicalEnv(), state,
                                             depth + 1, log, debug);
                    nextFun();
                    state.merge(nextFun.result());
                    handled = true;
                }
            }
        } else if (auto call = StaticCall::Cast(i)) {
            auto trg = call->cls();
            if (trg && trg->argNames.size() == calli->nCallArgs()) {
                TheScopeAnalysis nextFun(trg, trg->argNames, trg->closureEnv(),
                                         state, depth + 1, log, debug);
                nextFun();
                state.merge(nextFun.result());
                handled = true;
            }
        } else {
            // TODO: support for NamedCall
            assert((CallBuiltin::Cast(i) || CallSafeBuiltin::Cast(i) ||
                    NamedCall::Cast(i)) &&
                   "New call instruction not handled?");
        }
    }

    // Keep track of closures
    auto mkfun = MkFunCls::Cast(i);
    tryLoad(state, i,
            [&](AbstractLoad load) {
                handled = true;
                // let's check if we loaded a closure
                if (!mkfun)
                    load.result.ifSingleValue(
                        [&](Value* val) { mkfun = MkFunCls::Cast(val); });
            });
    if (mkfun)
        state.envs[mkfun->lexicalEnv()].mkClosures[i] = mkfun;

    if (!handled) {
        if (i->leaksEnv()) {
            state.envs[i->env()].leaked = true;
        }
        if (i->changesEnv()) {
            state.envs[i->env()].taint();
        }
    }
}
}

namespace rir {
namespace pir {

ScopeAnalysis::ScopeAnalysis(Closure* function, LogStream& log) {
    TheScopeAnalysis analysis(function, function->argNames, log,
                              TheScopeAnalysis::DebugLevel::Instruction);
    analysis();

    // Collect all abstract values of all loads
    analysis.foreach<PositioningStyle::BeforeInstruction>(
        [&](const ScopeAnalysisState& state, Instruction* i) {
            analysis.tryLoad(state, i, [&](AbstractLoad load) {
                loads.emplace(i, load);
                if (load.result.isUnknown()) {
                    for (auto env : state.envs.potentialParents(i->env()))
                        allStoresObserved.insert(env);
                } else {
                    load.result.eachSource([&](ValOrig& src) {
                        observedStores.insert(src.origin);
                    });
                }
            });
            if (i->leaksEnv()) {
                for (auto env : state.envs.potentialParents(i->env()))
                    allStoresObserved.insert(env);
            }
        });
    funTypes = std::move(analysis.result().funTypes);
}
}
}
