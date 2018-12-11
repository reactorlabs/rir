#ifndef PIR_SCOPE_ANALYSIS_H
#define PIR_SCOPE_ANALYSIS_H

#include "../analysis/generic_static_analysis.h"
#include "../pir/pir.h"
#include "abstract_value.h"

#include <unordered_map>
#include <unordered_set>

namespace rir {
namespace pir {

/*
 * Heavyweight scope analysis.
 *
 * This analysis soundly traces all stores and gives a static approximation for
 * all loads (`LdVar`).
 *
 */
class ScopeAnalysisState {
    AbstractREnvironmentHierarchy envs;
    std::unordered_map<Instruction*, AbstractPirValue> returnValues;
    AbstractPirValue result;
    std::set<Instruction*> observedStores;
    std::set<Value*> allStoresObserved;

    bool mayUseReflection = false;

    AbstractResult mergeCall(Code* cur, const ScopeAnalysisState& other) {
        AbstractResult res;

        if (!mayUseReflection && other.mayUseReflection) {
            mayUseReflection = true;
            res.lostPrecision();
        }
        for (auto s : other.observedStores) {
            if (!observedStores.count(s)) {
                observedStores.insert(s);
                res.update();
            }
        }
        for (auto s : other.allStoresObserved) {
            if (!allStoresObserved.count(s)) {
                allStoresObserved.insert(s);
                res.update();
            }
        }
        res.max(envs.merge(other.envs));
        return res;
    }

    friend class ScopeAnalysis;

  public:
    AbstractResult merge(const ScopeAnalysisState& other) {
        AbstractResult res;

        for (const auto& f : other.returnValues)
            res.max(returnValues[f.first].merge(f.second));
        if (!mayUseReflection && other.mayUseReflection) {
            mayUseReflection = true;
            res.lostPrecision();
        }
        allStoresObserved.insert(other.allStoresObserved.begin(),
                                 other.allStoresObserved.end());
        for (auto s : other.observedStores) {
            if (!observedStores.count(s)) {
                observedStores.insert(s);
                res.update();
            }
        }
        for (auto s : other.allStoresObserved) {
            if (!allStoresObserved.count(s)) {
                allStoresObserved.insert(s);
                res.update();
            }
        }
        res.max(envs.merge(other.envs));
        return res;
    }

    bool envNotEscaped(Value* v) const {
        return envs.known(v) && !envs.at(v).leaked;
    }

    bool deadStore(Instruction* i) const {
        return !allStoresObserved.count(i->env()) && !observedStores.count(i);
    }

    bool noReflection() const { return !mayUseReflection; };

    void print(std::ostream& out, bool tty) const {
        envs.print(out, tty);
        if (returnValues.size() > 0) {
            out << "== Infered call/force result:\n";
            for (auto& t : returnValues) {
                out << "* ";
                t.first->printRef(out);
                out << " : ";
                t.second.print(out, tty);
                out << "\n";
            }
        }
        out << "== Result: ";
        result.print(out, tty);
        out << "\n";
        if (mayUseReflection)
            out << "* Reflection possible\n";
    }
};

class ScopeAnalysis : public StaticAnalysis<ScopeAnalysisState> {
  private:
    const std::vector<SEXP> argNames;
    const std::vector<Value*> args;

    static constexpr size_t MAX_DEPTH = 2;
    size_t depth;
    Value* staticClosureEnv = Env::notClosed();
    using StaticAnalysis::PositioningStyle;

  protected:
    AbstractResult apply(ScopeAnalysisState& state,
                         Instruction* i) const override;

  public:
    // Default
    ScopeAnalysis(Closure* cls, LogStream& log)
        : StaticAnalysis("Scope", cls, cls, log), argNames(cls->argNames),
          depth(0) {}

    // For interprocedural analysis of a function
    ScopeAnalysis(Closure* cls, const std::vector<Value*>& args,
                  Value* staticClosureEnv,
                  const ScopeAnalysisState& initialState, size_t depth,
                  LogStream& log)
        : StaticAnalysis("Scope", cls, cls, initialState, log),
          argNames(cls->argNames), args(args), depth(depth),
          staticClosureEnv(staticClosureEnv) {
        assert(args.size() == argNames.size());
    }

    // For interprocedural analysis of a promise
    ScopeAnalysis(Closure* cls, Promise* prom, Value* promEnv,
                  const ScopeAnalysisState& initialState, size_t depth,
                  LogStream& log);

    typedef std::function<void(const AbstractLoad&)> LoadMaybe;
    typedef std::function<void(const AbstractPirValue&)> ValueMaybe;
    typedef std::function<void()> Maybe;

    // Lookup what the analysis knows about the result of executing
    // instruction i. This recursively queries all available ressources,
    // such as binding structure, inter-procedural return values, function
    // arguments and so on.
    void lookup(const ScopeAnalysisState& envs, Value* i, const LoadMaybe&,
                const Maybe& notFound = []() {}) const;
    void lookup(const ScopeAnalysisState& envs, Value* i,
                const ValueMaybe& action,
                const Maybe& notFound = []() {}) const {
        lookup(envs, i, [&](AbstractLoad load) { action(load.result); },
               notFound);
    }

    AbstractLoad loadFun(const ScopeAnalysisState& state, SEXP name,
                         Value* env) const {
        auto aLoad = state.envs.getFun(env, name);
        // recurse into the result
        if (aLoad.result.isSingleValue())
            lookup(state, aLoad.result.singleValue().val,
                   [&](const AbstractLoad& ld) { aLoad = ld; });
        return aLoad;
    }
    AbstractLoad load(const ScopeAnalysisState& state, SEXP name,
                      Value* env) const {
        auto aLoad = state.envs.get(env, name);
        // recurse into the result
        if (aLoad.result.isSingleValue())
            lookup(state, aLoad.result.singleValue().val,
                   [&](const AbstractLoad& ld) { aLoad = ld; });
        return aLoad;
    }
    AbstractLoad superLoad(const ScopeAnalysisState& state, SEXP name,
                           Value* env) const {
        auto aLoad = state.envs.get(env, name);
        // recurse into the result
        if (aLoad.result.isSingleValue())
            lookup(state, aLoad.result.singleValue().val,
                   [&](const AbstractLoad& ld) { aLoad = ld; });
        return aLoad;
    }
};
}
}

#endif
