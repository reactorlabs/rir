#ifndef PIR_SCOPE_ANALYSIS_H
#define PIR_SCOPE_ANALYSIS_H

#include "../analysis/generic_static_analysis.h"
#include "../pir/closure.h"
#include "../pir/closure_version.h"
#include "../pir/pir.h"
#include "abstract_value.h"
#include "compiler/util/visitor.h"

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
struct ScopeAnalysisResults {
    std::unordered_map<Instruction*, AbstractLoad> results;
    std::unordered_map<Instruction*, AbstractPirValue> returnValues;
    bool _changed;
    void resetChanged() { _changed = false; }
    bool changed() const { return _changed; }
};

class ScopeAnalysisState {
    AbstractREnvironmentHierarchy envs;
    AbstractPirValue returnValue;
    std::unordered_map<MkArg*, AbstractPirValue> forcedPromise;

    bool mayUseReflection = false;

    AbstractResult mergeCall(Code* cur, const ScopeAnalysisState& other) {
        return mergeGeneric(other);
    }

    AbstractResult mergeGeneric(const ScopeAnalysisState& other) {
        AbstractResult res;

        if (!mayUseReflection && other.mayUseReflection) {
            mayUseReflection = true;
            res.lostPrecision();
        }
        if (!returnValue.isUnknown() && other.returnValue.isUnknown()) {
            returnValue.taint();
            res.lostPrecision();
        }

        for (auto u = forcedPromise.begin(); u != forcedPromise.end(); u++) {
            if (u->second.isUnknown())
                continue;
            auto f = other.forcedPromise.find(u->first);
            if (f == other.forcedPromise.end()) {
                u->second.taint();
                res.lostPrecision();
            } else {
                res.max(u->second.merge(f->second));
            }
        }
        if (forcedPromise.size() != other.forcedPromise.size()) {
            for (auto u = other.forcedPromise.begin();
                 u != other.forcedPromise.end(); u++) {
                if (!forcedPromise.count(u->first)) {
                    forcedPromise[u->first] = AbstractPirValue::tainted();
                    res.lostPrecision();
                }
            }
        }

        return res.max(envs.merge(other.envs));
    }

    friend class ScopeAnalysis;

  public:
    AbstractResult merge(const ScopeAnalysisState& other) {
        return mergeGeneric(other);
    }

    AbstractResult mergeExit(const ScopeAnalysisState& other) {
        return mergeGeneric(other);
    }

    bool envNotEscaped(Value* v) const {
        return envs.known(v) && !envs.at(v).leaked();
    }

    bool noReflection() const { return !mayUseReflection; }

    void print(std::ostream& out, bool tty) const {
        envs.print(out, tty);
        out << "== Result: ";
        returnValue.print(out, tty);
        out << "\n";
        if (mayUseReflection)
            out << "* Reflection possible\n";
    }
};

class ScopeAnalysis
    : public StaticAnalysis<
          ScopeAnalysisState,
          ScopeAnalysisResults /*, AnalysisDebugLevel::Taint*/> {
  private:
    const std::vector<Value*> args;

    static constexpr size_t MAX_DEPTH = 2;
    static constexpr size_t MAX_SIZE = 120;
    static constexpr size_t MAX_PROM_SIZE = 14;
    static constexpr size_t MAX_RESULTS = 800;
    static constexpr size_t MAX_SUB_ANALYSIS = 10;
    size_t depth;
    Value* staticClosureEnv = Env::notClosed();
    bool inPromise = false;
    bool canDeopt = false;

    AbstractResult doCompute(ScopeAnalysisState& state, Instruction* i,
                             bool updateGlobalState);

    ScopeAnalysisResults* globalStateStore = nullptr;

    std::unordered_map<Instruction*, std::unique_ptr<ScopeAnalysis>>
        subAnalysis;

  protected:
    AbstractResult compute(ScopeAnalysisState& state, Instruction* i) override {
        return doCompute(state, i, true);
    }
    AbstractResult apply(ScopeAnalysisState& state,
                         Instruction* i) const override {
        return const_cast<ScopeAnalysis*>(this)->doCompute(state, i, false);
    }

  public:
    using StaticAnalysis::PositioningStyle;

    // Default
    ScopeAnalysis(ClosureVersion* cls, Code* code, AbstractLog& log)
        : StaticAnalysis("Scope", cls, code, log), depth(0),
          globalStateStore(new ScopeAnalysisResults) {
        globalState = globalStateStore;
        Visitor::run(code->entry, [&](Instruction* i) {
            if (Assume::Cast(i))
                canDeopt = true;
        });
    }

    // For interprocedural analysis of a function
    ScopeAnalysis(ClosureVersion* cls, const std::vector<Value*>& args,
                  Value* staticClosureEnv,
                  const ScopeAnalysisState& initialState,
                  ScopeAnalysisResults* globalState, size_t depth,
                  AbstractLog& log)
        : StaticAnalysis("Scope", cls, cls, initialState, globalState, log),
          depth(depth), staticClosureEnv(staticClosureEnv) {
        assert(args.size() == cls->effectiveNArgs());
        Visitor::run(code->entry, [&](Instruction* i) {
            if (Assume::Cast(i))
                canDeopt = true;
        });
    }

    // For interprocedural analysis of a promise
    ScopeAnalysis(ClosureVersion* cls, Promise* prom, Value* promEnv,
                  const ScopeAnalysisState& initialState,
                  ScopeAnalysisResults* globalState, size_t depth,
                  AbstractLog& log)
        : StaticAnalysis("Scope", cls, prom, initialState, globalState, log),
          depth(depth), staticClosureEnv(promEnv), inPromise(true) {
        Visitor::run(code->entry, [&](Instruction* i) {
            if (Assume::Cast(i))
                canDeopt = true;
        });
    }

    ScopeAnalysis(const ScopeAnalysis&) = delete;
    ScopeAnalysis& operator=(const ScopeAnalysis& other) = delete;

    ~ScopeAnalysis() {
        if (globalStateStore)
            delete globalStateStore;
    }

    typedef std::function<void(const AbstractLoad&)> LoadMaybe;
    typedef std::function<void(const AbstractPirValue&)> ValueMaybe;
    typedef std::function<void()> Maybe;

  public:
    // Lookup what the analysis knows about the result of executing
    // instruction i. This recursively queries all available ressources,
    // such as binding structure, inter-procedural return values, function
    // arguments and so on.
    void lookup(Value*, const LoadMaybe&,
                const Maybe& notFound = []() {}) const;
    void lookup(Value* v, const ValueMaybe& action,
                const Maybe& notFound = []() {}) const {
        lookup(v, [&](AbstractLoad load) { action(load.result); }, notFound);
    }
    // lookupAt must be called with a state that is valid at the instruction
    // position!
    void lookupAt(const ScopeAnalysisState&, Instruction*, const LoadMaybe&,
                  const Maybe& notFound = []() {}) const;
    void lookupAt(const ScopeAnalysisState& state, Instruction* i,
                  const ValueMaybe& action,
                  const Maybe& notFound = []() {}) const {
        lookupAt(state, i, [&](AbstractLoad load) { action(load.result); },
                 notFound);
    }

    AbstractLoad loadFun(const ScopeAnalysisState& state, SEXP name,
                         Value* env) const {
        auto aLoad = state.envs.getFun(env, name);
        if (aLoad.result.isSingleValue())
            lookup(aLoad.result.singleValue().val,
                   [&](const AbstractLoad& ld) { aLoad = ld; });
        return aLoad;
    }
    AbstractLoad load(const ScopeAnalysisState& state, SEXP name,
                      Value* env) const {
        auto aLoad = state.envs.get(env, name);
        if (aLoad.result.isSingleValue())
            lookup(aLoad.result.singleValue().val,
                   [&](const AbstractLoad& ld) { aLoad = ld; });
        return aLoad;
    }
    AbstractLoad superLoad(const ScopeAnalysisState& state, SEXP name,
                           Value* env) const {
        auto aLoad = state.envs.get(env, name);
        if (aLoad.result.isSingleValue())
            lookup(aLoad.result.singleValue().val,
                   [&](const AbstractLoad& ld) { aLoad = ld; });
        return aLoad;
    }

    typedef std::function<void(
        const std::unordered_map<SEXP, std::pair<AbstractPirValue, bool>>&)>
        MaybeMaterialized;
    void tryMaterializeEnv(const ScopeAnalysisState& state, Value* env,
                           const MaybeMaterialized&);
};

} // namespace pir
} // namespace rir

#endif
