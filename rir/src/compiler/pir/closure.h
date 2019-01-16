#ifndef COMPILER_CLOSURE_H
#define COMPILER_CLOSURE_H

#include "../../runtime/Function.h"
#include "code.h"
#include "optimization_context.h"
#include "pir.h"
#include <functional>
#include <map>
#include <sstream>

#include "utils/FormalArgs.h"

namespace rir {
namespace pir {

/*
 * Closure
 *
 * A function does not have an environment per se, but just a number of named
 * arguments. If an environment is necessary, `MkEnv` can bind arguments
 * (referred to by `LdArg`).
 *
 */
class Closure {
    friend class Module;

    Closure(const std::string& name, const FormalArgs& formals,
            rir::Function* function, Env* env)
        : function(function), env(env), name(name), formals(formals) {}

    rir::Function* function;
    Env* env;

  public:
    const std::string name;
    Env* closureEnv() const { return env; }

    rir::Function* rirVersion() { return function; }

    const FormalArgs formals;
    const std::vector<SEXP>& argNames() const { return formals.names; }

    size_t nargs() const { return argNames().size(); }

    void print(std::ostream& out, bool tty) const;

    friend std::ostream& operator<<(std::ostream& out, const Closure& e) {
        out << e.name;
        return out;
    }

    ClosureVersion*
    declareVersion(const OptimizationContext& optimizationContext);

    typedef std::function<void(pir::ClosureVersion*)> ClosureVersionIterator;
    void eachVersion(ClosureVersionIterator it) const {
        for (auto& v : versions)
            it(v.second);
    }

    bool existsVersion(const OptimizationContext& ctx) {
        return versions.count(ctx);
    }
    ClosureVersion* getVersion(const OptimizationContext& ctx) {
        return versions.at(ctx);
    }
    ClosureVersion* findCompatibleVersion(const OptimizationContext& ctx) const;

    typedef std::function<void(ClosureVersion*)> MaybeClsVersion;
    ClosureVersion* cloneWithAssumptions(ClosureVersion* cls, Assumptions asmpt,
                                         const MaybeClsVersion& change);

    void erase(const OptimizationContext& ctx) { versions.erase(ctx); }

    ~Closure();

  private:
    std::map<const OptimizationContext, ClosureVersion*> versions;
};

} // namespace pir
} // namespace rir

#endif
