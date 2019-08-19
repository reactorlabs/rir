#ifndef COMPILER_CLOSURE_H
#define COMPILER_CLOSURE_H

#include "../../runtime/Function.h"
#include "closure_signature.h"
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
  private:
    friend class Module;

    Closure(const std::string& name, rir::Function* function, SEXP formals,
            SEXP srcRef, const ClosureSignature& signature);
    Closure(const std::string& name, SEXP closure, rir::Function* function,
            Env* env, const ClosureSignature& signature);

    void invariant() const;

    SEXP origin_;
    rir::Function* function;
    Env* env;
    SEXP srcRef_;
    const std::string name_;
    const FormalArgs formals_;
    ClosureSignature signature;

    std::map<const OptimizationContext, ClosureVersion*> versions;

  public:
    SEXP rirClosure() const {
        assert(origin_ && "Inner function does not have a source rir closure");
        return origin_;
    }

    rir::Function* rirFunction() const { return function; }
    SEXP srcRef() { return srcRef_; }
    Env* closureEnv() const { return env; }
    const std::string& name() const { return name_; }
    size_t nargs() const { return formals_.nargs(); }
    const FormalArgs& formals() const { return formals_; }

    void print(std::ostream& out, bool tty) const;

    friend std::ostream& operator<<(std::ostream& out, const Closure& e) {
        out << e.name();
        return out;
    }

    ClosureVersion* declareVersion(const OptimizationContext&);
    void erase(const OptimizationContext& ctx) { versions.erase(ctx); }

    bool existsVersion(const OptimizationContext& ctx) {
        return versions.count(ctx);
    }
    ClosureVersion* getVersion(const OptimizationContext& ctx) {
        return versions.at(ctx);
    }
    ClosureVersion* findCompatibleVersion(const OptimizationContext& ctx) const;

    typedef std::function<void(ClosureVersion*)> MaybeClsVersion;
    ClosureVersion* cloneWithAssumptions(ClosureVersion* cls,
                                         const Assumptions& asmpt,
                                         const MaybeClsVersion& change);

    typedef std::function<void(pir::ClosureVersion*)> ClosureVersionIterator;
    void eachVersion(ClosureVersionIterator it) const {
        for (auto& v : versions)
            it(v.second);
    }

    ~Closure();
};

} // namespace pir
} // namespace rir

#endif
