#ifndef COMPILER_CLOSURE_H
#define COMPILER_CLOSURE_H

#include "code.h"
#include "pir.h"
#include "runtime/Context.h"
#include "runtime/Function.h"
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
            SEXP srcRef, Context userContext);
    Closure(const std::string& name, SEXP closure, rir::Function* function,
            Env* env, Context userContext);

    void invariant() const;

    SEXP origin_;
    rir::Function* function;
    Env* env;
    SEXP srcRef_;
    const std::string name_;
    const FormalArgs formals_;

    std::map<const Context, ClosureVersion*> versions;
    Context userContext_;

  public:
    bool matchesUserContext(Context c) const {
        return c.smaller(this->userContext_);
    }

    SEXP rirClosure() const {
        assert(origin_ && "Inner function does not have a source rir closure");
        return origin_;
    }
    bool hasOriginClosure() const { return origin_; }

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

    ClosureVersion* declareVersion(const Context&, rir::Function* optFunction);
    void erase(const Context& ctx) { versions.erase(ctx); }

    bool existsVersion(const Context& ctx) { return versions.count(ctx); }
    ClosureVersion* getVersion(const Context& ctx) { return versions.at(ctx); }
    ClosureVersion* findCompatibleVersion(const Context& ctx) const;

    typedef std::function<void(ClosureVersion*)> MaybeClsVersion;
    ClosureVersion* cloneWithAssumptions(ClosureVersion* cls,
                                         const Context& asmpt,
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
