#include "closure.h"
#include "closure_version.h"
#include "env.h"
#include "runtime/DispatchTable.h"

namespace rir {
namespace pir {

Closure::Closure(const std::string& name, rir::Function* function, SEXP formals,
                 SEXP srcRef, Context userContext)
    : origin_(nullptr), function(function), env(Env::notClosed()),
      srcRef_(srcRef), name_(name), formals_(function, formals),
      userContext_(userContext) {
    invariant();
}

Closure::Closure(const std::string& name, SEXP closure, rir::Function* f,
                 Env* env, Context userContext)
    : origin_(closure), function(f), env(env), name_(name),
      formals_(f, FORMALS(closure)), userContext_(userContext) {

    static SEXP srcRefSymbol = Rf_install("srcref");
    srcRef_ = Rf_getAttrib(closure, srcRefSymbol);
    invariant();
}

void Closure::invariant() const {
    // If this is a rir inner function, then we do not have an origin rir
    // closure (since the closure is then created at runtime).
    assert(origin_ || env == Env::notClosed());
    assert(!origin_ || TYPEOF(origin_) == CLOSXP);
    assert(env == Env::notClosed() || env->rho == CLOENV(origin_));
    assert(!origin_ || formals_.original() == FORMALS(origin_));
}

Closure::~Closure() {
    for (auto c : versions)
        delete c.second;
}

ClosureVersion* Closure::cloneWithAssumptions(ClosureVersion* version,
                                              const Context& asmpt,
                                              const MaybeClsVersion& change) {
    auto newCtx = version->context() | asmpt;
    if (versions.count(newCtx))
        return versions.at(newCtx);

    auto copy = version->clone(asmpt);
    versions[newCtx] = copy;
    change(copy);
    return copy;
}

ClosureVersion* Closure::findCompatibleVersion(const Context& ctx) const {
    // ordered by number of assumptions
    for (auto& candidate : versions) {
        const auto& candidateCtx = candidate.first;
        if (ctx.smaller(candidateCtx))
            return candidate.second;
    }
    return nullptr;
}

ClosureVersion* Closure::declareVersion(const Context& optimizationContext,
                                        rir::Function* optFunction) {
    assert(!versions.count(optimizationContext));
    versions[optimizationContext] = nullptr;
    auto entry = versions.find(optimizationContext);
    auto v = new ClosureVersion(this, optFunction, entry->first);
    entry->second = v;
    return v;
}

void Closure::print(std::ostream& out, bool tty) const {
    eachVersion([&](ClosureVersion* v) {
        v->print(out, tty);
        out << "-------------------------------\n";
    });
}

} // namespace pir
} // namespace rir
