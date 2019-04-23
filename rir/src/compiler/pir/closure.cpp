#include "closure.h"
#include "closure_version.h"
#include "env.h"
#include "runtime/DispatchTable.h"

namespace rir {
namespace pir {

Closure::Closure(const std::string& name, rir::Function* function, SEXP formals,
                 SEXP srcRef)
    : origin_(nullptr), function(function), env(Env::notClosed()),
      srcRef_(srcRef), name_(name), formals_(formals) {
    invariant();
}

Closure::Closure(const std::string& name, SEXP closure, rir::Function* f,
                 Env* env)
    : origin_(closure), function(f), env(env), name_(name),
      formals_(FORMALS(closure)) {

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
                                              const Assumptions& asmpt,
                                              const MaybeClsVersion& change) {
    auto newCtx = version->optimizationContext();
    newCtx.assumptions = newCtx.assumptions | asmpt;
    if (versions.count(newCtx))
        return versions.at(newCtx);

    auto copy = version->clone(asmpt);
    versions[newCtx] = copy;
    change(copy);
    return copy;
}

ClosureVersion*
Closure::findCompatibleVersion(const OptimizationContext& ctx) const {
    // ordered by number of assumptions
    for (auto c = versions.rbegin(); c != versions.rend(); c++) {
        auto candidate = *c;
        auto candidateCtx = candidate.first;
        if (candidateCtx.subtype(OptimizationContext(ctx.assumptions)))
            return candidate.second;
    }
    return nullptr;
}

ClosureVersion*
Closure::declareVersion(const OptimizationContext& optimizationContext) {
    assert(!versions.count(optimizationContext));
    versions[optimizationContext] = nullptr;
    auto entry = versions.find(optimizationContext);
    auto v = new ClosureVersion(this, entry->first);
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
