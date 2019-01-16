#include "module.h"
#include "pir_impl.h"

namespace rir {
namespace pir {

void Module::print(std::ostream& out, bool tty) {
    eachPirFunction([&](Closure* c) {
        c->print(out, tty);
        out << "\n-------------------------------\n";
    });
}

void Module::erase(rir::Function* f, OptimizationContext ctx) {
    auto& vs = closures.at(f);
    auto i = vs.find(ctx);
    delete (*i).second;
    vs.erase(i);
}

Closure* Module::cloneWithAssumptions(Closure* cls, Assumptions asmpt,
                                      const MaybeCls& change) {
    auto& versions = closures.at(cls->rirVersion());
    for (auto& v : versions) {
        if (v.second == cls) {
            auto copy = cls->clone(asmpt);
            auto newCtx = v.first;
            newCtx.assumptions = newCtx.assumptions | asmpt;
            if (versions.count(newCtx))
                return versions.at(newCtx);

            versions[newCtx] = copy;
            change(copy);
            return copy;
        }
    }

    assert(false);
    return nullptr;
}

Closure* Module::declare(const std::string& name, rir::Function* f,
                         OptimizationContext ctx, const std::vector<SEXP>& a) {
    auto& closureVersions = closures[f];
    assert(!closureVersions.count(ctx));
    auto closure = new Closure(name, a, ctx.environment, f, ctx.assumptions,
                               Closure::Properties());
    closureVersions.emplace(ctx, closure);
    return closure;
}

void Module::eachPirFunction(PirClosureIterator it) {
    for (auto& cs : closures)
        for (auto& c : cs.second)
            it(c.second);
}

Env* Module::getEnv(SEXP rho) {
    if (rho == R_NilValue)
        return Env::nil();

    if (environments.count(rho))
        return environments.at(rho);

    assert(TYPEOF(rho) == ENVSXP);
    Env* parent = getEnv(ENCLOS(rho));
    Env* env = new Env(rho, parent);
    environments[rho] = env;
    return env;
}

Module::~Module() {
    for (auto& e : environments)
        delete e.second;
    for (auto& cs : closures)
        for (auto& c : cs.second)
            delete c.second;
}
}
}
