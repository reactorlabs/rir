#include "module.h"
#include "pir_impl.h"

namespace rir {
namespace pir {

void Module::print(std::ostream& out, bool tty) {
    eachPirClosure([&](Closure* c) {
        c->print(out, tty);
        out << "\n================================\n";
    });
}

Closure* Module::getOrDeclare(const std::string& name, rir::Function* f,
                              Env* env, const FormalArgs& formals) {
    if (!closures.count(Idx(f, env))) {
        closures[Idx(f, env)] = new Closure(name, formals, f, env);
    }
    return closures.at(Idx(f, env));
}

void Module::eachPirClosure(PirClosureIterator it) {
    for (auto& c : closures)
        it(c.second);
}

void Module::eachPirClosureVersion(PirClosureVersionIterator it) {
    for (auto& c : closures)
        c.second->eachVersion(it);
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
        delete cs.second;
}
}
}
