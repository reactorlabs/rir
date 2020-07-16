#include "module.h"
#include "pir_impl.h"

namespace rir {
namespace pir {

void Module::print(std::ostream& out, bool tty) {
    eachPirClosure([&](Closure* c) {
        out << "================================\n";
        c->print(out, tty);
    });
}

Closure* Module::getOrDeclareRirFunction(const std::string& name,
                                         rir::Function* f, SEXP formals,
                                         SEXP src) {
    auto env = Env::notClosed();
    if (!closures.count(Idx(f, env))) {
        closures[Idx(f, env)] = new Closure(name, f, formals, src);
    }
    return closures.at(Idx(f, env));
}

Closure* Module::getOrDeclareRirClosure(const std::string& name, SEXP closure,
                                        rir::Function* f) {
    // For Identification we use the real env, but for optimization we only use
    // the real environment if this is not an inner function. When it is an
    // inner function, then the env is expected to change over time.
    auto id = Idx(f, getEnv(CLOENV(closure)));
    auto env = f->flags.contains(Function::InnerFunction)
                   ? Env::notClosed()
                   : getEnv(CLOENV(closure));
    if (!closures.count(id))
        closures[id] = new Closure(name, closure, f, env);
    assert(closures.at(id)->rirClosure() == closure);
    return closures.at(id);
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
