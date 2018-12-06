#include "module.h"
#include "pir_impl.h"

namespace rir {
namespace pir {

void Module::print(std::ostream& out, bool tty) {
    for (auto f : closures) {
        f->print(out, tty);
        out << "\n-------------------------------\n";
    }
}

void Module::printEachVersion(std::ostream& out, bool tty) {
    for (auto f : closures) {
        out << "\n======= Function ========================\n";
        f->print(out, tty);
        out << "\n=========================================\n";
    }
}

void Module::createIfMissing(const std::string& name, rir::Function* f,
                             const std::vector<SEXP>& a, Env* env,
                             MaybeCreate create) {
    auto idx = FunctionAndEnv(f, env);
    if (functionMap.count(idx)) {
        return;
    }
    assert(functionMap.count(idx) == 0);
    auto closure = new Closure(name, a, env, f);
    closures.push_back(closure);
    functionMap.emplace(idx, closure);

    if (!create(closure)) {
        functionMap.erase(functionMap.find(idx));
        for (auto i = closures.begin(); i != closures.end(); ++i) {
            if (*i == closure) {
                closures.erase(i);
                break;
            }
        }
        delete closure;
    }
}

void Module::eachPirFunction(PirClosureIterator it) {
    for (auto& f : closures)
        it(f);
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
    for (auto e : environments)
        delete e.second;
    for (auto c : closures)
        delete c;
}
}
}
