#include "module.h"
#include "pir_impl.h"

namespace rir {
namespace pir {

void Module::print(std::ostream& out) {
    for (auto f : functions) {
        f.second.current()->print(out);
        out << "\n-------------------------------\n";
    }
}

void Module::printEachVersion(std::ostream& out) {
    for (auto f : functions) {
        out << "\n======= Function ========================\n";
        f.second.current()->print(out);
        f.second.eachVersion([&](Closure* f) {
            out << "\n     == Version ========================\n";
            f->print(out);
        });
        out << "\n=========================================\n";
    }
}

void Module::createIfMissing(rir::Function* f, const std::vector<SEXP>& a,
                             Env* env, MaybeCreate create) {
    auto idx = FunctionAndEnv(f, env);
    if (functions.count(idx)) {
        return;
    }
    Closure* cls = declare(f, a, env);
    if (!create(cls)) {
        // creation failed, delete declaration
        auto it = functions.find(idx);
        delete cls;
        functions.erase(it);
    }
}

Closure* Module::declare(rir::Function* fun, const std::vector<SEXP>& args,
                         Env* env) {
    auto idx = FunctionAndEnv(fun, env);
    assert(functions.count(idx) == 0);
    auto* f = new pir::Closure(args, env, fun);
    functions.emplace(idx, f);
    return f;
}

void Module::VersionedClosure::deallocatePirFunctions() {
    for (auto f : translations) {
        delete f;
    }
    delete pirClosure;
}

void Module::eachPirFunction(PirClosureIterator it) {
    for (auto& f : functions)
        it(f.second.current());
}

void Module::eachPirFunction(PirClosureVersionIterator it) {
    for (auto& f : functions)
        it(f.second);
}

void Module::VersionedClosure::eachVersion(PirClosureIterator it) {
    for (auto f : translations)
        it(f);
}

void Module::VersionedClosure::saveVersion() {
    auto f = current()->clone();
    translations.push_back(f);
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
    for (auto f : functions)
        f.second.deallocatePirFunctions();
    for (auto e : environments)
        delete e.second;
}
}
}
