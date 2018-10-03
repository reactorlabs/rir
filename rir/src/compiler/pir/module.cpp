#include "module.h"
#include "pir_impl.h"

namespace rir {
namespace pir {

void Module::print(std::ostream& out, bool tty) {
    for (auto f : functions) {
        f.current()->print(out, tty);
        out << "\n-------------------------------\n";
    }
}

void Module::printEachVersion(std::ostream& out, bool tty) {
    for (auto f : functions) {
        out << "\n======= Function ========================\n";
        f.current()->print(out, tty);
        f.eachVersion([&](Closure* f) {
            out << "\n     == Version ========================\n";
            f->print(out, tty);
        });
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
    auto* cls = new pir::Closure(name, a, env, f);
    auto functionsIdx = functions.size();
    functions.push_back(VersionedClosure(cls));
    functionMap.emplace(idx, functionsIdx);

    if (!create(cls)) {
        // creation failed, delete declaration
        auto it = functionMap.find(idx);
        functionMap.erase(it);
        if (functionsIdx == functions.size() - 1)
            functions.pop_back();
        else
            functions.erase(functions.begin() + functionsIdx);
        delete cls;
    }
}

void Module::VersionedClosure::deallocatePirFunctions() {
    for (auto f : translations) {
        delete f;
    }
    delete pirClosure;
}

void Module::eachPirFunction(PirClosureIterator it) {
    for (auto& f : functions)
        it(f.current());
}

void Module::eachPirFunction(PirClosureVersionIterator it) {
    for (auto& f : functions)
        it(f);
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
        f.deallocatePirFunctions();
    for (auto e : environments)
        delete e.second;
}
}
}
