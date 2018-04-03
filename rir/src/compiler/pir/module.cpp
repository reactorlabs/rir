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
        f.second.eachVersion([&](Function* f) {
            out << "\n     == Version ========================\n";
            f->print(out);
        });
        out << "\n=========================================\n";
    }
}

Function* Module::declare(rir::Function* fun, const std::vector<SEXP>& args) {
    auto* f = new pir::Function(args);
    functions.emplace(fun, f);
    return f;
}

void Module::VersionedFunction::deallocatePirFunctions() {
    for (auto f : translations) {
        delete f;
    }
    delete pirFunction;
}

void Module::eachPirFunction(PirFunctionIterator it) {
    for (auto& f : functions)
        it(f.second.current());
}

void Module::eachPirFunction(PirFunctionVersionIterator it) {
    for (auto& f : functions)
        it(f.second);
}

void Module::VersionedFunction::eachVersion(PirFunctionIterator it) {
    for (auto f : translations)
        it(f);
}

void Module::VersionedFunction::saveVersion() {
    auto f = current()->clone();
    translations.push_back(f);
}

Module::~Module() {
    for (auto f : functions)
        f.second.deallocatePirFunctions();
}
}
}
