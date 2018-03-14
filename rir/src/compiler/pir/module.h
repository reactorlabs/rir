#ifndef PIR_MODULE_H
#define PIR_MODULE_H

#include <functional>
#include <iostream>
#include <unordered_map>
#include <vector>

#include "../../runtime/Function.h"

namespace rir {
namespace pir {

class Function;

class Module {
  public:
    Function* declare(rir::Function*, const std::vector<SEXP>& a);
    void print(std::ostream& out = std::cout);
    void printEachVersion(std::ostream& out = std::cout);

    typedef std::function<void(pir::Function*)> PirFunctionIterator;
    struct VersionedFunction {
        SEXP closure = nullptr;
        pir::Function* pirFunction;

        VersionedFunction(SEXP closure, pir::Function* pir)
            : closure(closure), pirFunction(pir) {}
        VersionedFunction(pir::Function* pir) : pirFunction(pir) {}

        pir::Function* current() { return pirFunction; }

        void saveVersion();

        void eachVersion(PirFunctionIterator it);

        void deallocatePirFunctions();

      private:
        std::vector<pir::Function*> translations;
    };

    pir::Function* get(rir::Function* f) {
        assert(functions.count(f));
        return functions.at(f).current();
    }

    typedef std::function<void(VersionedFunction&)> PirFunctionVersionIterator;
    void eachPirFunction(PirFunctionIterator it);
    void eachPirFunction(PirFunctionVersionIterator it);

    ~Module();

  private:
    std::unordered_map<rir::Function*, VersionedFunction> functions;
};

}
}

#endif
