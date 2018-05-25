#ifndef PIR_MODULE_H
#define PIR_MODULE_H

#include <functional>
#include "R/r.h"
#include <iostream>
#include <unordered_map>
#include <vector>

#include "../../runtime/Function.h"

namespace rir {
namespace pir {

class Closure;
class Env;

class Module {
    std::unordered_map<SEXP, Env*> environments;

  public:
    Env* getEnv(SEXP);

    void print(std::ostream& out = std::cout);
    void printEachVersion(std::ostream& out = std::cout);

    typedef std::function<void(pir::Closure*)> PirClosureIterator;
    struct VersionedClosure {
        SEXP closure = nullptr;
        pir::Closure* pirClosure;

        VersionedClosure(SEXP closure, pir::Closure* pir)
            : closure(closure), pirClosure(pir) {}
        VersionedClosure(pir::Closure* pir) : pirClosure(pir) {}

        pir::Closure* current() { return pirClosure; }

        void saveVersion();

        void eachVersion(PirClosureIterator it);

        void deallocatePirFunctions();

      private:
        std::vector<pir::Closure*> translations;
    };

    pir::Closure* get(rir::Function* f) {
        assert(functions.count(f));
        return functions.at(f).current();
    }

    typedef std::function<void(Closure* f)> MaybeVal;
    typedef std::function<bool(Closure* f)> MaybeCreate;

    void getAndCreateIfMissing(rir::Function* f, const std::vector<SEXP>& a,
                               Env* env, MaybeVal get, MaybeCreate create);

    typedef std::function<void(VersionedClosure&)> PirClosureVersionIterator;
    void eachPirFunction(PirClosureIterator it);
    void eachPirFunction(PirClosureVersionIterator it);

    ~Module();

  private:
    Closure* declare(rir::Function*, const std::vector<SEXP>& a, Env* env);
    std::unordered_map<rir::Function*, VersionedClosure> functions;
};

}
}

#endif
