#ifndef PIR_MODULE_H
#define PIR_MODULE_H

#include "R/r.h"
#include <functional>
#include <iostream>
#include <map>
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

    void print(std::ostream& out = std::cout, bool tty = false);
    void printEachVersion(std::ostream& out = std::cout, bool tty = false);

    typedef std::function<void(pir::Closure*)> PirClosureIterator;
    struct VersionedClosure {
        SEXP closure = nullptr;
        pir::Closure* pirClosure;

        VersionedClosure(SEXP closure, pir::Closure* pir)
            : closure(closure), pirClosure(pir) {}
        explicit VersionedClosure(pir::Closure* pir) : pirClosure(pir) {}

        pir::Closure* current() { return pirClosure; }

        void saveVersion();

        void eachVersion(PirClosureIterator it);

        void deallocatePirFunctions();

      private:
        std::vector<pir::Closure*> translations;
    };

    typedef std::pair<rir::Function*, Env*> FunctionAndEnv;
    pir::Closure* get(FunctionAndEnv idx) {
        assert(functionMap.count(idx));
        return functions[functionMap.at(idx)].current();
    }

    typedef std::function<bool(Closure* f)> MaybeCreate;
    void createIfMissing(const std::string& name, rir::Function* f,
                         const std::vector<SEXP>& a, Env* env,
                         MaybeCreate create);

    typedef std::function<void(VersionedClosure&)> PirClosureVersionIterator;
    void eachPirFunction(PirClosureIterator it);
    void eachPirFunction(PirClosureVersionIterator it);

    ~Module();

  private:
    std::map<FunctionAndEnv, size_t> functionMap;
    std::vector<VersionedClosure> functions;
};

}
}

#endif
