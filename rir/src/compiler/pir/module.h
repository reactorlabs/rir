#ifndef PIR_MODULE_H
#define PIR_MODULE_H

#include "R/r.h"
#include <functional>
#include <iostream>
#include <map>
#include <unordered_map>
#include <vector>

#include "../../runtime/Function.h"
#include "optimization_context.h"

namespace rir {
namespace pir {

class Closure;
class Env;

class Module {
    std::unordered_map<SEXP, Env*> environments;

  public:
    Env* getEnv(SEXP);

    void print(std::ostream& out = std::cout, bool tty = false);

    bool exists(rir::Function* f, OptimizationContext ctx) {
        return closures[f].count(ctx);
    }
    Closure* get(rir::Function* f, OptimizationContext ctx) {
        return closures.at(f).at(ctx);
    }
    void erase(rir::Function* f, OptimizationContext ctx);

    Closure* declare(const std::string& name, rir::Function* f,
                     OptimizationContext ctx, const std::vector<SEXP>& a);

    typedef std::function<void(pir::Closure*)> PirClosureIterator;
    void eachPirFunction(PirClosureIterator it);

    ~Module();

  private:
    typedef std::unordered_map<OptimizationContext, Closure*> ClosureVersions;
    std::unordered_map<rir::Function*, ClosureVersions> closures;
};

}
}

#endif
