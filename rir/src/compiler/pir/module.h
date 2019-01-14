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

    bool exists(rir::Function* f, const OptimizationContext& ctx) {
        return closures[f].count(ctx);
    }
    Closure* get(rir::Function* f, const OptimizationContext& ctx) {
        return closures.at(f).at(ctx);
    }
    Closure* findCompatible(rir::Function* f, const OptimizationContext& ctx) {
        if (!closures.count(f))
            return nullptr;
        auto candidates = closures.at(f);
        // Reverse since they are ordered by number of assumptions
        for (auto c = candidates.rbegin(); c != candidates.rend(); c++) {
            auto candidate = *c;
            auto candidateCtx = candidate.first;
            if (candidateCtx.environment == ctx.environment &&
                ctx.assumptions.includes(candidateCtx.assumptions))
                return candidate.second;
        }
        return nullptr;
    }

    Closure* cloneWithAssumptions(Closure* cls, Assumptions asmpt);

    void erase(rir::Function* f, OptimizationContext ctx);

    Closure* declare(const std::string& name, rir::Function* f,
                     OptimizationContext ctx, const std::vector<SEXP>& a);

    typedef std::function<void(pir::Closure*)> PirClosureIterator;
    void eachPirFunction(PirClosureIterator it);

    ~Module();

  private:
    typedef std::map<OptimizationContext, Closure*> ClosureVersions;
    std::map<rir::Function*, ClosureVersions> closures;
};

}
}

#endif
