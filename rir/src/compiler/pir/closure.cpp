#include "closure.h"
#include "closure_version.h"

namespace rir {
namespace pir {

Closure::~Closure() {
    for (auto c : versions)
        delete c.second;
}

ClosureVersion* Closure::cloneWithAssumptions(ClosureVersion* version,
                                              Assumptions asmpt,
                                              const MaybeClsVersion& change) {
    auto newCtx = version->optimizationContext;
    newCtx.assumptions = newCtx.assumptions | asmpt;
    if (versions.count(newCtx))
        return versions.at(newCtx);

    auto copy = version->clone(asmpt);
    versions[newCtx] = copy;
    change(copy);
    return copy;
}

ClosureVersion*
Closure::findCompatibleVersion(const OptimizationContext& ctx) const {
    // Reverse since they are ordered by number of assumptions
    for (auto c = versions.rbegin(); c != versions.rend(); c++) {
        auto candidate = *c;
        auto candidateCtx = candidate.first;
        if (ctx.assumptions.includes(candidateCtx.assumptions))
            return candidate.second;
    }
    return nullptr;
}

ClosureVersion*
Closure::declareVersion(const OptimizationContext& optimizationContext) {
    assert(!versions.count(optimizationContext));
    versions[optimizationContext] = nullptr;
    auto entry = versions.find(optimizationContext);
    auto v = new ClosureVersion(this, entry->first);
    entry->second = v;
    return v;
}

void Closure::print(std::ostream& out, bool tty) const {
    eachVersion([&](ClosureVersion* v) {
        v->print(out, tty);
        out << "-------------------------------\n";
    });
}

} // namespace pir
} // namespace rir
