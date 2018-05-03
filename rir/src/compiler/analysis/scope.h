#ifndef PIR_SCOPE_ANALYSIS_H
#define PIR_SCOPE_ANALYSIS_H

#include "../analysis/generic_static_analysis.h"
#include "../pir/pir.h"
#include "abstract_value.h"

#include <unordered_map>
#include <unordered_set>

namespace rir {
namespace pir {

/*
 * Heavyweight scope analysis.
 *
 * This analysis soundly traces all stores and gives a static approximation for
 * all loads (`LdVar`).
 *
 */
class ScopeAnalysis {
    std::unordered_set<Instruction*> observedStores;
    std::unordered_set<Value*> allStoresObserved;

  public:
    bool deadStore(Instruction* i) {
        return !allStoresObserved.count(i->env()) && !observedStores.count(i);
    }
    std::unordered_map<Instruction*, AbstractLoad> loads;
    AbstractREnvironmentHierarchy finalState;
    ScopeAnalysis(Closure* fun);
};
}
}

#endif
