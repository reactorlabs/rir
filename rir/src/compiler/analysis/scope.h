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
  public:
    std::unordered_map<Instruction*, AbstractLoad> loads;
    std::unordered_set<Instruction*> observedStores;
    AbstractREnvironmentHierarchy finalState;
    ScopeAnalysis(Function* fun);
};
}
}

#endif
