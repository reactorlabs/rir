#ifndef PIR_SCOPE_ANALYSIS_H
#define PIR_SCOPE_ANALYSIS_H

#include "../analysis/generic_static_analysis.h"
#include "../pir/pir.h"
#include "abstract_value.h"

#include <algorithm>
#include <set>
#include <unordered_map>

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
    typedef AbstractEnvironment<AbstractValue> AbstractEnv;
    typedef AbstractEnvironmentSet<AbstractEnv, AbstractValue> AbstractState;
    typedef std::pair<Value*, AbstractValue> AbstractLoadVal;
    std::unordered_map<Instruction*, AbstractLoadVal> loads;
    std::set<Instruction*> observedStores;
    AbstractState finalState;
    ScopeAnalysis(Function* fun);
};
}
}

#endif
