#ifndef PIR_COMPILER_LOWER_LLVM_H
#define PIR_COMPILER_LOWER_LLVM_H

#include "compiler/pir/pir.h"
#include "runtime/Code.h"
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace rir {
namespace pir {

class LowerLLVM {
  public:
    void*
    tryCompile(ClosureVersion* cls, Code* code,
               const std::unordered_map<Promise*, unsigned>&,
               const std::unordered_set<Instruction*>& needsEnsureNamed,
               const std::unordered_set<Instruction*>& needsSetShared,
               const std::unordered_set<Instruction*>& needsLdVarForUpdate,
               bool refcountAnalysisOverflow);
};

} // namespace pir
} // namespace rir

#endif
