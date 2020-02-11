#ifndef PIR_COMPILER_LOWER_LLVM_H
#define PIR_COMPILER_LOWER_LLVM_H

#include "compiler/analysis/reference_count.h"
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
               const NeedsRefcountAdjustment& refcount,
               const std::unordered_set<Instruction*>& needsLdVarForUpdate);
};

} // namespace pir
} // namespace rir

#endif
