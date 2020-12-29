#ifndef PIR_COMPILER_LOWER_LLVM_H
#define PIR_COMPILER_LOWER_LLVM_H

#include "compiler/pir/pir.h"

#include <unordered_map>
#include <unordered_set>

namespace rir {
struct Code;

namespace pir {

struct NeedsRefcountAdjustment;

typedef std::unordered_map<Code*, std::pair<unsigned, MkArg*>> PromMap;

class LowerLLVM {
  public:
    LowerLLVM() {}
    void compile(rir::Code* target, ClosureVersion* cls, Code* code,
                 const PromMap&, const NeedsRefcountAdjustment& refcount,
                 const std::unordered_set<Instruction*>& needsLdVarForUpdate,
                 LogStream& log);
};

} // namespace pir
} // namespace rir

#endif
