// Needed at the top because lower.h imports R macros
#include "llvm_imports.h"

#include "jit.h"
#include "lower_function.h"
#include "native/lower.h"

namespace rir {
namespace pir {

void* LowerLLVM::tryCompile(
    ClosureVersion* cls, Code* code,
    const std::unordered_map<Promise*, unsigned>& m,
    const NeedsRefcountAdjustment& refcount,
    const std::unordered_set<Instruction*>& needsLdVarForUpdate) {

    Jit::createModule();
    auto mangledName = Jit::mangle(cls->name());
    LowerFunction funCompiler(mangledName, cls, code, m, refcount,
                              needsLdVarForUpdate);
    if (!funCompiler.tryCompile())
        return nullptr;

    return Jit::tryCompile(funCompiler.fun);
}

} // namespace pir
} // namespace rir
