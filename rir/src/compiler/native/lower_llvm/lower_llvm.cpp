// Needed at the top because lower_llvm.h imports R macros
#include "llvm_imports.h"

#include "compiler/native/lower_llvm.h"
#include "jit_llvm.h"
#include "lower_function_llvm.h"

namespace rir {
namespace pir {

void* LowerLLVM::tryCompile(
    ClosureVersion* cls, Code* code,
    const std::unordered_map<Promise*, unsigned>& m,
    const NeedsRefcountAdjustment& refcount,
    const std::unordered_set<Instruction*>& needsLdVarForUpdate) {

    JitLLVM::createModule();
    auto mangledName = JitLLVM::mangle(cls->name());
    LowerFunctionLLVM funCompiler(mangledName, cls, code, m, refcount,
                                  needsLdVarForUpdate);
    if (!funCompiler.tryCompile())
        return nullptr;

    return JitLLVM::tryCompile(funCompiler.fun);
}

} // namespace pir
} // namespace rir
