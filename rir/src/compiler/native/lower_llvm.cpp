#include "lower_llvm.h"
#include "jit_llvm.h"
#include "lower_function_llvm.h"

namespace rir {
namespace pir {

void LowerLLVM::compile(
    rir::Code* target, ClosureVersion* cls, Code* code, const PromMap& m,
    const NeedsRefcountAdjustment& refcount,
    const std::unordered_set<Instruction*>& needsLdVarForUpdate,
    LogStream& log) {

    JitLLVM::createModule();
    auto mangledName = JitLLVM::mangle(cls->name());
    LowerFunctionLLVM funCompiler(mangledName, cls, code, m, refcount,
                                  needsLdVarForUpdate, log);
    funCompiler.compile();
    if (funCompiler.pirTypeFeedback)
        target->pirTypeFeedback(funCompiler.pirTypeFeedback);
    if (funCompiler.hasArgReordering())
        target->arglistOrder(ArglistOrder::New(funCompiler.getArgReordering()));
    auto native = JitLLVM::compile(funCompiler.fun);
    target->nativeCode = (NativeCode)native;
}

} // namespace pir
} // namespace rir
