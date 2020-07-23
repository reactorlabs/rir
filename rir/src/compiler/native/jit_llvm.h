#ifndef RIR_COMPILER_JIT_LLVM_H
#define RIR_COMPILER_JIT_LLVM_H

#include "builtins.h"
#include "llvm/ExecutionEngine/JITSymbol.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"

namespace rir {
namespace pir {

class ClosureVersion;
class JitLLVM {
  public:
    static std::string mangle(const std::string&);
    static llvm::LLVMContext C;
    static void createModule();
    static llvm::Module& module();
    static void* tryCompile(llvm::Function*);
    static llvm::Function* declare(ClosureVersion* v, const std::string& name,
                                   llvm::FunctionType* signature);
    static llvm::Function* getBuiltin(const NativeBuiltin&);
    static llvm::Function* get(ClosureVersion* v);
    static llvm::Value* getFunctionDeclaration(const std::string& Name,
                                               llvm::FunctionType* signature,
                                               llvm::IRBuilder<>& builder);
};

} // namespace pir
} // namespace rir

#endif
