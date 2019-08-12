#ifndef RIR_COMPILER_JIT_LLVM_H
#define RIR_COMPILER_JIT_LLVM_H

#include "llvm/ExecutionEngine/JITSymbol.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"

namespace rir {
namespace pir {

class JitLLVM {
  public:
    static std::string mangle(const std::string&);
    static llvm::LLVMContext C;
    static void createModule();
    static void* tryCompile(llvm::Function*);
    static llvm::Function* declare(const std::string& name,
                                   llvm::FunctionType* signature);
    static llvm::Value* getFunctionDeclaration(const std::string& Name,
                                               llvm::FunctionType* signature,
                                               llvm::IRBuilder<>& builder);
};

} // namespace pir
} // namespace rir

#endif
