#ifndef RIR_COMPILER_JIT_H
#define RIR_COMPILER_JIT_H

#include "llvm_imports.h"

namespace rir {
namespace pir {

class ClosureVersion;
class Jit {
  public:
    static std::string mangle(const std::string&);
    static llvm::LLVMContext C;
    static void createModule();
    static llvm::Module& module();
    static void* tryCompile(llvm::Function*);
    static llvm::Function* declare(ClosureVersion* v, const std::string& name,
                                   llvm::FunctionType* signature);
    static llvm::Function* get(ClosureVersion* v);
    static llvm::Value* getFunctionDeclaration(const std::string& Name,
                                               llvm::FunctionType* signature,
                                               llvm::IRBuilder<>& builder);
};

} // namespace pir
} // namespace rir

#endif
