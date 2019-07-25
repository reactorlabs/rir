#ifndef RIR_COMPILER_JIT_LLVM_H
#define RIR_COMPILER_JIT_LLVM_H

#include "llvm/IR/Function.h"

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
};

} // namespace pir
} // namespace rir

#endif
