#ifndef RIR_COMPILER_PASS_SCHEDULE_LLVM_H
#define RIR_COMPILER_PASS_SCHEDULE_LLVM_H

#include "llvm/ExecutionEngine/Orc/Core.h"
#include "llvm/ExecutionEngine/Orc/ThreadSafeModule.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Support/Error.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"

#include <memory>

namespace rir {
namespace pir {

class PassScheduleLLVM {
  public:
    llvm::Expected<llvm::orc::ThreadSafeModule>
    operator()(llvm::orc::ThreadSafeModule TSM,
               llvm::orc::MaterializationResponsibility& R);

    PassScheduleLLVM();

  private:
    static std::unique_ptr<llvm::legacy::PassManager> PM;
};

} // namespace pir
} // namespace rir

#endif
