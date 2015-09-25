#ifndef JIT_MEMORY_MANAGER_H
#define JIT_MEMORY_MANAGER_H

#include <llvm/IR/Verifier.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/Support/raw_ostream.h>
#include "llvm/Analysis/Passes.h"

#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/MCJIT.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/CodeGen/GCStrategy.h"
#include "llvm/CodeGen/GCs.h"

#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/Analysis/TargetTransformInfo.h"

#include "Runtime.h"

using namespace llvm;

namespace rjit {

class JITMemoryManager : public llvm::SectionMemoryManager {
  private:
    struct MemoryGroup {
        llvm::SmallVector<llvm::sys::MemoryBlock, 16> AllocatedMem;
        llvm::SmallVector<llvm::sys::MemoryBlock, 16> FreeMem;
        llvm::sys::MemoryBlock Near;
    };

  public:
    JITMemoryManager(){};

    uint64_t getSymbolAddress(const std::string& name) override;

    uint8_t* allocateDataSection(uintptr_t size, unsigned alignment,
                                 unsigned sectionID,
                                 llvm::StringRef sectionName,
                                 bool readonly) override;

    uint8_t* allocateCodeSection(uintptr_t Size, unsigned Alignment,
                                 unsigned SectionID,
                                 llvm::StringRef SectionName) {
        return allocateSection(CodeMem, Size, Alignment);
    }

    uint8_t* stackmapAddr() { return stackmapAddr_; }
    uintptr_t stackmapSize() { return stackmapSize_; }

  private:
    uint8_t* allocateSection(MemoryGroup& MemGroup, uintptr_t Size,
                             unsigned Alignment);

    bool finalizeMemory(std::string* ErrMsg) override;

    ~JITMemoryManager() {
        for (unsigned i = 0, e = CodeMem.AllocatedMem.size(); i != e; ++i)
            sys::Memory::releaseMappedMemory(CodeMem.AllocatedMem[i]);
    }

    uint8_t* stackmapAddr_;
    uintptr_t stackmapSize_;

    MemoryGroup CodeMem;
};
}
// namespace rjit

#endif
