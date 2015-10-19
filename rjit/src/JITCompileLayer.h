#ifndef COMPILER_LAYER_H
#define COMPILER_LAYER_H

#include "JITMemoryManager.h"

#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/MCJIT.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"

namespace rjit {

class JITCompileLayer {
  public:
    static ExecutionEngine* getEngine(llvm::Module* m);

  private:
    static void recordStackmaps(llvm::ExecutionEngine* engine, llvm::Module* m,
                                JITMemoryManager* mm);
};
}

#endif
