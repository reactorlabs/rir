#ifndef GC_PASS_H
#define GC_PASS_H

#include "llvm/Pass.h"
#include <llvm/IR/Module.h>

namespace rjit {

llvm::FunctionPass* createPlaceRJITSafepointsPass();
llvm::ModulePass* createRJITRewriteStatepointsForGCPass();
}

#endif
