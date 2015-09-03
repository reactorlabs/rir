#ifndef GC_PASS_H
#define GC_PASS_H

#include "llvm/Pass.h"

llvm::FunctionPass * createPlaceRJITSafepointsPass();

#endif
