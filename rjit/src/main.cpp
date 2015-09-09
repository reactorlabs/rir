#include <llvm/IR/Verifier.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/Support/raw_ostream.h>
#include "llvm/Analysis/Passes.h"

#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/MCJIT.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"

#include <cctype>
#include <cstdio>
#include <map>
#include <string>
#include <vector>
#include <sstream>


#include "rbc.h"

#include <R.h>
#include <Rinternals.h>


#include "ir.h"
#include "compiler.h"

using namespace llvm;
using namespace rjit;

#define REXPORT extern "C"

REXPORT SEXP initializeRJIT() {
    LLVMInitializeNativeTarget();
    LLVMInitializeNativeAsmPrinter();
    LLVMInitializeNativeAsmParser();
    return R_NilValue;
}

REXPORT SEXP jit(SEXP expression) {
    return compile(expression);
}

REXPORT SEXP jitConstants(SEXP expression) {
    assert(TYPEOF(expression) == NATIVESXP and "JIT constants can only be extracted from a NATIVESXP argument");
    return CDR(expression);
}

REXPORT SEXP jitLLVM(SEXP expression) {
    assert(TYPEOF(expression) == NATIVESXP and "LLVM code can only be extracted from a NATIVESXP argument");
    Function * f = reinterpret_cast<Function *>(TAG(expression));
    f->dump();
    return R_NilValue;
}

