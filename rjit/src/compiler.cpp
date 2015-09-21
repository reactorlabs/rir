#ifndef COMPILER_CPP
#define COMPILER_CPP

#include <cstdint>
#include <sstream>
#include <iostream>

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

#include "gc_pass.h"

#include "llvm/IR/Intrinsics.h"

#include "compiler.h"
#include "stack_map.h"

#include "symbols.h"
#include "types.h"
#include "JITMemoryManager.h"
#include "JITModule.h"

#include "Compiler.h"
#include "ICCompiler.h"


using namespace llvm;


namespace rjit {

extern uint64_t nextStackmapId;

// Functions to call in the debugger:
/*
   extern void printType(Type * t);
void printType(Type * t) {
    std::string type_str;
    llvm::raw_string_ostream rso(type_str);
    t->print(rso);
    std::cout << "Type: " << rso.str() << std::endl;
}
extern void printTypeOf(Value * v);
void printTypeOf(Value * v) {
    Type * t = v->getType();
    printType(t);
}
extern void printAllTypeOf(std::vector<Value*> vs);
void printAllTypeOf(std::vector<Value*> vs) {
    for (auto v : vs) {
        Type * t = v->getType();
        printType(t);
    }
}
extern void disassNative(SEXP native);
void disassNative(SEXP native) {
    ((Function*)TAG(native))->dump();
} */






//extern const int patchpointSize;









/*


void patchIC(void * ic, uint64_t stackmapId, void * caller) {
    auto r = StackMap::getPatchpoint(stackmapId);
    assert(r.getNumLocations() == 1);

    uint8_t * patchAddr = (uint8_t*) ((uintptr_t)caller + r.getInstructionOffset());

    int reg = r.getLocation(0).getDwarfRegNum();

    uint8_t prefix = reg > 7 ? 0x49 : 0x48;
    uint8_t movinst = 0xb8 + (reg%8);

    static_assert(patchpointSize == 10, "requre 10 bytes to patch call");

    *patchAddr++ = prefix;
    *patchAddr++ = movinst;

    *(void**)(patchAddr) = ic;
} */

/*void * compileIC(uint64_t numargs, SEXP call, SEXP fun, SEXP rho, uint64_t stackmapId) {
    JITModule m("ic");

    ICCompiler compiler(stackmapId, numargs, m, nextStackmapId++);

    return compiler.compile(call, fun, rho);
} */

/** More complex compilation method that compiles multiple functions into a specified module name.

  The module name is expected to be a STRSXP and the functions is expected to be a pairlist. If pairlist has tags associated with the elements, they will be used as function names.
 */
SEXP compileFunctions(SEXP moduleName, SEXP functions) {
    char const * mName = CHAR(STRING_ELT(moduleName, 0));
    Compiler c(mName);
    while (functions != R_NilValue) {
        SEXP f = CAR(functions);
        // get the function ast
        SEXP body = BODY(f);
        SEXP name = TAG(functions);
        char const * fName = (name == R_NilValue) ? "unnamed function" : CHAR(PRINTNAME(name));
        if (TYPEOF(body) == BCODESXP)
            std::cout << "Ignoring " << fName << " because it is in bytecode" << std::endl;
        else if (TYPEOF(body) == NATIVESXP)
            std::cout << "Ignoring " << fName << " because it is already compiled" << std::endl;
        else
            SET_BODY(f, c.compileFunction(fName, body));
        // move to next function
        functions = CDR(functions);
    }
    c.jitAll();
    return moduleName;
}

SEXP compile(SEXP ast) {
    Compiler c("module");
    SEXP result = c.compile("rfunction", ast);
    c.jitAll();
    return result;
}

} // namespace

#endif // COMPILER_CPP

