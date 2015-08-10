#ifndef IR_H
#define IR_H

#include <llvm/IR/Verifier.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/DerivedTypes.h>

#include <R.h>
#include <Rinternals.h>

#include "rbc.h"

namespace ir {
    extern llvm::StructType * t_SEXPREC;
    extern llvm::PointerType * t_SEXP;
    extern llvm::IntegerType * t_RBoolean;
    extern llvm::StructType * t_InterpreterContext;
    extern llvm::PointerType * p_InterpreterContext;

    extern llvm::FunctionType * t_interpreterLoop;

    extern llvm::Function * initializeInterpreter;
    extern llvm::Function * finalizeInterpreter;
#define INSTRUCTION0(name, opcode) extern llvm::Function * name;
#define INSTRUCTION1(name, opcode) extern llvm::Function * name;
#define INSTRUCTION2(name, opcode) extern llvm::Function * name;
#define INSTRUCTION3(name, opcode) extern llvm::Function * name;
#define SPECIAL0(name, opcode) extern llvm::Function * name;
#define SPECIAL1(name, opcode) extern llvm::Function * name;
#define SPECIAL2(name, opcode) extern llvm::Function * name;
#define SPECIAL3(name, opcode) extern llvm::Function * name;
#define SPECIAL4(name, opcode) extern llvm::Function * name;
    RBC
#undef INSTRUCTION0
#undef INSTRUCTION1
#undef INSTRUCTION2
#undef INSTRUCTION3
#undef SPECIAL0
#undef SPECIAL1
#undef SPECIAL2
#undef SPECIAL3
#undef SPECIAL4
}



#endif // IR_H

