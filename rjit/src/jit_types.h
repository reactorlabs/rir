#ifndef JIT_TYPES_H
#define JIT_TYPES_H

#include "llvm_includes.h"

class RuntimeHelper;

class T {
public:
    static llvm::FunctionType * t_voidInstruction0;
    static llvm::FunctionType * t_voidInstruction1;
    static llvm::FunctionType * t_voidInstruction2;
    static llvm::FunctionType * t_voidInstruction3;
    static llvm::FunctionType * t_voidInstruction4;

    static llvm::FunctionType * t_intInstruction0;
    static llvm::FunctionType * t_intInstruction1;
    static llvm::FunctionType * t_intInstruction2;
    static llvm::FunctionType * t_intInstruction3;
    static llvm::FunctionType * t_intInstruction4;

    static void initialize(RuntimeHelper & runtime);
};

#endif
