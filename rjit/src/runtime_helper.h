#ifndef JIT_HELPER_H
#define JIT_HELPER_H

#include "llvm_includes.h"


class RuntimeHelper {
public:
    llvm::Function * getFunction(const std::string name);

    std::unique_ptr<llvm::Module> evalM;
    llvm::LLVMContext & context;

    class T {
    public:
        T(llvm::Module * m, llvm::LLVMContext & context);

        llvm::StructType * t_SEXPREC;
        llvm::PointerType * t_SEXP;
        llvm::StructType * t_R_bcstack_t;
        llvm::PointerType * bcStackPtr;
        llvm::IntegerType * t_Rboolean;
        llvm::StructType * t_InterpreterContext;
        llvm::PointerType * p_InterpreterContext;
        llvm::StructType * t_RCNTXT;
        llvm::PointerType * p_RCNTXT;
        llvm::FunctionType * t_applyClosure;
        llvm::StructType * t_listsxp; 
    };

    std::unique_ptr<T> t;

    static RuntimeHelper helper;

private:
    RuntimeHelper();
};

#endif
