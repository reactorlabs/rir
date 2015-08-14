#include "r_intrinsics.h"
#include "memory_manager.h"

using namespace llvm;

Function * Intrinsics::get(std::string name, llvm::Module * module) {
    RuntimeHelper & helper = RuntimeHelper::helper;

    std::vector<llvm::Type*> args;
    args.clear();

    if (name == "__jit__vectorElt") {
        args.push_back(helper.t->t_SEXP);
        args.push_back(llvm::IntegerType::get(llvm::getGlobalContext(), 32));
        MemoryManager::manager.addSymbol("__jit__vectorElt", (uint64_t)&__jit__vectorElt);
    } else if (name == "__jit__cdr") {
        args.push_back(helper.t->t_SEXP);
        MemoryManager::manager.addSymbol("__jit__cdr", (uint64_t)&__jit__cdr);
    } else {
        return nullptr;
    }
    
    auto funT = llvm::FunctionType::get(
        helper.t->t_SEXP, args, false);

    return llvm::Function::Create(
            funT, llvm::Function::ExternalLinkage, name, module);
}

// Fully at the end to avoid name clashes with R macros
#include <R.h>
#include <Rinternals.h>

SEXP __jit__cdr(SEXP body) {
    return CDR(body);
}

SEXP __jit__vectorElt(SEXP e, int i) {
    return VECTOR_ELT(e, i);
}


