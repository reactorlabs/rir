#ifndef R_INTRINSICS_H
#define R_INTRINSICS_H

#include "llvm_includes.h"
#include "runtime_helper.h"

#include <unordered_map>

SEXP __jit__cdr(SEXP);
SEXP __jit__vectorElt(SEXP, int);

class Intrinsics {
    public:
    static std::unordered_map<std::string, int> known;

    static llvm::Function * get(std::string name, llvm::Module * module);
};

#endif
