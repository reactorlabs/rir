#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/IR/LLVMContext.h>
#include <iostream>
#include "RIntlns.h"

using namespace llvm;

namespace rjit {

/*
 * Functions to call in the debugger
 *
 * Please don't remove -- this is not dead code
 */

extern void printType(Type* t);
void printType(Type* t) {
    std::string type_str;
    llvm::raw_string_ostream rso(type_str);
    t->print(rso);
    std::cout << "Type: " << rso.str() << std::endl;
}
extern void printTypeOf(Value* v);
void printTypeOf(Value* v) {
    Type* t = v->getType();
    printType(t);
}
extern void printAllTypeOf(std::vector<Value*> vs);
void printAllTypeOf(std::vector<Value*> vs) {
    for (auto v : vs) {
        Type* t = v->getType();
        printType(t);
    }
}
extern void disassNative(SEXP native);
void disassNative(SEXP native) { ((Function*)TAG(native))->dump(); }
extern void disassLLVM(Function* f);
void disassLLVM(Function* f) { f->dump(); }
}
