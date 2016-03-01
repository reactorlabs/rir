#ifndef TYPES_H_
#define TYPES_H_

#include <llvm/IR/Verifier.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/Support/raw_ostream.h>

namespace rjit {

namespace t {

extern llvm::PointerType* SEXP;
extern llvm::StructType* SEXP_u1;
extern llvm::Type* Int;
extern llvm::Type* Double;
extern llvm::Type* Void;
extern llvm::Type* Bool;

/** Vector length in R. This is equivalent to ptrdiff_t type.
 */
extern llvm::Type* VectorLength;

extern llvm::StructType* SEXPREC;
extern llvm::StructType* VECTOR_SEXPREC;

extern llvm::StructType* cntxt;
extern llvm::PointerType* cntxtPtr;

extern llvm::Type* t_void;
extern llvm::Type* voidPtr;
extern llvm::Type* t_i64;
extern llvm::PointerType* i8ptr;

extern llvm::FunctionType* void_void;
extern llvm::FunctionType* void_sexp;
extern llvm::FunctionType* void_sexpsexp;
extern llvm::FunctionType* void_sexpsexpsexp;
extern llvm::FunctionType* sexp_sexp;
extern llvm::FunctionType* sexp_sexpsexp;
extern llvm::FunctionType* sexp_sexpsexpsexp;
extern llvm::FunctionType* sexp_sexpsexpsexpsexp;

extern llvm::FunctionType* sexp_sexpint;
extern llvm::FunctionType* sexp_sexpsexpint;
extern llvm::FunctionType* int_sexp;
extern llvm::FunctionType* int_sexpsexp;
extern llvm::FunctionType* int_sexpsexpsexp;
extern llvm::FunctionType* int_sexpint;

extern llvm::FunctionType* void_argssexp;
extern llvm::FunctionType* void_argssexpsexp;
extern llvm::FunctionType* void_argssexpint;

extern llvm::FunctionType* void_cntxtsexpsexpsexpsexpsexp;
extern llvm::FunctionType* void_cntxtsexp;
extern llvm::FunctionType* void_cntxtsexpsexp;
extern llvm::FunctionType* sexp_contxtsexpsexp;

extern llvm::FunctionType* nativeFunction_t;
extern llvm::Type* nativeFunctionPtr_t;

} // namespace t

} // namespace rjit

#endif // TYPES_H_
