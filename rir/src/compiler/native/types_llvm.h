#ifndef TYPES_H_
#define TYPES_H_

#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/LLVMContext.h"

namespace rir {
namespace pir {

void initializeTypes(llvm::LLVMContext& context);

namespace t {

extern llvm::PointerType* SEXP;
extern llvm::PointerType* SEXP_ptr;
extern llvm::StructType* SEXP_u1;
extern llvm::Type* i1;
extern llvm::Type* Int;
extern llvm::Type* Double;
extern llvm::Type* Void;
extern llvm::PointerType* IntPtr;
extern llvm::PointerType* DoublePtr;

extern llvm::StructType* setjmp_buf;
extern llvm::PointerType* setjmp_buf_ptr;

/** Vector length in R. This is equivalent to ptrdiff_t type.
 */
extern llvm::Type* VectorLength;

extern llvm::StructType* SEXPREC;
extern llvm::StructType* VECTOR_SEXPREC;
extern llvm::PointerType* VECTOR_SEXPREC_ptr;

extern llvm::StructType* RirRuntimeObject;
extern llvm::PointerType* Code_ptr;
extern llvm::StructType* LazyEnvironment;

extern llvm::StructType* DeoptReason;
extern llvm::PointerType* DeoptReasonPtr;

extern llvm::StructType* RCNTXT;
extern llvm::PointerType* RCNTXT_ptr;

extern llvm::StructType* stackCell;
extern llvm::PointerType* stackCellPtr;

extern llvm::Type* t_void;
extern llvm::Type* voidPtr;
extern llvm::Type* charPtr;
extern llvm::Type* i64;
extern llvm::Type* i32;
extern llvm::Type* i8;
extern llvm::PointerType* i64ptr;
extern llvm::PointerType* i8ptr;

extern llvm::FunctionType* void_void;
extern llvm::FunctionType* void_voidPtr;
extern llvm::FunctionType* void_sexp;
extern llvm::FunctionType* void_sexpsexp;
extern llvm::FunctionType* void_sexpsexpsexp;
extern llvm::FunctionType* sexp_sexp;
extern llvm::FunctionType* sexp_sexpsexp;
extern llvm::FunctionType* sexp_sexpsexpsexp;
extern llvm::FunctionType* sexp_sexpsexpsexpsexp;
extern llvm::FunctionType* sexp_sexp3int;
extern llvm::FunctionType* sexp_sexp3int2;
extern llvm::FunctionType* sexp_sexp2int2;

extern llvm::FunctionType* sexp_double;
extern llvm::FunctionType* sexp_sexpint;
extern llvm::FunctionType* sexp_sexpsexpint;
extern llvm::FunctionType* int_sexpsexp;
extern llvm::FunctionType* int_sexpsexpsexp;
extern llvm::FunctionType* int_sexpint;
extern llvm::FunctionType* int_sexp;
extern llvm::FunctionType* double_sexp;
extern llvm::FunctionType* sexp_int;
extern llvm::FunctionType* sexp_double;

extern llvm::FunctionType* void_argssexp;
extern llvm::FunctionType* void_argssexpsexp;
extern llvm::FunctionType* void_argssexpint;

extern llvm::FunctionType* nativeFunction;
extern llvm::Type* nativeFunctionPtr;
extern llvm::FunctionType* builtinFunction;
extern llvm::Type* builtinFunctionPtr;

} // namespace t
} // namespace pir
} // namespace rir

#endif // TYPES_H_
