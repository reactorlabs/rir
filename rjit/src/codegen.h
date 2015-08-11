#ifndef CODEGEN_H
#define CODEGEN_H


#include <R.h>
#include <Rinternals.h>


#define DIE asm("int3")

namespace rjit {

/** Pointer to evaluation function.

  This has the same signature as bcEval function in R for now.
 */
typedef SEXP (*RFunctionPtr)(SEXP, SEXP, Rboolean);

/** Compiles given bytecode into its evaluation function and returns the pointer to it.
 */
SEXP compile(SEXP bytecode);

}


#endif // CODEGEN_H

