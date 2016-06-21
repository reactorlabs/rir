#ifndef RIR_INTERPRETER_C_H
#define RIR_INTERPRETER_C_H

#include <R.h>
#include <Rinternals.h>

#undef length

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#else
#define bool int
#define true 1
#define false 0
#endif

// we cannot use specific sizes for enums in C
typedef uint8_t OpcodeT;

/** Any argument to BC must be the size of this type. */
// TODO add static asserts somewhere in C++
typedef uint32_t ArgT;

// type  for constant & ast pool indices
typedef uint32_t Immediate;

// type  signed immediate values (unboxed ints)
typedef uint32_t SignedImmediate;

// type of relative jump offset (all jumps are relative)
typedef int32_t JumpOffset;

// enums in C are not namespaces so I am using OP_ to disambiguate
typedef enum {
#define DEF_INSTR(name, ...) name,
#include "insns.h"
    numInsns_
} Opcode;

/**
 * Aliases for readability.
 */
typedef SEXP FunctionSEXP;
typedef SEXP ClosureSEXP;
typedef SEXP PromiseSEXP;
typedef SEXP IntSEXP;

// type of relative jump offset (all jumps are relative)
typedef int32_t JumpOffset;

typedef struct Function Function; // Forward declaration

// all sizes in bytes,
// length in element sizes

/**
 * Code holds a sequence of instructions; for each instruction
 * it records the index of the source AST. Code is part of a
 * Function.
 *
 * Code objects are allocated contiguously within the data
 * section of a Function. The Function header can be found,
 * at an offset from the start of each Code object
 *
 * Instructions are variable size; Code knows how many bytes
 * are required for instructions.
 *
 * The number of indices of source ASTs stored in Code equals
 * the number of instructions.
 *
 * Instructions and AST indices are allocated one after the
 * other in the Code's data section with padding to ensure
 * alignment of indices.
 */
typedef struct Code {
    unsigned header; /// offset to Function object

    // TODO comment these
    unsigned src; /// AST of the function (or promise) represented by the code

    unsigned stackLength; /// Number of slots in stack required

    unsigned iStackLength; /// Number of slots in the integer stack required

    unsigned codeSize; /// bytes of code (not padded)

    unsigned srcLength; /// number of instructions

    uint8_t data[]; /// the instructions
} Code;

/** Returns a pointer to the instructions in c.  */
OpcodeT* code(Code* c);

/** Returns a pointer to the source AST indices in c.  */
unsigned* src(Code* c);

/** Returns a pointer to the Function to which c belongs. */
struct Function* function(Code* c);

/** Returns the next Code in the current function. */
Code* next(Code* c);

// TODO removed src reference, now each code has its own

/** A Function holds the RIR code for some GNU R function.
 *  Each function start with a header and a sequence of
 *  Code objects for the body and all of the promises
 *  in the code.
 *
 *  The header start with a magic constant. This is a
 *  temporary hack so that it is possible to differentiate
 *  an R int vector from a Function. Eventually, we will
 *  add a new SEXP type for this purpose.
 *
 *  The size of the function, in bytes, includes the size
 *  of all of its Code objects and is padded to a word
 *  boundary.
 *
 *  A Function may be the result of optimizing another
 *  Function, in which case the origin field stores that
 *  Function as a SEXP pointer.
 *
 *  A Function has a source AST, stored in src.
 *
 *  A Function has a number of Code objects, codeLen, stored
 *  inline in data.
 */
typedef struct Function {
    unsigned magic; /// used to detect Functions 0xCAFEBABE

    unsigned size; /// Size, in bytes, of the function and its data

    FunctionSEXP origin; /// Same Function with fewer optimizations, NULL if original

    unsigned codeLength; /// number of Code objects in the Function

    // TODO this is misleading because Code objects are not continuous now
    Code data[]; // Code objects stored inline

} Function;

bool isValidFunction(FunctionSEXP s);

Function* origin(Function* f);

/** Returns the first code object associated with the function.
 */
Code* begin(Function* f);

/** Returns the end of the function as code object, for interation purposes.
 */
Code* end(Function* f);

/** Returns an AST located at index in the AST_Pool */
SEXP source(size_t index);

/** TODO Returns the code object with given offset */
Code * codeAt(Function * f, unsigned offset);

/** TODO Makes sure the gc undersands our stacks and pools. */
void gc_callback(void (*forward_node)(SEXP));

SEXP rirEval_c(Code* cur, SEXP env, unsigned numArgs);

#ifdef __cplusplus
}







#endif

#endif // RIR_INTERPRETER_C_H
