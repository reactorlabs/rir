#ifndef RIR_INTERPRETER_DATA_C_H
#define RIR_INTERPRETER_DATA_C_H

#include <stdint.h>
#include <assert.h>
#include "../config.h"
#include "runtime/Code.h"
#include "runtime/RirHeader.h"

#include "R/r.h"


#if defined(__GNUC__) && (! defined(NO_THREADED_CODE))
#  define THREADED_CODE
#endif

// Function magic constant is designed to help to distinguish between Function
// objects and normal EXTERNALSXPs. Normally this is not necessary, but a very
// creative user might try to assign arbitrary EXTERNAL to a closure which we
// would like to spot. Of course, such a creative user might actually put the
// magic in his vector too...
#define FUNCTION_MAGIC (unsigned)0xCAFEBABE

#define DISPATCH_TABLE_MAGIC (unsigned)0xBEEF1234

/**
 * Aliases for readability.
 */
typedef SEXP FunctionSEXP;
typedef SEXP SignatureSEXP;
typedef SEXP PromiseSEXP;
typedef SEXP DispatchTableEntry;

typedef rir::Code Code;

// all sizes in bytes,
// length in element sizes


// ============
// Please do not change those without also changing how they are handled in the
// CodeEditor
// Indicates an argument is missing
#define MISSING_ARG_IDX ((unsigned)-1)
// Indicates an argument does not correspond to a valid CodeObject
#define DOTS_ARG_IDX ((unsigned)-2)
// Maximum valid entry for a CodeObject offset/idx entry 
#define MAX_ARG_IDX ((unsigned)-3)

/** Returns whether the SEXP appears to be valid promise, i.e. a pointer into
 * the middle of the linearized code.
 */
INLINE Code* isValidCodeObject(SEXP s) {
    if (TYPEOF(s) != EXTERNALSXP)
        return nullptr;
    Code* c = (Code*)s;
    if (c->magic != CODE_MAGIC)
        return nullptr;
    return c;
}

INLINE Code* next(Code* c) { return (Code*)((uintptr_t)c + c->size()); }

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

// TODO: some better mechanism... for now, keep these synced with the order
// of the SEXPs in Function
#define FUNCTION_SIGNATURE_OFFSET 0
#define FUNCTION_ORIGIN_OFFSET 1
#define FUNCTION_NEXT_OFFSET 2

#pragma pack(push)
#pragma pack(1)
struct Function {
    rir::rir_header info;  /// for exposing SEXPs to GC

    SignatureSEXP signature; /// pointer to this version's signature

    FunctionSEXP origin; /// Same Function with fewer optimizations,
                         //   NULL if original

    FunctionSEXP next;

    unsigned magic; /// used to detect Functions 0xCAFEBABE

    unsigned size; /// Size, in bytes, of the function and its data

    unsigned invocationCount;

    unsigned envLeaked : 1;
    unsigned envChanged : 1;
    unsigned deopt : 1;
    unsigned markOpt : 1;
    unsigned spare : 28;

    unsigned codeLength; /// number of Code objects in the Function

    // We can get to this by searching, but this is faster and so worth the
    // extra four bytes
    unsigned foffset; ///< Offset to the code of the function (last code)

    uint8_t data[]; // Code objects stored inline
};
#pragma pack(pop)

/** Returns the EXTERNALSXP for the Function object. */
INLINE SEXP function2store(Function* f) {
    SEXP result = (SEXP)((uintptr_t)f - sizeof(VECTOR_SEXPREC));
    assert(TYPEOF(result) == EXTERNALSXP &&
           "Either wrong memory altogether or bad counting");
    return result;
}

/** Returns the Function object an SEXP. */
INLINE Function* sexp2function(SEXP s) {
    return (Function*)INTEGER(s);
}

INLINE Function* isValidFunctionObject(SEXP s) {
    if (TYPEOF(s) != EXTERNALSXP)
        return nullptr;
    Function* f = sexp2function(s);
    if (f->magic != FUNCTION_MAGIC)
        return nullptr;
    return f;
}

/** Returns the first code object associated with the function.
 */
INLINE Code* begin(Function* f) { return (Code*)f->data; }

/** Returns the end of the function as code object, for interation purposes.
 */
INLINE Code* end(Function* f) { return (Code*)((uintptr_t)f + f->size); }

/** Returns a pointer to the code of the function (the last code object). */
INLINE Code* bodyCode(Function* f) {
    return (Code*)((uintptr_t)f + f->foffset);
}

/** Returns the code object with given offset */
INLINE Code* codeAt(Function* f, unsigned offset) {
    return (Code*)((uintptr_t)f + offset);
}

/*
 * A dispatch table (vtable) for functions.
 */
#pragma pack(push)
#pragma pack(1)
typedef struct DispatchTable {
    rir::rir_header info;  /// for exposing SEXPs to GC

    uint32_t magic; /// used to detect DispatchTables 0xBEEF1234

    uint32_t capacity; /// size of the entry array
                       /// note: number of currently occupied slots
                       /// is stored in info.gc_area_length

    DispatchTableEntry entry[];
} DispatchTable;
#pragma pack(pop)

INLINE SEXP dispatchTable2store(DispatchTable* t) {
    SEXP result = (SEXP)((uintptr_t)t - sizeof(VECTOR_SEXPREC));
    assert(TYPEOF(result) == EXTERNALSXP &&
           "Either wrong memory altogether or bad counting");
    return result;
}

INLINE DispatchTable* sexp2dispatchTable(SEXP s) {
    return (DispatchTable*)INTEGER(s);
}

INLINE DispatchTable* isValidDispatchTableObject(SEXP s) {
    if (TYPEOF(s) != EXTERNALSXP)
        return nullptr;
    DispatchTable* t = sexp2dispatchTable(s);
    if (t->magic != DISPATCH_TABLE_MAGIC)
        return nullptr;
    return t;
}

INLINE Function* extractFunction(SEXP s) {
    return sexp2function(sexp2dispatchTable(BODY(s))->entry[0]);
}

const static uint32_t NO_DEOPT_INFO = (uint32_t)-1;

INLINE Code* findDefaultArgument(Code* c) {
    Code* e = end(c->function());
    while (c != e && !c->isDefaultArgument)
        c = next(c);
    return c;
}

#endif // RIR_INTERPRETER_C_H
