#ifndef RIR_INTERPRETER_DATA_C_H
#define RIR_INTERPRETER_DATA_C_H

#include <stdint.h>
#include <assert.h>
#include "../config.h"

#include "runtime/Code.h"
#include "runtime/RirHeader.h"
#include "runtime/Function.h"
#include "runtime/DispatchTable.h"

#include "R/r.h"


#if defined(__GNUC__) && (! defined(NO_THREADED_CODE))
#  define THREADED_CODE
#endif

typedef rir::Code Code;
typedef rir::Function Function;
typedef rir::DispatchTable DispatchTable;

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
RIR_INLINE Code* isValidCodeObject(SEXP s) {
    if (TYPEOF(s) != EXTERNALSXP)
        return nullptr;
    Code* c = (Code*)s;
    if (c->magic != CODE_MAGIC)
        return nullptr;
    return c;
}

RIR_INLINE Function* isValidFunctionObject(SEXP s) {
    if (TYPEOF(s) != EXTERNALSXP)
        return nullptr;
    return Function::check(s);
}

RIR_INLINE DispatchTable* isValidDispatchTableObject(SEXP s) {
    if (TYPEOF(s) != EXTERNALSXP)
        return nullptr;
    return DispatchTable::check(s);
}

const static uint32_t NO_DEOPT_INFO = (uint32_t)-1;

RIR_INLINE Code* findDefaultArgument(Code* c) {
    Code* e = c->function()->codeEnd();
    while (c != e && !c->isDefaultArgument)
        c = c->next();
    return c;
}

#endif // RIR_INTERPRETER_C_H
