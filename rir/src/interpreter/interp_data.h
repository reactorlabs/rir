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

// Indicates an argument is missing
#define MISSING_ARG_IDX ((unsigned)-1)
// Indicates an argument does not correspond to a valid CodeObject
#define DOTS_ARG_IDX ((unsigned)-2)
// Maximum valid entry for a CodeObject offset/idx entry 
#define MAX_ARG_IDX ((unsigned)-3)

const static uint32_t NO_DEOPT_INFO = (uint32_t)-1;

#endif // RIR_INTERPRETER_C_H
