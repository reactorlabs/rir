#ifndef API_H_
#define API_H_

#include "R/r.h"
#include "compiler/debugging/debugging.h"
#include <stdint.h>

#define REXPORT extern "C"

extern int R_ENABLE_JIT;

REXPORT SEXP rir_eval(SEXP, SEXP);
REXPORT SEXP pir_compile(SEXP, SEXP);
SEXP pirCompile(SEXP, const rir::pir::DebugOptions);

#endif // API_H_
