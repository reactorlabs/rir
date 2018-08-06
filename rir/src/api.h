#ifndef API_H_
#define API_H_

#include "R/r.h"
#include <stdint.h>

#define REXPORT extern "C"

extern int R_ENABLE_JIT;

REXPORT SEXP rir_eval(SEXP, SEXP);
REXPORT SEXP pir_compile(SEXP, SEXP, SEXP);
SEXP pir_compile_(SEXP, uint32_t, SEXP);

#endif // API_H_
