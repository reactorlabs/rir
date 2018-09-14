#ifndef API_H_
#define API_H_

#include "R/r.h"
#include "compiler/debugging/debugging.h"
#include <stdint.h>

#define REXPORT extern "C"

extern int R_ENABLE_JIT;

REXPORT SEXP rir_eval(SEXP exp, SEXP env);
REXPORT SEXP pir_compile(SEXP closure, SEXP name, SEXP sebugFlags);
REXPORT SEXP rir_compile(SEXP what, SEXP env);
SEXP pirCompile(SEXP closure, const std::string& name,
                const rir::pir::DebugOptions);
SEXP pirOptDefaultOpts(SEXP closure, SEXP name);
SEXP pirOptDefaultOptsDryrun(SEXP closure, SEXP name);

#endif // API_H_
