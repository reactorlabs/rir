#ifndef API_H_
#define API_H_

#include "R/r.h"
#include "compiler/log/debug.h"
#include "runtime/Context.h"
#include <stdint.h>

#define REXPORT extern "C"

extern int R_ENABLE_JIT;
extern rir::pir::DebugOptions PirDebug;

REXPORT SEXP rirInvocationCount(SEXP what);
REXPORT SEXP pirCompileWrapper(SEXP closure, SEXP name, SEXP debugFlags,
                               SEXP debugStyle);
REXPORT SEXP rirCompile(SEXP what, SEXP env);
REXPORT SEXP pirTests();
REXPORT SEXP pirCheck(SEXP f, SEXP check, SEXP env);
REXPORT SEXP pirSetDebugFlags(SEXP debugFlags);
SEXP pirCompile(SEXP closure, const rir::Context& assumptions,
                const std::string& name, const rir::pir::DebugOptions& debug);
extern SEXP rirOptDefaultOpts(SEXP closure, const rir::Context&, SEXP name);
extern SEXP rirOptDefaultOptsDryrun(SEXP closure, const rir::Context&,
                                    SEXP name);
REXPORT SEXP rirSerialize(SEXP data, SEXP file);
REXPORT SEXP rirDeserialize(SEXP file);

#endif // API_H_
