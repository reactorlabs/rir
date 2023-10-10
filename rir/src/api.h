#ifndef API_H_
#define API_H_

#include "R/r.h"
#include "compiler/log/debug.h"
#include "runtime/Context.h"

#include <stdint.h>

extern int R_ENABLE_JIT;

REXPORT SEXP rirInvocationCount(SEXP what);
REXPORT SEXP pirCompileWrapper(SEXP closure, SEXP name, SEXP debugFlags,
                               SEXP debugStyle);
REXPORT SEXP rirCompile(SEXP what, SEXP env);
REXPORT SEXP pirTests();
REXPORT SEXP pirCheck(SEXP f, SEXP check, SEXP env);
REXPORT SEXP pirSetDebugFlags(SEXP debugFlags);
SEXP pirCompile(SEXP closure, const rir::Context& assumptions,
                const std::string& name, const rir::pir::DebugOptions& debug,
                std::string* closureVersionPirPrint = nullptr);
extern SEXP rirOptDefaultOpts(SEXP closure, const rir::Context&, SEXP name);
extern SEXP rirOptDefaultOptsDryrun(SEXP closure, const rir::Context&,
                                    SEXP name);
REXPORT SEXP rirSerialize(SEXP data, SEXP file);
REXPORT SEXP rirDeserialize(SEXP file);

REXPORT SEXP rirSetUserContext(SEXP f, SEXP udc);
REXPORT SEXP rirCreateSimpleIntContext();

REXPORT SEXP initializeUUIDPool();
REXPORT SEXP initializePrintPrettyGraphFromEnv();
/// Send a message from the compiler client (this) to each connected compiler
/// server, which kills the server (exit 0) on receive. Then stops the client
/// for the remainder of the session
REXPORT SEXP rirKillCompilerServers();
REXPORT SEXP tryToRunCompilerServer();

// this method is just to have an easy way to play around with the code and get
// feedback by calling .Call('playground')
REXPORT SEXP playground();

#endif // API_H_
