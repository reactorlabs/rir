#ifndef API_H_
#define API_H_

#include "R/r.h"
#include "compiler/log/debug.h"
#include "runtime/Context.h"
#include <stdint.h>

#define REXPORT extern "C"

extern int R_ENABLE_JIT;

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

REXPORT SEXP rirSetUserContext(SEXP f, SEXP udc);
REXPORT SEXP rirCreateSimpleIntContext();
REXPORT SEXP serializerCleanup();

// serializer
struct hastAndIndex {
    SEXP hast;
    int index;
};
void hash_ast(SEXP ast, size_t & hast);
void printAST(int space, SEXP ast);
void printAST(int space, int val);
hastAndIndex getHastAndIndex(unsigned src, bool constantPool = false);
REXPORT SEXP startSerializer();
REXPORT SEXP stopSerializer();
REXPORT SEXP loadBitcodes();
REXPORT SEXP processBitcodeFolders(SEXP path);
REXPORT SEXP addMaskDataToBitcodeMetadata(SEXP path);
REXPORT SEXP compileStats();
REXPORT SEXP startDebugMessages();
REXPORT SEXP stopDebugMessages();
#endif // API_H_
