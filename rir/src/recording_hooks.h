#ifndef RECORDING_HOOKS_H
#define RECORDING_HOOKS_H

#include "api.h"
#include "llvm/IR/Module.h"
#include <R/r.h>
#include <string>

#ifdef RECORDING
#define REC_HOOK(code) code

namespace rir {

struct Context;
struct DispatchTable;
struct Function;
struct Code;
struct DeoptReason;
struct ObservedCallees;
struct ObservedTest;
struct ObservedValues;
class TypeFeedback;

namespace pir {
class Module;
}

namespace recording {

void recordCompile(SEXP cls, const std::string& name,
                   const Context& assumptions);
void recordOsrCompile(const SEXP cls);
void recordCompileFinish(bool succesful, pir::Module* module);

void addCompilationLLVMBitcode(pir::ClosureVersion* version,
                               llvm::Function* fun);
void addCompilationSC(pir::ClosureVersion* version, Context context,
                      TypeFeedback* typeFeedback);
void addCompilationSCCloned(pir::ClosureVersion* newVersion,
                            pir::ClosureVersion* prevVersion);

void recordDeopt(rir::Code* c, const DispatchTable* dt, DeoptReason& reason,
                 const Context& context, SEXP trigger);

void recordInvocationDoCall(SEXP cls, Function* f, Context callContext);
void recordInvocationNativeCallTrampoline(SEXP cls, Function* f,
                                          Context callContext,
                                          bool missingAsmptPresent,
                                          bool missingAsmptRecovered);
void recordInvocationRirEval(Function* f);
void recordUnregisterInvocation(SEXP cls, Function* f);

void recordSC(const ObservedCallees& type, size_t idx);
void recordSC(const ObservedTest& type, size_t idx);
void recordSC(const ObservedValues& type, size_t idx);
void recordSCChanged(bool changed);
void recordSCFunctionContext(Function* fun, const Context& ctx);

// Compile heuristics
void recordMarkOptReasonHeuristic();
void recordPirWarmupReason(size_t invocation_count);

// Compile condition
void recordMarkOptReasonCondition();
void recordNotOptimizedReason();
void recordIsImprovingReason();
void recordReoptimizeFlagReason();

// OSR reason
void recordOsrTriggerCallerCallee();
void recordOsrTriggerLoop(size_t loopCount);

void recordReasonsClear();

void recordTypeFeedbackCreated(const DispatchTable* dt, const Context& ctx);

// Record from environment
void recordExecution(const char* filePath, const char* filter);

} // namespace recording
} // namespace rir

// R API
REXPORT SEXP filterRecordings(SEXP compile, SEXP deoptimize, SEXP typeFeedback,
                              SEXP invocation, SEXP context);
REXPORT SEXP startRecordings();
REXPORT SEXP stopRecordings();
REXPORT SEXP resetRecordings();
REXPORT SEXP isRecordings();
REXPORT SEXP saveRecordings(SEXP filename);
REXPORT SEXP loadRecordings(SEXP filename);
REXPORT SEXP getRecordings();
REXPORT SEXP printEventPart(SEXP obj, SEXP type, SEXP functions);
REXPORT SEXP recordCustomEvent(SEXP message);

#else
#define REC_HOOK(code)
#endif // RECORDING

REXPORT SEXP isRecordingsDefined();

#endif
