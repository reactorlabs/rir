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

namespace recording {

void recordCompile(const SEXP cls, const std::string& name,
                   const Context& assumptions);
void recordOsrCompile(const SEXP cls);
void recordCompileFinish(bool succesful);
void recordLLVMBitcode(llvm::Function* fun);

void recordDeopt(rir::Code* c, const DispatchTable* dt, DeoptReason& reason,
                 SEXP trigger);

void recordInvocation(Function* f);
void recordUnregisterInvocation(Function* f);
void recordInvocationDoCall();
void recordInvocationNativeCallTrampoline();

void recordSC(const ObservedCallees& type, size_t idx, Function* fun);
void recordSC(const ObservedTest& type, size_t idx, Function* fun);
void recordSC(const ObservedValues& type, size_t idx, Function* fun);
void recordSCChanged(bool changed);

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

// Record from environment
void recordExecution(const char* filePath, const char* filter);

} // namespace recording
} // namespace rir

// R API
REXPORT SEXP filterRecordings(SEXP compile, SEXP deoptimize, SEXP typeFeedback,
                              SEXP invocation);
REXPORT SEXP startRecordings();
REXPORT SEXP stopRecordings();
REXPORT SEXP resetRecordings();
REXPORT SEXP isRecordings();
REXPORT SEXP saveRecordings(SEXP filename);
REXPORT SEXP loadRecordings(SEXP filename);
REXPORT SEXP getRecordings();
REXPORT SEXP printRecordings(SEXP from);
REXPORT SEXP printEventPart(SEXP obj, SEXP type, SEXP functions);

#else
#define REC_HOOK(code)
#endif // RECORDING

REXPORT SEXP isRecordingsDefined();

#endif
