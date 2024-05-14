#ifndef RECORDING_HOOKS_H
#define RECORDING_HOOKS_H

#include "llvm/IR/Module.h"

#include <R/r.h>
#include <string>

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
void recordLLVMBitcode( llvm::Function* fun );

void recordDeopt(rir::Code* c, const DispatchTable* dt, DeoptReason& reason,
                 SEXP trigger);
void recordDtOverwrite(const DispatchTable* dt, size_t version,
                       size_t oldDeoptCount);
void recordInvocation(Function* f, ssize_t deltaCount, size_t deltaDeopt);
void prepareRecordSC(const Code* container);
void recordSC(const ObservedCallees& type);
void recordSC(const ObservedTest& type);
void recordSC(const ObservedValues& type);

// Compile heuristics
void recordMarkOptReasonHeuristic();
void recordInvocationCountTimeReason( size_t count, size_t minimalCount, unsigned long time, unsigned long minimalTime );
void recordPirWarmupReason( size_t invocation_count );

// Compile condition
void recordMarkOptReasonCondition();
void recordNotOptimizedReason();
void recordIsImprovingReason();
void recordReoptimizeFlagReason();

// OSR reason
void recordOsrTriggerCallerCalle();
void recordOsrTriggerLoop(size_t loopCount);

void recordReasonsClear();

// Record from environment
void recordExecution( const char* filePath, const char* filter );

} // namespace recording
} // namespace rir

#endif
