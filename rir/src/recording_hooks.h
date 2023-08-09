#ifndef RECORDING_HOOKS_H
#define RECORDING_HOOKS_H

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
void recordDeopt(rir::Code* c, const SEXP cls, DeoptReason& reason,
                 SEXP trigger);
void recordDtOverwrite(const DispatchTable* dt, size_t version,
                       size_t oldDeoptCount);
void recordInvocation(const Function* f, ssize_t deltaCount, size_t deltaDeopt);
void prepareRecordSC(const Code* container);
void recordSC(const ObservedCallees& type);
void recordSC(const ObservedTest& type);
void recordSC(const ObservedValues& type);

} // namespace recording
} // namespace rir

#endif
