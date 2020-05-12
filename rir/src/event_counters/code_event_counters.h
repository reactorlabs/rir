#pragma once

#include "event_counters.h"
#include "utils/Set.h"
#include "utils/UUID.h"
#include <algorithm>
#include <cassert>
#include <chrono>
#include <fstream>
#include <iostream>
#include <stack>
#include <string>
#include <unordered_map>
#include <vector>

namespace rir {

struct DispatchTable;
struct FunctionSignature;
struct Function;
struct Code;

// Closure-specific event counters
class CodeEventCounters {
    using Timestamp = std::chrono::system_clock::time_point;

    struct InfoDuringProfile {
        Timestamp startTime;
        // the context with an exit handler to cancel the profile or decrease
        // the number of recursive calls, for each call.
        std::stack<RCNTXT> frames;

        // Whether we exited all calls, so we are done profiling
        bool exitedAllCalls();

        void popCall(bool explicitlyEndContext);

        void pushCall(const Code* myAssociatedCode);

        explicit InfoDuringProfile(const Timestamp& startTime);
    };

    struct CallSite {
        UUID callerCodeUid;
        ptrdiff_t bytecodeOffset;

        CallSite(const Code* callerCode, const void* address);

        bool operator==(const CallSite& other) const;
    };

    struct DispatchTableInfo {
        std::string name;
        unsigned size;
        unsigned numDeopts;

        DispatchTableInfo(const DispatchTable* dispatchTable,
                          const std::string& name, unsigned numDeopts);
    };

    // Names of the events. names.size() is the # of events
    std::vector<std::string> names;
    std::unordered_map<UUID, InfoDuringProfile> codesBeingProfiled;
    // Each entry counts events for one rir code block - the key is the
    // code's uid, the value contains the events aligned with names
    std::unordered_map<UUID, std::vector<size_t>> counters;
    // Names inferred for closure code blocks
    std::unordered_map<UUID, std::string> closureNames;
    // Map of closure version code uid's to function header description
    std::unordered_map<UUID, std::string> functionHeaders;
    // Map of closure version code uids to RIR call sites so far
    std::unordered_map<UUID, SmallSet<CallSite>> closureCallSites;
    // Map of dispatch table's baseline's body's uid (more reliable than
    // address) to info about the entire table
    std::unordered_map<UUID, DispatchTableInfo> closureDispatchTables;

    CodeEventCounters() {}

  public:
    static CodeEventCounters& instance() {
        static CodeEventCounters c;
        return c;
    }
    unsigned registerCounter(const std::string& name);
    void count(SEXP calleeSexp, unsigned counter, size_t n = 1);
    void count(const Code* code, unsigned counter, size_t n = 1);
    void profileStart(const Code* code);
    void profileEnd(const Code* code, bool isBecauseOfContextJump = false);
    void countCallSite(const Function* callee, const Code* callerCode,
                       const void* address);
    void countCallSite(const Code* calleeCode, const Code* callerCode,
                       const void* address);
    void countDeopt(const DispatchTable* dispatchTable);
    void updateDispatchTableInfo(SEXP dispatchTableSexp, SEXP name);
    void updateDispatchTableInfo(const DispatchTable* dispatchTable,
                                 const std::string& name);
    void updateDispatchTableButNotContainedFunctionInfo(
        const DispatchTable* dispatchTable, const std::string& name);
    void assignName(const DispatchTable* dispatchTable,
                    const std::string& name);
    void assignName(const Function* function, const std::string& name,
                    size_t version);
    void recordContainedFunctionHeaders(const DispatchTable* dispatchTable);
    void recordHeader(const Function* function);
    bool aCounterIsNonzero() const;
    bool hasADispatchTable() const;
    void dump() const;
    void dumpCodeCounters() const;
    void dumpNumClosureVersions() const;
    void reset();
    void flush();
};

#ifdef MEASURE
namespace codeEvents {
static unsigned CallSites =
    CodeEventCounters::instance().registerCounter("# distinct callsites");
static unsigned Invocations =
    CodeEventCounters::instance().registerCounter("# invocations");
static unsigned TotalExecutionTime =
    CodeEventCounters::instance().registerCounter("total execution time (µs)");
} // namespace codeEvents

#endif

} // namespace rir
