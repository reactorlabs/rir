#pragma once

#include "event_counters.h"
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

        InfoDuringProfile(Timestamp startTime);
    };

    // Names of the events. names.size() is the # of events
    std::vector<std::string> names;
    std::unordered_map<UUID, InfoDuringProfile> codesBeingProfiled;
    // Each entry counts events for one rir code block - the key is the
    // code's uid, the value contains the events aligned with names
    std::unordered_map<UUID, std::vector<size_t>> counters;
    // Names inferred for closure code blocks
    std::unordered_map<UUID, std::string> closureNames;

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
    void assignName(SEXP dispatchTableSexp, SEXP name);
    void assignName(DispatchTable* dispatchTable, const std::string& name);
    void assignName(Function* function, const std::string& name);
    bool aCounterIsNonzero() const;
    void dump();
    void reset();
    void flush();
};

#ifdef MEASURE
namespace codeEvents {
static unsigned Invocations =
    CodeEventCounters::instance().registerCounter("# invocations");
static unsigned TotalExecutionTime =
    CodeEventCounters::instance().registerCounter("total execution time (µs)");
static unsigned ArgsListCreated =
    CodeEventCounters::instance().registerCounter("# times argslist created");
} // namespace codeEvents

#endif

} // namespace rir
