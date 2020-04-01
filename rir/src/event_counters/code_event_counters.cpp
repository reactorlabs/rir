#include "code_event_counters.h"
#include "R/Printing.h"
#include "runtime/Code.h"

namespace rir {

using Clock = std::chrono::system_clock;
using Timestamp = Clock::time_point;

const std::string ANONYMOUS_DEALLOCATED = "<anonymous deallocated>";

static void endProfileBecauseOfContextSwitch(void* data) {
    const Code* code = (const Code*)data;
    CodeEventCounters::instance().profileEnd(code, true);
}

// Whether we exited all calls, so we are done profiling
bool CodeEventCounters::InfoDuringProfile::exitedAllCalls() {
    return frames.empty();
}

void CodeEventCounters::InfoDuringProfile::popCall(bool explicitlyEndContext) {
    assert(!frames.empty());
    if (explicitlyEndContext) {
        RCNTXT& rContext = frames.top();
        endcontext(&rContext);
    }
    frames.pop();
}

void CodeEventCounters::InfoDuringProfile::pushCall(
    const Code* myAssociatedCode) {
    frames.push({});
    RCNTXT& rContext = frames.top();
    begincontext(&rContext, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
                 R_NilValue, R_NilValue);
    rContext.cend = &rir::endProfileBecauseOfContextSwitch;
    rContext.cenddata = (void*)myAssociatedCode;
}

CodeEventCounters::InfoDuringProfile::InfoDuringProfile(Timestamp startTime)
    : startTime(startTime) {}

unsigned CodeEventCounters::registerCounter(const std::string& name) {
#ifndef MEASURE
    assert(false);
#endif
    auto existing = std::find(names.begin(), names.end(), name);
    if (existing != names.end()) {
        return existing - names.begin();
    }
    names.push_back(name);
    return names.size() - 1;
}

void CodeEventCounters::count(const Code* code, unsigned counter, size_t n) {
    if (!counters.count(code->uid)) {
        counters[code->uid] = std::vector<size_t>(names.size(), 0);
    }
    counters.at(code->uid).at(counter) += n;
}

void CodeEventCounters::profileStart(const Code* code) {
    if (!codesBeingProfiled.count(code->uid)) {
        Timestamp startTime = Clock::now();
        codesBeingProfiled.emplace(code->uid, startTime);
    }

    CodeEventCounters::InfoDuringProfile& info =
        codesBeingProfiled.at(code->uid);
    info.pushCall(code);
}

void CodeEventCounters::profileEnd(const Code* code,
                                   bool isBecauseOfContextJump) {
    if (!codesBeingProfiled.count(code->uid)) {
        SEXP codeAst = code->getAst();
        std::string codeName = dumpSexp(codeAst);

        std::cerr << "CodeEventCounters: tried to finish profiling code "
                  << codeName << " but it's not being profiled";
        assert(false);
    }

    CodeEventCounters::InfoDuringProfile& info =
        codesBeingProfiled.at(code->uid);

    info.popCall(!isBecauseOfContextJump);
    if (!info.exitedAllCalls()) {
        return;
    }

    Timestamp startTime = info.startTime;
    Timestamp endTime = Clock::now();
    Timestamp::duration duration = endTime - startTime;
    size_t durationMillis =
        (size_t)std::chrono::duration_cast<std::chrono::milliseconds>(duration)
            .count();

    // so we know it's no longer being profiled
    codesBeingProfiled.erase(code->uid);

#ifdef MEASURE
    count(code, codeEvents::TotalExecutionTime, durationMillis);
#else
    assert(false);
#endif
}

bool CodeEventCounters::aCounterIsNonzero() const { return !counters.empty(); }

void CodeEventCounters::dump() {
    if (!aCounterIsNonzero()) {
        return;
    }

    std::ofstream file;
    file.open("code_events.csv");

    // Heading
    file << "name";
    for (unsigned i = 0; i < names.size(); ++i) {
        file << ", " << names.at(i);
    }
    file << "\n";

    // Body
    for (std::pair<UUID, std::vector<size_t>> codeUidAndCodeCounters :
         counters) {
        UUID codeUid = codeUidAndCodeCounters.first;
        std::vector<size_t> codeCounters = codeUidAndCodeCounters.second;

        Code* code = Code::withUidIfExists(codeUid);
        std::string codeName;
        if (code == nullptr) {
            // The code was deallocated - must've been an anonymous closure
            // anyways
            codeName = ANONYMOUS_DEALLOCATED;
        } else {
            SEXP codeAst = code->getAst();
            codeName = dumpSexp(codeAst);
        }

        file << std::quoted(codeName);
        for (unsigned i = 0; i < names.size(); ++i) {
            file << ", " << codeCounters.at(i);
        }
        file << "\n";
    }

    file.close();
}

void CodeEventCounters::reset() { counters.clear(); }

void CodeEventCounters::flush() {
    dump();
    reset();
}

} // namespace rir