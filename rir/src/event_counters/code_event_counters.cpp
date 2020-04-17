#include "code_event_counters.h"
#include "R/Printing.h"
#include "runtime/Code.h"
#include "runtime/DispatchTable.h"
#include "runtime/Function.h"

namespace rir {

using Clock = std::chrono::system_clock;
using Timestamp = Clock::time_point;

const std::string ANONYMOUS_DEALLOCATED = "<anonymous deallocated>";
const ptrdiff_t UNKNOWN_BYTECODE_OFFSET = -1;

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
        assert(R_GlobalContext == &rContext);
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
    assert(R_GlobalContext == &rContext);
    ((RCNTXT*)R_GlobalContext)->cend = &rir::endProfileBecauseOfContextSwitch;
    ((RCNTXT*)R_GlobalContext)->cenddata = (void*)myAssociatedCode;
}

CodeEventCounters::InfoDuringProfile::InfoDuringProfile(
    const Timestamp& startTime)
    : startTime(startTime) {}

CodeEventCounters::CallSite::CallSite(const Code* callerCode,
                                      const void* address)
    : callerCodeUid(callerCode == NULL ? UUID::null() : callerCode->uid),
      bytecodeOffset(callerCode == NULL || address == nullptr
                         ? UNKNOWN_BYTECODE_OFFSET
                         : (ptrdiff_t)address - (ptrdiff_t)callerCode->code()) {
}

bool CodeEventCounters::CallSite::operator==(const CallSite& other) const {
    return callerCodeUid == other.callerCodeUid &&
           bytecodeOffset == other.bytecodeOffset;
}

CodeEventCounters::DispatchTableInfo::DispatchTableInfo(
    const DispatchTable* dispatchTable, const std::string& name,
    unsigned numDeopts)
    : name(name), size(dispatchTable->size()), numDeopts(numDeopts) {}

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

void CodeEventCounters::count(SEXP calleeSexp, unsigned counter, size_t n) {
    if (auto dispatchTable = DispatchTable::check(calleeSexp)) {
        for (size_t i = 0; i < dispatchTable->size(); i++) {
            Function* function = dispatchTable->get(i);
            count(function->body(), counter, n);
        }
    } else if (auto function = Function::check(calleeSexp)) {
        count(function->body(), counter, n);
    }
}

void CodeEventCounters::count(const Code* code, unsigned counter, size_t n) {
    if (!counters.count(code->uid)) {
        counters[code->uid] = std::vector<size_t>(names.size(), 0);
    }
    counters.at(code->uid).at(counter) += n;
}

void CodeEventCounters::countClosureCall(const DispatchTable* closure,
                                         const Function* takenVersion) {
    UUID codeUid = takenVersion->body()->uid;
    Timestamp time = Clock::now();
    for (size_t i = 0; i < closure->size(); i++) {
        Function* aVersion = closure->get(i);
        if (aVersion == takenVersion) {
            invocationTakenTimestamps[codeUid].push_back(time);
        } else {
            invocationMissedTimestamps[codeUid].push_back(time);
        }
    }
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
    size_t durationMicros =
        (size_t)std::chrono::duration_cast<std::chrono::microseconds>(duration)
            .count();

    // so we know it's no longer being profiled
    codesBeingProfiled.erase(code->uid);

#ifdef MEASURE
    count(code, codeEvents::TotalExecutionTime, durationMicros);
#else
    (void)durationMicros;
    assert(false);
#endif
}

void CodeEventCounters::countCallSite(const Function* callee,
                                      const Code* callerCode,
                                      const void* address) {
    countCallSite(callee->body(), callerCode, address);
}

void CodeEventCounters::countCallSite(const Code* calleeCode,
                                      const Code* callerCode,
                                      const void* address) {
    SmallSet<CallSite>& callSites = closureCallSites[calleeCode->uid];
    CallSite callSite(callerCode, address);

    if (!callSites.count(callSite)) {
        callSites.insert(callSite);
#ifdef MEASURE
        count(calleeCode, codeEvents::CallSites);
#else
        assert(false);
#endif
    }
}

void CodeEventCounters::countDeopt(const DispatchTable* dispatchTable) {
    UUID firstCodeUidWhichIdentifiesEntireTable =
        dispatchTable->get(0)->body()->uid;
    DispatchTableInfo& info =
        closureDispatchTables.at(firstCodeUidWhichIdentifiesEntireTable);
    info.numDeopts++;
    info.size = dispatchTable->size();
}

void CodeEventCounters::updateDispatchTableInfo(SEXP dispatchTableSexp,
                                                SEXP name) {
    updateDispatchTableInfo(DispatchTable::unpack(BODY(dispatchTableSexp)),
                            dumpSexp(name));
}

void CodeEventCounters::updateDispatchTableInfo(
    const DispatchTable* dispatchTable, const std::string& name) {
    updateDispatchTableButNotContainedFunctionInfo(dispatchTable, name);
    assignName(dispatchTable, name);
    recordContainedFunctionHeaders(dispatchTable);
}

void CodeEventCounters::updateDispatchTableButNotContainedFunctionInfo(
    const DispatchTable* dispatchTable, const std::string& name) {
    UUID firstCodeUidWhichIdentifiesEntireTable =
        dispatchTable->get(0)->body()->uid;
    unsigned numDeopts =
        closureDispatchTables.count(firstCodeUidWhichIdentifiesEntireTable)
            ? closureDispatchTables.at(firstCodeUidWhichIdentifiesEntireTable)
                  .numDeopts
            : 0;
    closureDispatchTables.emplace(
        firstCodeUidWhichIdentifiesEntireTable,
        DispatchTableInfo(dispatchTable, name, numDeopts));
}

void CodeEventCounters::assignName(const DispatchTable* dispatchTable,
                                   const std::string& name) {
    // Go backwards to prevent conflicts when a new version is added
    for (int i = dispatchTable->size() - 1; i >= 0; i--) {
        Function* function = dispatchTable->get(i);
        assignName(function, name, i);
    }
}

static bool
closureNameConflicts(const std::unordered_map<UUID, std::string>& closureNames,
                     UUID& closureUid, const std::string& closureName) {
    for (std::pair<UUID, std::string> aClosureUidAndName : closureNames) {
        UUID aClosureUid = aClosureUidAndName.first;
        std::string aClosureName = aClosureUidAndName.second;

        if (aClosureUid != closureUid && aClosureName == closureName) {
            return true;
        }
    }
    return false;
}

void CodeEventCounters::assignName(const Function* function,
                                   const std::string& name, size_t version) {
    UUID uid = function->body()->uid;
    std::string nameWithVersion = name + "$" + std::to_string(version);

    std::string fullName = nameWithVersion;
    unsigned confilictResolvingSuffix = 0;
    while (true) {
        bool nameConflicts = closureNameConflicts(closureNames, uid, fullName);

        if (!nameConflicts) {
            break;
        }

        confilictResolvingSuffix++;
        fullName =
            nameWithVersion + "~" + std::to_string(confilictResolvingSuffix);
    }

    closureNames[uid] = fullName;
}

void CodeEventCounters::recordContainedFunctionHeaders(
    const DispatchTable* dispatchTable) {
    for (size_t i = 0; i < dispatchTable->size(); i++) {
        Function* function = dispatchTable->get(i);
        recordHeader(function);
    }
}

static std::string getHeader(const Function* function) {
    std::stringstream headerStream;
    function->printHeader(headerStream);
    std::string result = headerStream.str();
    std::replace(result.begin(), result.end(), '\n', ';');
    return result;
}

void CodeEventCounters::recordHeader(const Function* function) {
    UUID uid = function->body()->uid;
    functionHeaders[uid] = getHeader(function);
}

bool CodeEventCounters::aCounterIsNonzero() const { return !counters.empty(); }

bool CodeEventCounters::hasADispatchTable() const {
    return !closureDispatchTables.empty();
}

void CodeEventCounters::dump() const {
    dumpCodeCounters();
    dumpNumClosureVersions();
}

static std::string printTimestampVector(const std::vector<Timestamp>& vector) {
    std::stringstream result;
    for (auto it = vector.begin(); it != vector.end(); ++it) {
        Timestamp timestamp = *it;
        size_t timestampMicros =
            std::chrono::time_point_cast<std::chrono::microseconds>(timestamp)
                .time_since_epoch()
                .count();
        result << std::to_string(timestampMicros);
        if (it + 1 != vector.end()) {
            result << "; ";
        }
    }
    return result.str();
}

void CodeEventCounters::dumpCodeCounters() const {
    if (!aCounterIsNonzero()) {
        return;
    }

    std::ofstream file;
    file.open("code_events.csv");

    // Heading
    file << "name";
    file << ", function header, timestamps when invoked, timestamps when a "
            "sibling version is invoked";
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
        if (closureNames.count(codeUid)) {
            codeName = closureNames.at(codeUid);
        } else {
            if (code == nullptr) {
                // The code was deallocated - must've been an anonymous
                // closure anyways
                codeName = ANONYMOUS_DEALLOCATED;
            } else {
                SEXP codeAst = code->getAst();
                codeName = "<code: " + dumpSexp(codeAst) + ">";
            }
        }

        if (code != nullptr) {
            if (codesBeingProfiled.count(code->uid)) {
                Rf_warning("Warning: profiling not ended for %s, ignoring last "
                           "invocation execution time...",
                           codeName.c_str());
            }
        }

        // Empty string if not a function
        std::string functionHeaderOrEmpty;
        if (functionHeaders.count(codeUid)) {
            functionHeaderOrEmpty = functionHeaders.at(codeUid);
        } else {
            functionHeaderOrEmpty = "";
        }

        file << std::quoted(codeName);
        file << ", " << std::quoted(functionHeaderOrEmpty) << ", "
             << (invocationTakenTimestamps.count(codeUid)
                     ? printTimestampVector(
                           invocationTakenTimestamps.at(codeUid))
                     : "")
             << ", "
             << (invocationMissedTimestamps.count(codeUid)
                     ? printTimestampVector(
                           invocationMissedTimestamps.at(codeUid))
                     : "");
        for (unsigned i = 0; i < names.size(); ++i) {
            file << ", " << codeCounters.at(i);
        }
        file << "\n";
    }

    file.close();
}

void CodeEventCounters::dumpNumClosureVersions() const {
    if (!hasADispatchTable()) {
        return;
    }

    std::ofstream file;
    file.open("num_closures_per_table.csv");

    // Heading
    file << "name, final size, # deopts\n";

    // Body
    for (std::pair<UUID, DispatchTableInfo> dispatchTableFirstCodeUidAndInfo :
         closureDispatchTables) {
        DispatchTableInfo info = dispatchTableFirstCodeUidAndInfo.second;

        file << std::quoted(info.name) << ", " << info.size << ", "
             << info.numDeopts << "\n";
    }

    file.close();
}

void CodeEventCounters::reset() {
    counters.clear();
    closureDispatchTables.clear();
}

void CodeEventCounters::flush() {
    dump();
    reset();
}

} // namespace rir