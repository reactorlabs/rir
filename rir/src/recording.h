#ifndef RECORDING_H
#define RECORDING_H

#include "api.h"
#include "compiler/pir/closure_version.h"
#include "compiler/pir/pir.h"
#include "recording_hooks.h"
#include "runtime/Context.h"
#include "runtime/DispatchTable.h"
#include "runtime/TypeFeedback.h"
#include <R/r.h>
#include <array>
#include <cstddef>
#include <cstdint>
#include <iterator>
#include <memory>
#include <sstream>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

namespace rir {
namespace recording {

class Record;
struct FunRecording;

#define NO_INDEX ((size_t)-1)
#define GLOBAL_ENV_NAME ".GlobalEnv"

enum class SpeculativeContextType { Callees, Test, Values };

class CodeIndex {
    bool promise_;
    size_t index_;

  public:
    CodeIndex(bool promise, size_t index) : promise_(promise), index_(index) {}
    Code* locate(Function* fun);
};

struct SpeculativeContext {
    SpeculativeContextType type;

    union Value {
        std::array<size_t, rir::ObservedCallees::MaxTargets> callees;
        ObservedTest test;
        ObservedValues values;
    } value;

    SpeculativeContext(decltype(value.callees) callees)
        : type{SpeculativeContextType::Callees}, value{.callees = callees} {}

    SpeculativeContext(ObservedTest test)
        : type{SpeculativeContextType::Test}, value{.test = test} {}

    SpeculativeContext(ObservedValues values)
        : type{SpeculativeContextType::Values}, value{.values = values} {}

    void print(const std::vector<FunRecording>& mapping,
               std::ostream& out) const;
};

struct OptReason {
    enum class Type : uint8_t {
        MarkOpt,
        Warmup,
        Invocation,
        NotOptimized,
        IsImproving,
        Reoptimize,
        OsrTriggered // TODO somewhere else
    } reason;

    // For InvocationCountTime
    size_t count = 0;
    unsigned long time = 0;

    friend std::ostream& operator <<(std::ostream& out, const OptReason& reason){
        switch (reason.reason){
        case Type::MarkOpt:
            out << "MarkOpt";
            break;

        case Type::Warmup:
            out << "Warmup";
            break;

        case Type::Invocation:
            out << "Invocation, count: " << reason.count << ", time: " << reason.time;
            break;

        case Type::NotOptimized:
            out << "NotOptimized";
            break;

        case Type::IsImproving:
            out << "IsImproving";
            break;

        case Type::Reoptimize:
            out << "Reoptimize";
            break;

        case OptReason::Type::OsrTriggered:
            out << "OsrTriggered";
            break;
        }

        return out;
    }
};

/**
 * Recorded event
 *
 * `Event` is an abstract class.
 */
class Event {
  public:
    virtual SEXP toSEXP() const = 0;
    virtual void fromSEXP(SEXP sexp) = 0;
    virtual void print(const std::vector<FunRecording>& mapping,
                       std::ostream& out) const = 0;

    /**
     * Returns `true` if the Event directly or indirectly stores the index of a
     * given function recording
     *
     * For instance, a CompilationEvent would return `true` if its stored
     * speculative contexts contained an ObservedCallee refering to the given
     * `recordingIdx`.
     *
     * Function recordings that are never refered may be removed.
     */
    virtual bool containsReference(size_t recordingIdx) const { return false; }
    virtual const char*
    targetName(std::vector<FunRecording>& mapping) const = 0;
};

/**
 * Recorded event that is implicitly attached to a CLOSXP-typed SEXP
 *
 * `ClosureEvent` is an abstract class.
 */
class ClosureEvent : public Event {
  protected:
    ClosureEvent() = default;
    ClosureEvent(size_t closureIndex) : closureIndex(closureIndex){};

    size_t closureIndex;

    virtual bool containsReference(size_t recordingIdx) const override {
        return recordingIdx == closureIndex;
    };

    const char* targetName(std::vector<FunRecording>& mapping) const override;
};

/**
 * Recorded event that is implicitly attached to a CLOSXP's dispatch table
 *
 * `DtEvent` is an abstract class.
 */
class DtEvent : public Event {
  protected:
    DtEvent() = default;
    DtEvent(size_t dispatchTableIndex)
        : dispatchTableIndex(dispatchTableIndex){};

    size_t dispatchTableIndex;

    virtual bool containsReference(size_t recordingIdx) const override {
        return recordingIdx == dispatchTableIndex;
    };

    const char* targetName(std::vector<FunRecording>& mapping) const override;
};

/**
 * `FunctionEvent`s are `Event`s that relate to a closure (instead of any of the
 * 3 function kinds), but also to a specific function version inside its
 * DispatchTable.
 *
 * `VersionEvent` is an abstract class.
 */
class VersionEvent : public DtEvent {
  protected:
    VersionEvent() = default;
    VersionEvent(size_t dispatchTableIndex, Context version)
        : DtEvent(dispatchTableIndex), version(version){};

    Context version = Context(0UL);

  private:
    /**
     * Returns the Function that this event refers to, given the enclosing
     * DispatchTable
     */
    Function* functionVersion(const DispatchTable& in) const;
};

/**
 * Notifies an update to a speculative context
 */
class SpeculativeContextEvent : public DtEvent {
  public:
    SpeculativeContextEvent(size_t dispatchTableIndex, ssize_t codeIndex,
                            size_t offset, SpeculativeContext sc)
        : DtEvent(dispatchTableIndex), codeIndex(codeIndex), offset(offset),
          sc(sc) {}
    SpeculativeContextEvent()
        : codeIndex(-2), offset(0), sc(SpeculativeContext({0, 0, 0})) {}
    SEXP toSEXP() const override;
    void fromSEXP(SEXP sexp) override;
    virtual bool containsReference(size_t dispatchTable) const override;

  protected:
    void print(const std::vector<FunRecording>& mapping,
               std::ostream& out) const override;

  private:
    // -1 for function body itself, n≥0 for promise index
    ssize_t codeIndex;
    size_t offset;
    SpeculativeContext sc;
};

class CompilationEvent : public ClosureEvent {
  public:
    CompilationEvent(size_t closureIndex,
                     unsigned long dispatch_context,
                     std::string compileName,
                     std::vector<SpeculativeContext>&& speculative_contexts,
                     const std::vector<OptReason>& opt_reasons
                     )
        : ClosureEvent(closureIndex),
        dispatch_context(dispatch_context),
        compileName(compileName),
        speculative_contexts(speculative_contexts),
        opt_reasons(opt_reasons)
        {}

    CompilationEvent() {}

    SEXP toSEXP() const override;
    void fromSEXP(SEXP sexp) override;
    virtual bool containsReference(size_t recordingIdx) const override;

  protected:
    void print(const std::vector<FunRecording>& mapping,
               std::ostream& out) const override;

  private:
    unsigned long dispatch_context;

    // Name under which the closure was compiled, to be passed to pirCompile()
    std::string compileName;

    std::vector<SpeculativeContext> speculative_contexts;
    std::vector<OptReason> opt_reasons;
};

class DeoptEvent : public VersionEvent {
  public:
    DeoptEvent(const DeoptEvent&) = delete;
    DeoptEvent& operator=(DeoptEvent const&);
    DeoptEvent(size_t dispatchTableIndex, Context version,
               DeoptReason::Reason reason,
               std::pair<ssize_t, ssize_t> reasonCodeIdx,
               uint32_t reasonCodeOff, SEXP trigger);
    ~DeoptEvent();
    void setTrigger(SEXP newTrigger);
    SEXP toSEXP() const override;
    void fromSEXP(SEXP file) override;
    virtual bool containsReference(size_t recordingIdx) const override;

  protected:
    void print(const std::vector<FunRecording>& mapping,
               std::ostream& out) const override;

  private:
    DeoptReason::Reason reason_;
    /* negative indicates promise index, positive function index */
    std::pair<ssize_t, ssize_t> reasonCodeIdx_;
    uint32_t reasonCodeOff_;

    // These 2 fields are mutually exclusive
    SEXP trigger_ = nullptr;
    ssize_t triggerClosure_ = -1; // References a FunRecorder index
};

class DtInitEvent : public DtEvent {
  public:
    DtInitEvent(size_t dtIndex, size_t invocations, size_t deopts)
        : DtEvent(dtIndex), invocations(invocations), deopts(deopts){};
    SEXP toSEXP() const override;
    void fromSEXP(SEXP file) override;

  protected:
    void print(const std::vector<FunRecording>& mapping,
               std::ostream& out) const override;

  private:
    size_t invocations, deopts;
};

class InvocationEvent : public VersionEvent {
  public:
    InvocationEvent(size_t dispatchTableIndex, Context version,
                    ssize_t deltaCount, size_t deltaDeopt)
        : VersionEvent(dispatchTableIndex, version), deltaCount(deltaCount),
          deltaDeopt(deltaDeopt){};

    InvocationEvent() : VersionEvent(){};

    SEXP toSEXP() const override;
    void fromSEXP(SEXP sexp) override;

  protected:
    void print(const std::vector<FunRecording>& mapping,
               std::ostream& out) const override;

  private:
    ssize_t deltaCount = 0;
    size_t deltaDeopt = 0;
};

// From names.c
extern "C" FUNTAB R_FunTab[];

constexpr size_t R_FunTab_Len_calc() {
    for (size_t i = 0;; i++) {
        if (R_FunTab[i].name == nullptr) {
            return i;
        }
    }
}

const size_t R_FunTab_Len = R_FunTab_Len_calc();

/**
 * R function (closure, builtin or special) to be persisted outside of the R
 * session
 */
struct FunRecording {
    // For CLOSXP:      -1
    // For primitives:  index into "names.c"'s array of primitive functions
    ssize_t primIdx = -1;

    /* possibly empty name of the closure */
    std::string name;
    /* possibly empty name of the environment in which the name was bound to the
     * closure */
    std::string env;
    /* the CLOSXP serialized into RAWSXP using the R_SerializeValue */
    SEXP closure = R_NilValue;

    // Just prints the name if the closure (or pointer if it has no name)
    friend std::ostream& operator<<(std::ostream& out,
                                    const FunRecording& that);

    FunRecording() = default;
    FunRecording(size_t primIdx) : primIdx(primIdx) {
        assert(primIdx < R_FunTab_Len);
        name = R_FunTab[primIdx].name;
    }
};

class Record {
    Record(const Record&) = delete;
    Record& operator=(const Record&) = delete;

    std::unordered_map<const DispatchTable*, size_t> dt_to_recording_index_;
    std::unordered_map<int, size_t> primitive_to_body_index;
    std::unordered_map<SEXP, size_t> bcode_to_body_index;
    std::vector<FunRecording> functions;

    std::vector<std::unique_ptr<Event>> log;

  protected:
    size_t indexOfBaseline(const rir::Code* code);

  public:
    Record() = default;
    ~Record();

    /**
     * Bitmask filter of events to record
     */
    struct {
        bool compile : 1;
        bool deopt : 1;
        bool typeFeedback : 1;
        bool invoke : 1;
        bool optReason : 1;
    } filter = {
        .compile = true,
        .deopt = true,
        .typeFeedback = false,
        .invoke = false,
        .optReason = true,
    };

    template <typename E, typename... Args>
    void record(SEXP cls, Args&&... args) {
        auto entry = initOrGetRecording(cls);
        log.emplace_back(
            std::make_unique<E>(entry.first, std::forward<Args>(args)...));
    }

    template <typename E, typename... Args>
    void record(SEXP cls, const std::string& name, Args&&... args) {
        auto entry = initOrGetRecording(cls, name);
        log.emplace_back(
            std::make_unique<E>(entry.first, std::forward<Args>(args)...));
    }

    template <typename E, typename... Args>
    void record(const DispatchTable* dt, Args&&... args) {
        auto entry = initOrGetRecording(dt);
        log.emplace_back(
            std::make_unique<E>(entry.first, std::forward<Args>(args)...));
    }

    template <typename E, typename... Args>
    void record(const DispatchTable* cls, const std::string& name,
                Args&&... args) {
        auto entry = initOrGetRecording(cls, name);
        log.emplace_back(
            std::make_unique<E>(entry.first, std::forward<Args>(args)...));
    }

    /**
     * Returns `true` if the list of recorded functions contains a closure whose
     * DispatchTable is the one given
     */
    bool contains(const DispatchTable* dt);

    std::pair<size_t, FunRecording&> initOrGetRecording(const DispatchTable* dt,
                                                        std::string name = "");

    std::pair<size_t, FunRecording&> initOrGetRecording(const SEXP cls,
                                                        std::string name = "");

    void recordSpeculativeContext(DispatchTable* dt,
                                  std::vector<SpeculativeContext>& ctx);

    void recordSpeculativeContext(const Code* code,
                                  std::vector<SpeculativeContext>& ctx);

    std::pair<ssize_t, ssize_t> findIndex(rir::Code* code, rir::Code* needle);
    SEXP save();
    void printRecordings(std::ostream& out);
    void reset() {
        dt_to_recording_index_.clear();
        functions.clear();
    }
};

// utilities
SEXP setClassName(SEXP s, const char* className);
bool stringStartsWith(const std::string& s, const std::string& prefix);
std::string getEnvironmentName(SEXP env);
SEXP getEnvironment(const std::string& name);

} // namespace recording

} // namespace rir

// R API
REXPORT SEXP startRecordings();
REXPORT SEXP stopRecordings();
REXPORT SEXP resetRecordings();
REXPORT SEXP isRecordings();
REXPORT SEXP saveRecordings(SEXP filename);
REXPORT SEXP loadRecordings(SEXP filename);
REXPORT SEXP getRecordings();
REXPORT SEXP printRecordings(SEXP filename, SEXP fromFile);

#endif
