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

// utilities
SEXP setClassName(SEXP s, const char* className);
bool stringStartsWith(const std::string& s, const std::string& prefix);
std::string getEnvironmentName(SEXP env);
SEXP getEnvironment(const std::string& name);

class Record;
struct FunRecording;

#define NO_INDEX ((size_t)-1)
#define GLOBAL_ENV_NAME ".GlobalEnv"

enum class SpeculativeContextType { Callees, Test, Values };

struct SpeculativeContext {
    SpeculativeContextType type;

    union Value {
        std::array<size_t, rir::ObservedCallees::MaxTargets> callees;
        ObservedTest test;
        ObservedValues values;
    } value;

    explicit SpeculativeContext(decltype(value.callees) callees)
        : type{SpeculativeContextType::Callees}, value{.callees = callees} {}

    explicit SpeculativeContext(ObservedTest test)
        : type{SpeculativeContextType::Test}, value{.test = test} {}

    explicit SpeculativeContext(const ObservedValues& values)
        : type{SpeculativeContextType::Values}, value{.values = values} {}

    void print(const std::vector<FunRecording>& mapping,
               std::ostream& out) const;
};

struct CompileReason {
    virtual SEXP toSEXP() const = 0;
    virtual void fromSEXP(SEXP sexp) = 0;
    virtual void print(std::ostream& out) const = 0;

    virtual ~CompileReason() = default;
};

template <typename Derived, size_t FieldsCount>
struct CompileReasonImpl : CompileReason {
    virtual ~CompileReasonImpl() = default;

    virtual SEXP toSEXP() const override {
        auto vec = PROTECT(Rf_allocVector(VECSXP, FieldsCount));
        setClassName(vec, Derived::NAME);

        UNPROTECT(1);
        return vec;
    }

    virtual void fromSEXP(SEXP sexp) override {
        assert(Rf_isVector(sexp));
        assert(Rf_length(sexp) == FieldsCount);
    }

    virtual void print(std::ostream& out) const override {
        out << Derived::NAME;
    }
};

struct MarkOptReason : public CompileReasonImpl<MarkOptReason, 0> {
    static constexpr const char * NAME = "MarkOpt";
    virtual ~MarkOptReason() = default;
};

struct InvocationCountTimeReason : public CompileReasonImpl<InvocationCountTimeReason, 4> {
    static constexpr const char * NAME = "InvocationCountTime";
    virtual ~InvocationCountTimeReason() = default;

    InvocationCountTimeReason(size_t count, size_t minimalCount,
                              unsigned long time, unsigned long minimalTime)
        : count(count), minimalCount(minimalCount), time(time),
          minimalTime(minimalTime) {}

    InvocationCountTimeReason() {}

    size_t count = 0;
    size_t minimalCount = 0;
    unsigned long time = 0;
    unsigned long minimalTime = 0;

    virtual SEXP toSEXP() const override;
    virtual void fromSEXP(SEXP sexp) override;

    virtual void print(std::ostream& out) const override {
        this->CompileReasonImpl::print(out);

        out << ", count=" << count << ", minimalCount=" << minimalCount
            << ", time=" << time << ", minimalTime=" << minimalTime;
    }
};

struct PirWarmupReason : public CompileReasonImpl<PirWarmupReason, 1> {
    static constexpr const char * NAME = "PirWarmupReason";
    virtual ~PirWarmupReason() = default;

    explicit PirWarmupReason(size_t invocationCount)
        : invocationCount(invocationCount) {}

    PirWarmupReason() {}

    size_t invocationCount = 0;

    virtual SEXP toSEXP() const override;
    virtual void fromSEXP(SEXP sexp) override;

    virtual void print(std::ostream& out) const override {
        this->CompileReasonImpl::print(out);

        out << ", invocationCount=" << invocationCount;
    }
};

struct NotOptimizedReason : public CompileReasonImpl<NotOptimizedReason, 0> {
    virtual ~NotOptimizedReason() = default;
    static constexpr const char * NAME = "NotOptimized";
};

struct IsImprovingReason : public CompileReasonImpl<IsImprovingReason, 0> {
    virtual ~IsImprovingReason() = default;
    static constexpr const char * NAME = "IsImproving";
};

struct ReoptimizeFlagReason : public CompileReasonImpl<ReoptimizeFlagReason, 0> {
    virtual ~ReoptimizeFlagReason() = default;
    static constexpr const char * NAME = "ReoptimizeFlag";
};

struct OSRCallerCalleeReason : public CompileReasonImpl<OSRCallerCalleeReason, 0>{
    virtual ~OSRCallerCalleeReason() = default;
    static constexpr const char * NAME = "OSRCallerCallee";
};

struct OSRLoopReason : public CompileReasonImpl<OSRLoopReason, 1>{
    virtual ~OSRLoopReason() = default;
    static constexpr const char * NAME = "OSRLoop";

    explicit OSRLoopReason(size_t loopCount)
        : loopCount(loopCount) {}

    OSRLoopReason() {}

    size_t loopCount = 0;

    virtual SEXP toSEXP() const override;
    virtual void fromSEXP(SEXP sexp) override;

    virtual void print(std::ostream& out) const override {
        this->CompileReasonImpl::print(out);

        out << ", loopCount=" << loopCount;
    }
};

struct CompileReasons {
    CompileReasons()
        : heuristic(nullptr), condition(nullptr), osr(nullptr) {}

    CompileReasons(CompileReasons&& other)
        : heuristic(std::move(other.heuristic)),
          condition(std::move(other.condition)), osr(std::move(other.osr)) {}

    std::unique_ptr<CompileReason> heuristic;
    std::unique_ptr<CompileReason> condition;
    std::unique_ptr<CompileReason> osr;

    template <typename T, typename... Args>
    void set_heuristic(Args&&... args) {
        heuristic = std::make_unique<T>(std::forward<Args>(args)...);
    }

    template <typename T, typename... Args>
    void set_condition(Args&&... args) {
        condition = std::make_unique<T>(std::forward<Args>(args)...);
    }

    template<typename T, typename... Args>
    void set_osr(Args&&... args) {
        osr = std::make_unique<T>(std::forward<Args>(args)...);
    }

};

/**
 * Recorded event
 *
 * `Event` is an abstract class.
 */
class Event {
  public:
    virtual ~Event() = default;

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
    targetName(const std::vector<FunRecording>& mapping) const = 0;
};

/**
 * Recorded event that is implicitly attached to a CLOSXP-typed SEXP
 *
 * `ClosureEvent` is an abstract class.
 */
class ClosureEvent : public Event {
  public:
    virtual ~ClosureEvent() = default;

  protected:
    ClosureEvent() = default;
    explicit ClosureEvent(size_t closureIndex) : closureIndex(closureIndex){};

    size_t closureIndex;

    virtual bool containsReference(size_t recordingIdx) const override {
        return recordingIdx == closureIndex;
    };

    const char* targetName(const std::vector<FunRecording>& mapping) const override;
};

/**
 * Recorded event that is implicitly attached to a CLOSXP's dispatch table
 *
 * `DtEvent` is an abstract class.
 */
class DtEvent : public Event {
  public:
    virtual ~DtEvent() = default;

  protected:
    DtEvent() = default;
    explicit DtEvent(size_t dispatchTableIndex)
        : dispatchTableIndex(dispatchTableIndex){};

    size_t dispatchTableIndex;

    virtual bool containsReference(size_t recordingIdx) const override {
        return recordingIdx == dispatchTableIndex;
    };

    const char* targetName(const std::vector<FunRecording>& mapping) const override;
};

/**
 * `FunctionEvent`s are `Event`s that relate to a closure (instead of any of the
 * 3 function kinds), but also to a specific function version inside its
 * DispatchTable.
 *
 * `VersionEvent` is an abstract class.
 */
class VersionEvent : public DtEvent {
  public:
    virtual ~VersionEvent() = default;

  protected:
    VersionEvent() = default;
    VersionEvent(size_t dispatchTableIndex, Context version)
        : DtEvent(dispatchTableIndex), version(version){};

    Context version = Context();
};

/**
 * Notifies an update to a speculative context
 */
class SpeculativeContextEvent : public DtEvent {
  public:
    SpeculativeContextEvent(size_t dispatchTableIndex, ssize_t codeIndex,
                            size_t offset, const SpeculativeContext& sc)
        : DtEvent(dispatchTableIndex), codeIndex(codeIndex), offset(offset),
          sc(sc) {}
    SpeculativeContextEvent()
        : codeIndex(-2), offset(0), sc(SpeculativeContext({0, 0, 0})) {}

    virtual ~SpeculativeContextEvent() = default;

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
    CompilationEvent(size_t closureIndex, unsigned long dispatch_context,
                     const std::string& compileName,
                     std::vector<SpeculativeContext>&& speculative_contexts,
                     CompileReasons&& compile_reasons)
        : ClosureEvent(closureIndex), dispatch_context(dispatch_context),
          compileName(compileName),
          speculative_contexts(std::move(speculative_contexts)),
          compile_reasons(std::move(compile_reasons)) {}

    CompilationEvent() {}

    virtual ~CompilationEvent() = default;

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
    CompileReasons compile_reasons;
};

class DeoptEvent : public VersionEvent {
  public:
    DeoptEvent(const DeoptEvent&) = delete;
    DeoptEvent& operator=(DeoptEvent const&);
    DeoptEvent(size_t dispatchTableIndex, Context version,
               DeoptReason::Reason reason,
               std::pair<ssize_t, ssize_t> reasonCodeIdx,
               uint32_t reasonCodeOff, SEXP trigger);
    virtual ~DeoptEvent();

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

    virtual ~DtInitEvent() = default;

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

    virtual ~InvocationEvent() = default;

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

inline size_t R_FunTab_Len_calc() {
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
    explicit FunRecording(size_t primIdx) : primIdx(primIdx) {
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

public:
    std::vector<FunRecording> functions;

    std::vector<std::unique_ptr<Event>> log;

  protected:
    size_t indexOfBaseline(const rir::Code* code);

  public:
    Record() = default;
    ~Record();

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
                                                        const std::string& name = "");

    std::pair<size_t, FunRecording&> initOrGetRecording(const SEXP cls,
                                                        const std::string& name = "");

    void recordSpeculativeContext(DispatchTable* dt,
                                  std::vector<SpeculativeContext>& ctx);

    void recordSpeculativeContext(const Code* code,
                                  std::vector<SpeculativeContext>& ctx);

    std::pair<ssize_t, ssize_t> findIndex(rir::Code* code, rir::Code* needle);
    SEXP save();

    void reset() {
        dt_to_recording_index_.clear();
        functions.clear();
    }
};

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
REXPORT SEXP printRecordings(SEXP from);
REXPORT SEXP printEventPart(SEXP obj, SEXP type);

#endif
