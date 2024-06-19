#ifndef RECORDING_H
#define RECORDING_H

#ifdef RECORDING

#include "api.h"
#include "compiler/pir/closure_version.h"
#include "compiler/pir/pir.h"
#include "recording_hooks.h"
#include "runtime/Context.h"
#include "runtime/DispatchTable.h"
#include "runtime/TypeFeedback.h"
#include <R/r.h>
#include <array>
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <memory>
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
std::string getClosureName(SEXP cls);

class Record;
struct FunRecording;

constexpr size_t NO_INDEX = ((size_t)-1);
constexpr const char* GLOBAL_ENV_NAME = ".GlobalEnv";

enum class SpeculativeContextType { Callees, Test, Values };

struct SpeculativeContext {
    SpeculativeContextType type;
    using ObservedCalleesArr =
        std::array<size_t, rir::ObservedCallees::MaxTargets>;

    union Value {
        ObservedCalleesArr callees;
        ObservedTest test;
        ObservedValues values;
    } value;

    explicit SpeculativeContext(ObservedCalleesArr callees)
        : type{SpeculativeContextType::Callees}, value{.callees = callees} {}

    explicit SpeculativeContext(ObservedTest test)
        : type{SpeculativeContextType::Test}, value{.test = test} {}

    explicit SpeculativeContext(const ObservedValues& values)
        : type{SpeculativeContextType::Values}, value{.values = values} {}

    void print(const std::vector<FunRecording>& mapping,
               std::ostream& out) const;
};

// TODO unify serialization with event
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
    static constexpr const char* NAME = "MarkOpt";
    virtual ~MarkOptReason() = default;
};

struct PirWarmupReason : public CompileReasonImpl<PirWarmupReason, 1> {
    static constexpr const char* NAME = "PirWarmupReason";
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
    static constexpr const char* NAME = "NotOptimized";
};

struct IsImprovingReason : public CompileReasonImpl<IsImprovingReason, 0> {
    virtual ~IsImprovingReason() = default;
    static constexpr const char* NAME = "IsImproving";
};

struct ReoptimizeFlagReason
    : public CompileReasonImpl<ReoptimizeFlagReason, 0> {
    virtual ~ReoptimizeFlagReason() = default;
    static constexpr const char* NAME = "ReoptimizeFlag";
};

struct OSRCallerCalleeReason
    : public CompileReasonImpl<OSRCallerCalleeReason, 0> {
    virtual ~OSRCallerCalleeReason() = default;
    static constexpr const char* NAME = "OSRCallerCallee";
};

struct OSRLoopReason : public CompileReasonImpl<OSRLoopReason, 1> {
    virtual ~OSRLoopReason() = default;
    static constexpr const char* NAME = "OSRLoop";

    explicit OSRLoopReason(size_t loopCount) : loopCount(loopCount) {}

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
    CompileReasons() : heuristic(nullptr), condition(nullptr), osr(nullptr) {}

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

    template <typename T, typename... Args>
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

    const char*
    targetName(const std::vector<FunRecording>& mapping) const override;
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

    const char*
    targetName(const std::vector<FunRecording>& mapping) const override;
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
    SpeculativeContextEvent(size_t dispatchTableIndex, bool isPromise,
                            size_t index, const SpeculativeContext& sc,
                            bool changed)
        : DtEvent(dispatchTableIndex), is_promise(isPromise), index(index),
          sc(sc), changed(changed) {}

    SpeculativeContextEvent() = default;

    virtual ~SpeculativeContextEvent() = default;

    SEXP toSEXP() const override;
    void fromSEXP(SEXP sexp) override;

    static const std::vector<const char*> fieldNames;
    static constexpr const char* className = "event_sc";

  protected:
    void print(const std::vector<FunRecording>& mapping,
               std::ostream& out) const override;

  private:
    bool is_promise;
    // Index of the slot
    size_t index;
    SpeculativeContext sc = SpeculativeContext({0});
    bool changed;
};

class CompilationEvent : public ClosureEvent {
  public:
    using Clock = std::chrono::steady_clock;
    using Time = std::chrono::time_point<Clock>;
    using Duration = std::chrono::milliseconds;

    CompilationEvent(size_t closureIndex, unsigned long dispatch_context,
                     const std::string& compileName,
                     std::vector<SpeculativeContext>&& speculative_contexts,
                     CompileReasons&& compile_reasons)
        : ClosureEvent(closureIndex), dispatch_context(dispatch_context),
          compileName(compileName),
          speculative_contexts(std::move(speculative_contexts)),
          compile_reasons(std::move(compile_reasons)) {}

    CompilationEvent(CompilationEvent&& other) = default;

    CompilationEvent() {}

    virtual ~CompilationEvent() = default;

    SEXP toSEXP() const override;
    void fromSEXP(SEXP sexp) override;

    static const std::vector<const char*> fieldNames;
    static constexpr const char* className = "event_compile";

    void set_time(Duration time) { time_length = time; }

    void add_subcompilation(size_t idx) { subevents.push_back(idx); }

    void set_bitcode(const std::string& str) { bitcode = str; }

    void set_success(bool succes) { succesful = succes; }

  protected:
    void print(const std::vector<FunRecording>& mapping,
               std::ostream& out) const override;

  private:
    unsigned long dispatch_context; // TODO keep as a Context

    // Name under which the closure was compiled, to be passed to pirCompile()
    std::string compileName;

    std::vector<SpeculativeContext> speculative_contexts;
    CompileReasons compile_reasons;

    // Benchmarking
    Duration time_length;

    std::vector<size_t> subevents;

    // The LLVM Bitcode
    std::string bitcode;

    bool succesful = false;
};

class DeoptEvent : public VersionEvent {
  public:
    DeoptEvent(const DeoptEvent&) = delete;
    DeoptEvent& operator=(DeoptEvent const&);
    DeoptEvent(size_t dispatchTableIndex, Context version,
               DeoptReason::Reason reason, size_t reasonCodeIdx,
               ssize_t reasonPromiseIdx, uint32_t reasonCodeOff, SEXP trigger);
    virtual ~DeoptEvent();
    DeoptEvent() = default;

    void setTrigger(SEXP newTrigger);
    SEXP toSEXP() const override;
    void fromSEXP(SEXP file) override;

    static const std::vector<const char*> fieldNames;
    static constexpr const char* className = "event_deopt";

  protected:
    void print(const std::vector<FunRecording>& mapping,
               std::ostream& out) const override;

  private:
    DeoptReason::Reason reason_;
    size_t reasonCodeIdx_;
    // If it is a promise (>= 0), this is the index
    // in the extraEntryPools
    ssize_t reasonPromiseIdx_;

    uint32_t reasonCodeOff_;

    // These 2 fields are mutually exclusive
    SEXP trigger_ = nullptr;
    ssize_t triggerClosure_ = -1; // References a FunRecorder index
};

class InvocationEvent : public VersionEvent {
  public:
    enum Source : uint8_t {
        DoCall,
        NativeCallTrampoline,
        FIRST = DoCall,
        LAST = NativeCallTrampoline
    };

    using SourceSet = EnumSet<Source, uint8_t>;

    InvocationEvent(size_t dispatchTableIndex, Context version,
                    SourceSet source)
        : VersionEvent(dispatchTableIndex, version), source(source) {}

    InvocationEvent() : VersionEvent(){};

    virtual ~InvocationEvent() = default;

    SEXP toSEXP() const override;
    void fromSEXP(SEXP sexp) override;

    static const std::vector<const char*> fieldNames;
    static constexpr const char* className = "event_invocation";

  protected:
    void print(const std::vector<FunRecording>& mapping,
               std::ostream& out) const override;

  private:
    SourceSet source = SourceSet::None();
};

class UnregisterInvocationEvent : public VersionEvent {
  public:
    UnregisterInvocationEvent(size_t dispatchTableIndex, Context version)
        : VersionEvent(dispatchTableIndex, version) {}

    UnregisterInvocationEvent() : VersionEvent() {}

    virtual ~UnregisterInvocationEvent() = default;

    SEXP toSEXP() const override;
    void fromSEXP(SEXP sexp) override;

    static const std::vector<const char*> fieldNames;
    static constexpr const char* className = "event_unregister_invocation";

  protected:
    void print(const std::vector<FunRecording>& mapping,
               std::ostream& out) const override;
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
    explicit FunRecording(size_t primIdx, SEXP closure)
        : primIdx(primIdx), closure(closure) {
        assert(primIdx < R_FunTab_Len);
        name = R_FunTab[primIdx].name;
    }

    explicit FunRecording(const std::string& name, const std::string& env,
                          SEXP closure)
        : name(name), env(env), closure(closure) {}

    explicit FunRecording(const std::string& name) : name(name) {}
};

class Record {
    Record(const Record&) = delete;
    Record& operator=(const Record&) = delete;

    std::unordered_map<const DispatchTable*, size_t> dt_to_recording_index_;
    std::unordered_map<int, size_t> primitive_to_body_index;
    std::unordered_map<SEXP, size_t> bcode_to_body_index;

  public:
    std::vector<FunRecording> functions;

    // TODO deque (?)
    std::vector<std::unique_ptr<Event>> log;

  public:
    Record() = default;
    ~Record();

    template <typename E, typename... Args>
    void record(const DispatchTable* dt, Args&&... args) {
        auto entry = initOrGetRecording(dt);
        log.emplace_back(
            std::make_unique<E>(entry, std::forward<Args>(args)...));
    }

    size_t push_event(std::unique_ptr<Event> e) {
        size_t idx = log.size();
        log.emplace_back(std::move(e));
        return idx;
    }

    FunRecording& get_recording(size_t idx) { return functions[idx]; }

    // Can return just size_t
    size_t initOrGetRecording(const DispatchTable* dt,
                              const std::string& name = "");

    // Can just return size_t
    size_t initOrGetRecording(const SEXP cls, const std::string& name = "");

    // First is a fun recording index, second is -1 if it is baseline,
    // otherwise promise idx
    std::pair<size_t, ssize_t> findIndex(rir::Code* code, rir::Code* needle);
    SEXP save();

    void reset() {
        dt_to_recording_index_.clear();
        functions.clear();
    }
};

} // namespace recording

} // namespace rir

#endif // RECORDING
#endif
