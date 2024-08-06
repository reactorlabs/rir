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

constexpr size_t NO_INDEX = (size_t)-1;
constexpr size_t PROMISE_INDEX = (size_t)-2;
constexpr const char* GLOBAL_ENV_NAME = ".GlobalEnv";

// Controls if SEXP closures should be serialized
const bool SERIALIZE_SEXP = getenv("RIR_RECORD_SERIALIZE")
                                     ? atoi(getenv("RIR_RECORD_SERIALIZE"))
                                     : false;

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
    Event() = default;
    Event(size_t funRecIndex) : funRecIndex_(funRecIndex) {}

    virtual ~Event() = default;

    virtual SEXP toSEXP() const = 0;
    virtual void fromSEXP(SEXP sexp) = 0;

    size_t funRecIndex() const { return funRecIndex_; }

  protected:
    size_t funRecIndex_;
};

/**
 * `VersionEvent`s are `Event`s that relate to a closure and also to a specific
 * function version inside its DispatchTable.
 *
 * `VersionEvent` is an abstract class.
 */
class VersionEvent : public Event {
  public:
    virtual ~VersionEvent() = default;

  protected:
    VersionEvent() = default;
    VersionEvent(size_t dispatchTableIndex, Context version)
        : Event(dispatchTableIndex), version(version){};

    Context version = Context();
};

/**
 * Notifies an update to a speculative context
 */
class SpeculativeContextEvent : public Event {
  public:
    SpeculativeContextEvent(size_t dispatchTableIndex, bool isPromise,
                            size_t index, const SpeculativeContext& sc,
                            bool changed)
        : Event(dispatchTableIndex), is_promise(isPromise), index(index),
          sc(sc), changed(changed) {}

    SpeculativeContextEvent() = default;

    virtual ~SpeculativeContextEvent() = default;

    SEXP toSEXP() const override;
    void fromSEXP(SEXP sexp) override;

    static const std::vector<const char*> fieldNames;
    static constexpr const char* className = "event_sc";

  private:
    bool is_promise;
    // Index of the slot
    size_t index;
    SpeculativeContext sc = SpeculativeContext({0});
    bool changed;
};

class CompilationStartEvent : public Event {
  public:
    CompilationStartEvent(size_t funRecIndex, const std::string& compileName,
                          CompileReasons&& reasons)
        : Event(funRecIndex), compileName(compileName),
          compile_reasons(std::move(reasons)) {}

    CompilationStartEvent(){};

    virtual ~CompilationStartEvent() = default;

    SEXP toSEXP() const override;
    void fromSEXP(SEXP sexp) override;

    static const std::vector<const char*> fieldNames;
    static constexpr const char* className = "event_compile_start";

  private:
    // Name under which the closure was compiled, to be passed to pirCompile()
    std::string compileName;
    CompileReasons compile_reasons;
};

class CompilationEvent : public VersionEvent {
  public:
    CompilationEvent(size_t funRecIndex, Context version,
                     std::vector<SpeculativeContext>&& speculative_contexts,
                     const std::string& bitcode, const std::string& pir_code,
                     size_t deopt_count)
        : VersionEvent(funRecIndex, version),
          speculative_contexts(std::move(speculative_contexts)),
          bitcode(bitcode), pir_code(pir_code), deopt_count(deopt_count) {}

    CompilationEvent() {}

    virtual ~CompilationEvent() = default;

    SEXP toSEXP() const override;
    void fromSEXP(SEXP sexp) override;

    static const std::vector<const char*> fieldNames;
    static constexpr const char* className = "event_compile";

  private:
    std::vector<SpeculativeContext> speculative_contexts;

    // The LLVM Bitcode
    std::string bitcode;
    std::string pir_code;
    size_t deopt_count;
};

class CompilationEndEvent : public Event {
  public:
    using Clock = std::chrono::steady_clock;
    using Time = std::chrono::time_point<Clock>;
    using Duration = std::chrono::milliseconds;

    CompilationEndEvent(){};
    CompilationEndEvent(size_t funRecIndex, Duration duration, bool succesful)
        : Event(funRecIndex), time_length(duration), succesful(succesful){};

    SEXP toSEXP() const override;
    void fromSEXP(SEXP sexp) override;

    static const std::vector<const char*> fieldNames;
    static constexpr const char* className = "event_compile_end";

  private:
    // Benchmarking
    Duration time_length;

    bool succesful;
};

class DeoptEvent : public VersionEvent {
  public:
    DeoptEvent(const DeoptEvent&) = delete;
    DeoptEvent(size_t dispatchTableIndex, Context version,
               DeoptReason::Reason reason, size_t origin_function,
               FeedbackIndex index, SEXP trigger, ssize_t trigger_index)
        : VersionEvent(dispatchTableIndex, version), reason(reason),
          origin_function(origin_function), index(index), trigger(trigger),
          trigger_index(trigger_index) {
        if (trigger != R_NilValue) {
            R_PreserveObject(trigger);
        }
    }

    virtual ~DeoptEvent() {
        if (trigger != R_NilValue) {
            R_ReleaseObject(trigger);
        }
    }

    DeoptEvent() = default;

    SEXP toSEXP() const override;
    void fromSEXP(SEXP file) override;

    static const std::vector<const char*> fieldNames;
    static constexpr const char* className = "event_deopt";

  private:
    DeoptReason::Reason reason = DeoptReason::Reason::Unknown;
    size_t origin_function;
    FeedbackIndex index;

    // Either trigger is R_NilValue or trigger_index is -1
    SEXP trigger = R_NilValue;
    ssize_t trigger_index = -1;
};

class InvocationEvent : public VersionEvent {
  public:
    enum Source : uint8_t { Unknown, DoCall, NativeCallTrampoline, RirEval };

    InvocationEvent(size_t dispatchTableIndex, Context version, Source source,
                    Context callContext, bool isNative, uintptr_t address,
                    bool missingAsmptPresent, bool missingAsmptRecovered)
        : VersionEvent(dispatchTableIndex, version), source(source),
          callContext(callContext), isNative(isNative), address(address),
          missingAsmptPresent(missingAsmptPresent),
          missingAsmptRecovered(missingAsmptRecovered) {}

    InvocationEvent() : VersionEvent(){};

    virtual ~InvocationEvent() = default;

    SEXP toSEXP() const override;
    void fromSEXP(SEXP sexp) override;

    static const std::vector<const char*> fieldNames;
    static constexpr const char* className = "event_invocation";

  private:
    Source source = Unknown;
    Context callContext;
    bool isNative = false;
    uintptr_t address = 0;
    bool missingAsmptPresent = false;
    bool missingAsmptRecovered = false;
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
};

class CustomEvent : public Event {
  public:
    explicit CustomEvent(const std::string& name) : Event(), name(name) {}

    CustomEvent() = default;

    virtual ~CustomEvent() = default;

    SEXP toSEXP() const override;
    void fromSEXP(SEXP sexp) override;

    static const std::vector<const char*> fieldNames;
    static constexpr const char* className = "event_custom";

  private:
    std::string name;
};

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
    std::unordered_map<Function*, size_t> expr_to_body_index;

  public:
    std::vector<FunRecording> functions;

    // TODO deque (?)
    std::vector<std::unique_ptr<Event>> log;

  public:
    Record() = default;

    ~Record() { release(); }

    template <typename E, typename... Args>
    void record(SEXP cls, Args&&... args) {
        assert(cls != nullptr);
        auto entry = initOrGetRecording(cls);
        log.emplace_back(
            std::make_unique<E>(entry, std::forward<Args>(args)...));
    }

    template <typename E, typename... Args>
    void record(const DispatchTable* dt, Args&&... args) {
        auto entry = initOrGetRecording(dt);
        log.emplace_back(
            std::make_unique<E>(entry, std::forward<Args>(args)...));
    }

    template <typename E, typename... Args>
    void record(Function* fun, Args&&... args) {
        auto entry = initOrGetRecording(fun);
        log.emplace_back(
            std::make_unique<E>(entry, std::forward<Args>(args)...));
    }

    size_t push_event(std::unique_ptr<Event> e) {
        size_t idx = log.size();
        log.emplace_back(std::move(e));
        return idx;
    }

    FunRecording& get_recording(size_t idx) { return functions[idx]; }

    size_t initOrGetRecording(const DispatchTable* dt);
    size_t initOrGetRecording(const SEXP cls, const std::string& name = "");
    size_t initOrGetRecording(Function* fun);

    SEXP save();

    void release() {
        for (auto dt : dt_to_recording_index_) {
            R_ReleaseObject(dt.first->container());
        }

        for (auto bcode : bcode_to_body_index) {
            R_ReleaseObject(bcode.first);
        }

        for (auto expr : expr_to_body_index) {
            R_ReleaseObject(expr.first->container());
        }

        for (auto fun : functions) {
            auto clos = fun.closure;
            if (!Rf_isNull(clos)) {
                R_ReleaseObject(clos);
            }
        }
    }

    void reset() {
        release();
        dt_to_recording_index_.clear();
        primitive_to_body_index.clear();
        bcode_to_body_index.clear();
        functions.clear();
        expr_to_body_index.clear();
        log.clear();
    }
};

} // namespace recording

} // namespace rir

#endif // RECORDING
#endif
