#ifndef RECORDING_H
#define RECORDING_H

#include "api.h"
#include "compiler/pir/closure_version.h"
#include "compiler/pir/pir.h"
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

class Replay;
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

class Event {
  public:
    virtual SEXP toSEXP() const = 0;
    virtual void fromSEXP(SEXP sexp) = 0;
    virtual void replay(Replay& replay, SEXP closure,
                        std::string& closure_name) const = 0;
    virtual void print(const std::vector<FunRecording>& mapping,
                       std::ostream& out) const = 0;

    /**
     * Returns true if the event contains a reference to a given DT index
     */
    virtual bool containsReference(size_t dispatchTable) const { return false; }
};

/**
 * Notifies an update to a speculative context
 */
class SpeculativeContextEvent : public Event {
  public:
    SpeculativeContextEvent(ssize_t codeIndex, size_t offset,
                            SpeculativeContext sc)
        : codeIndex(codeIndex), offset(offset), sc(sc) {}
    SpeculativeContextEvent()
        : codeIndex(-2), offset(0), sc(SpeculativeContext({0, 0, 0})) {}
    SEXP toSEXP() const override;
    void fromSEXP(SEXP sexp) override;
    void replay(Replay& replay, SEXP closure,
                std::string& closure_name) const override;
    virtual bool containsReference(size_t dispatchTable) const;

  protected:
    void print(const std::vector<FunRecording>& mapping,
               std::ostream& out) const override;

  private:
    // -1 for function body itself, n≥0 for promise index
    ssize_t codeIndex;
    size_t offset;
    SpeculativeContext sc;
};

class CompilationEvent : public Event {
  public:
    CompilationEvent(unsigned long dispatch_context,
                     std::vector<SpeculativeContext>&& speculative_contexts)
        : dispatch_context(dispatch_context),
          speculative_contexts(speculative_contexts) {}
    CompilationEvent() {}

    SEXP toSEXP() const override;
    void fromSEXP(SEXP sexp) override;
    void replay(Replay& replay, SEXP closure,
                std::string& closure_name) const override;
    virtual bool containsReference(size_t dispatchTable) const;

  protected:
    void print(const std::vector<FunRecording>& mapping,
               std::ostream& out) const override;

  private:
    unsigned long dispatch_context;

    std::vector<SpeculativeContext> speculative_contexts;
};

class DeoptEvent : public Event {
  public:
    DeoptEvent(const DeoptEvent&) = delete;
    DeoptEvent& operator=(DeoptEvent const&);
    DeoptEvent(size_t functionIdx, DeoptReason::Reason reason,
               std::pair<ssize_t, ssize_t> reasonCodeIdx,
               uint32_t reasonCodeOff, SEXP trigger);
    ~DeoptEvent();
    void setTrigger(SEXP newTrigger);
    SEXP toSEXP() const override;
    void fromSEXP(SEXP file) override;
    void replay(Replay& replay, SEXP closure,
                std::string& closure_name) const override;
    virtual bool containsReference(size_t dispatchTable) const;

  protected:
    void print(const std::vector<FunRecording>& mapping,
               std::ostream& out) const override;

  private:
    /* 0 if it couldn't be found */
    size_t functionIdx_;
    DeoptReason::Reason reason_;
    /* negative indicates promise index, positive function index */
    std::pair<ssize_t, ssize_t> reasonCodeIdx_;
    uint32_t reasonCodeOff_;

    // These 2 fields are mutually exclusive
    SEXP trigger_ = nullptr;
    ssize_t triggerClosure_ = -1; // References a FunRecorder index
};

class DtInitEvent : public Event {
  public:
    DtInitEvent(size_t invocations, size_t deopts)
        : invocations(invocations), deopts(deopts){};
    SEXP toSEXP() const override;
    void fromSEXP(SEXP file) override;
    void replay(Replay& replay, SEXP closure,
                std::string& closure_name) const override;

  protected:
    void print(const std::vector<FunRecording>& mapping,
               std::ostream& out) const override;

  private:
    size_t invocations, deopts;
};

class InvocationEvent : public Event {
  public:
    InvocationEvent(size_t dtSize, size_t funIdx, ssize_t deltaCount,
                    size_t deltaDeopt)
        : dtSize(dtSize), funIdx(funIdx), deltaCount(deltaCount),
          deltaDeopt(deltaDeopt){};

    InvocationEvent() : dtSize(0){};

    SEXP toSEXP() const override;
    void fromSEXP(SEXP sexp) override;
    void replay(Replay& replay, SEXP closure,
                std::string& closure_name) const override;

  protected:
    void print(const std::vector<FunRecording>& mapping,
               std::ostream& out) const override;

  private:
    size_t dtSize;
    size_t funIdx = (size_t)-1;
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

class Replay {
    SEXP log;

  public:
    std::vector<FunRecording> functions;
    std::vector<DispatchTable*> rehydrated_dispatch_tables;
    std::vector<SEXP> rehydrated_closures;

    SEXP replayClosure(size_t idx);

    void replaySpeculativeContext(
        DispatchTable* dt,
        std::vector<SpeculativeContext>::const_iterator& ctxStart,
        std::vector<SpeculativeContext>::const_iterator& ctxEnd);

    void replaySpeculativeContext(
        Code* code, std::vector<SpeculativeContext>::const_iterator& ctxStart,
        std::vector<SpeculativeContext>::const_iterator& ctxEnd);

    Replay(SEXP recordings);

    ~Replay();

    size_t getEventCount();
    std::pair<size_t, std::unique_ptr<Event>> getEvent(size_t idx);

    size_t replay();
};

class Record {
    Record(const Record&) = delete;
    Record& operator=(const Record&) = delete;

    std::unordered_map<const DispatchTable*, size_t> dt_to_recording_index_;
    std::unordered_map<int, size_t> primitive_to_body_index;
    std::unordered_map<SEXP, size_t> bcode_to_body_index;
    std::vector<FunRecording> functions;

    std::vector<std::pair<size_t, std::unique_ptr<Event>>> log;

  protected:
    size_t indexOfBaseline(const rir::Code* code);

  public:
    Record() = default;
    ~Record();

    struct {
        bool compile : 1;
        bool deopt : 1;
        bool typeFeedback : 1;
        bool invoke : 1;
    } filter = {
        .compile = true,
        .deopt = true,
        .typeFeedback = false,
        .invoke = false,
    };

    void record(const DispatchTable* dt, std::unique_ptr<Event> event);
    void record(const SEXP cls, std::string name, std::unique_ptr<Event> event);
    void record(const SEXP cls, std::unique_ptr<Event> event);

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

// C++ API
void recordCompile(const SEXP cls, const std::string& name,
                   const Context& assumptions);
void recordDeopt(rir::Code* c, const SEXP cls, DeoptReason& reason,
                 SEXP trigger);
void recordDtOverwrite(const DispatchTable* dt, size_t funIdx,
                       size_t oldDeoptCount);
void recordInvocation(const Function* f, ssize_t deltaCount, size_t deltaDeopt);
void prepareRecordSC(const Code* container);
void recordSC(const ObservedCallees& type);
void recordSC(const ObservedTest& type);
void recordSC(const ObservedValues& type);

} // namespace recording

} // namespace rir

// R API
REXPORT SEXP startRecordings();
REXPORT SEXP stopRecordings();
REXPORT SEXP resetRecordings();
REXPORT SEXP isRecordings();
REXPORT SEXP replayRecordings(SEXP recordings, bool startRecording);
REXPORT SEXP replayRecordingsFromFile(SEXP filename, bool startRecording);
REXPORT SEXP saveRecordings(SEXP filename);
REXPORT SEXP loadRecordings(SEXP filename);
REXPORT SEXP getRecordings();
REXPORT SEXP printRecordings(SEXP filename, SEXP fromFile);

#endif
