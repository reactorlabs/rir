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

typedef std::uint32_t Idx;
#define NO_INDEX ((Idx)-1)

enum class SpeculativeContextType { Callees, Test, Values };

struct SpeculativeContext {
    SpeculativeContextType type;

    union Value {
        std::array<Idx, rir::ObservedCallees::MaxTargets> callees;
        ObservedTest test;
        ObservedValues values;
    } value;

    SpeculativeContext(decltype(value.callees) callees)
        : type{SpeculativeContextType::Callees}, value{.callees = callees} {}

    SpeculativeContext(ObservedTest test)
        : type{SpeculativeContextType::Test}, value{.test = test} {}

    SpeculativeContext(ObservedValues values)
        : type{SpeculativeContextType::Values}, value{.values = values} {}
};

class Event {
  public:
    friend std::ostream& operator<<(std::ostream& out, const Event& e);
    virtual SEXP toSEXP() const = 0;
    virtual void fromSEXP(SEXP sexp) = 0;
    virtual void replay(Replay& replay, SEXP closure,
                        std::string& closure_name) const = 0;
};

class CompilationEvent : public Event {
  public:
    CompilationEvent(unsigned long dispatch_context,
                     std::vector<SpeculativeContext>&& speculative_contexts)
        : dispatch_context(dispatch_context),
          speculative_contexts(speculative_contexts) {}
    SEXP toSEXP() const override;
    void fromSEXP(SEXP sexp) override;
    void replay(Replay& replay, SEXP closure,
                std::string& closure_name) const override;

  private:
    unsigned long dispatch_context;

    std::vector<SpeculativeContext> speculative_contexts;
};

class DeoptEvent : public Event {
  public:
    SEXP toSEXP() const override;
    void fromSEXP(SEXP file) override;
    void replay(Replay& replay, SEXP closure,
                std::string& closure_name) const override;
};

struct FunRecording {
    /* possibly empty name of the closure */
    std::string name;
    /* the CLOSXP serialized into RAWSXP using the R_SerializeValue*/
    SEXP closure;
    std::vector<std::unique_ptr<Event>> events;
};

class Replay {
    SEXP recordings_;
    SEXP rho_;
    std::vector<SEXP> closures_;

    SEXP replayClosure(Idx idx);

  public:
    void replaySpeculativeContext(
        DispatchTable* dt,
        std::vector<SpeculativeContext>::const_iterator& ctx);

    void replaySpeculativeContext(
        Code* code, std::vector<SpeculativeContext>::const_iterator& ctx);

    Replay(SEXP recordings, SEXP rho);

    ~Replay();

    size_t replay();
};

class Record {
    std::unordered_map<std::string, Idx> recordings_index_;
    std::vector<FunRecording> fun_recordings_;

  public:
    std::pair<Idx, FunRecording&> initOrGetRecording(const SEXP cls,
                                                     std::string name = "");
    void recordSpeculativeContext(DispatchTable* dt,
                                  std::vector<SpeculativeContext>& ctx);

    void recordSpeculativeContext(const Code* code,
                                  std::vector<SpeculativeContext>& ctx);

    size_t saveToFile(FILE* file);
};

// utilities
SEXP setClassName(SEXP s, const char* className);
std::string sexpAddress(const SEXP s);

// C++ API
void recordCompile(const SEXP cls, const std::string& name,
                   const Context& assumptions);
void recordDeopt(const SEXP cls);

} // namespace recording

} // namespace rir

// R API

REXPORT SEXP startRecording();
REXPORT SEXP stopRecording();
REXPORT SEXP isRecording();
REXPORT SEXP replayRecording(SEXP recordings, SEXP rho);
REXPORT SEXP replayRecordingFromFile(SEXP filename, SEXP rho);
REXPORT SEXP saveRecording(SEXP filename);

#endif
