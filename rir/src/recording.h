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
    virtual SEXP to_sexp() const = 0;
    virtual void init_from_sexp(SEXP sexp) = 0;
    virtual void replay(Replay& replay, SEXP closure) const = 0;
};

class CompilationEvent : public Event {
  public:
    CompilationEvent(unsigned long dispatch_context,
                     std::vector<SpeculativeContext>&& speculative_contexts)
        : dispatch_context(dispatch_context),
          speculative_contexts(speculative_contexts) {}
    SEXP to_sexp() const override;
    void init_from_sexp(SEXP sexp) override;
    void replay(Replay& replay, SEXP closure) const override;

  private:
    unsigned long dispatch_context;

    std::vector<SpeculativeContext> speculative_contexts;
};

class DeoptEvent : public Event {
  public:
    SEXP to_sexp() const override;
    void init_from_sexp(SEXP file) override;
    void replay(Replay& replay, SEXP closure) const override;
};

struct FunRecorder {
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

    Replay(SEXP recordings, SEXP rho) : recordings_(recordings), rho_(rho) {
        PROTECT(recordings_);
        PROTECT(rho_);

        assert(Rf_isVector(recordings_));
        assert(Rf_isEnvironment(rho_));

        auto n = Rf_length(recordings_);
        closures_.reserve(n);
        for (auto i = 0; i < n; i++) {
            closures_.push_back(R_NilValue);
        }
    }

    ~Replay() {
        UNPROTECT_PTR(recordings_);
        UNPROTECT_PTR(rho_);
    }

    void replay();
};

// utilities
SEXP setClassName(SEXP s, const char* className);
std::string sexpAddress(const SEXP s);

// C++ API
void record_compile(const SEXP cls, const std::string& name,
                    const Context& assumptions);
void record_deopt(const SEXP cls);

size_t saveTo(FILE* file);
size_t replayFrom(FILE* file, SEXP rho);

} // namespace recording

} // namespace rir

// R API
REXPORT SEXP start_recording();
REXPORT SEXP stop_recording();
REXPORT SEXP is_recording();
REXPORT SEXP recordingSave(SEXP filename);
REXPORT SEXP recordingReplay(SEXP filename, SEXP rho);
REXPORT SEXP replay(SEXP recordings, SEXP rho);

#endif
