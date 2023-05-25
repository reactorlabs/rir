#ifndef RECORDING_H
#define RECORDING_H

#include "api.h"
#include "compiler/pir/closure_version.h"
#include "compiler/pir/pir.h"
#include "runtime/Context.h"
#include "runtime/TypeFeedback.h"
#include <R/r.h>
#include <iterator>
#include <memory>
#include <sstream>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>
namespace rir {

namespace recording {

/**
 * A SEXP reference that's protected from garbage collection and will be
 * unprotected on destruction
 */
struct PreservedSEXP {
  public:
    inline PreservedSEXP() : sexp(nullptr) {}
    inline explicit PreservedSEXP(SEXP sexp) : sexp(sexp) { preserve(); }
    inline PreservedSEXP(const PreservedSEXP& other) : sexp(other.sexp) {
        preserve();
    }
    PreservedSEXP& operator=(const PreservedSEXP& other);
    PreservedSEXP& operator=(const SEXP& sexp);
    inline ~PreservedSEXP() { release(); }
    inline SEXP get() const { return sexp; }
    explicit operator SEXP() const { return sexp; }

  private:
    SEXP sexp;
    void preserve() const;
    void release();
};

enum class SpeculativeContextType { Callees, Test, Values };
struct SpeculativeContext {
    SpeculativeContextType type;
    union Value {
        ObservedCallees callees;
        ObservedTest test;
        ObservedValues values;
    } value;

    SpeculativeContext(ObservedCallees callees)
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
    virtual void replay(SEXP closure, std::string& closure_name) const = 0;
};

class CompilationEvent : public Event {
  public:
    CompilationEvent(unsigned long dispatch_context,
                     std::vector<SpeculativeContext>&& speculative_contexts)
        : dispatch_context(dispatch_context),
          speculative_contexts(speculative_contexts) {}
    SEXP to_sexp() const override;
    void init_from_sexp(SEXP sexp) override;
    void replay(SEXP closure, std::string& closure_name) const override;

  private:
    unsigned long dispatch_context;

    std::vector<SpeculativeContext> speculative_contexts;
};

class DeoptEvent : public Event {
  public:
    SEXP to_sexp() const override;
    void init_from_sexp(SEXP file) override;
    void replay(SEXP closure, std::string& closure_name) const override;
};

struct FunRecorder {
    std::string name;
    /* the CLOSXP serialized into RAWSXP using the R_SerializeValue*/
    PreservedSEXP closure;
    std::vector<std::unique_ptr<Event>> events;
};

void record_compile(const SEXP cls, const std::string& name,
                    const Context& assumptions);
void record_deopt(const SEXP cls);

size_t saveTo(FILE* file);
size_t replayFrom(FILE* file, SEXP rho);
REXPORT SEXP replay(SEXP recording, SEXP rho);

} // namespace recording

} // namespace rir

// R API
REXPORT SEXP start_recording();
REXPORT SEXP stop_recording();
REXPORT SEXP is_recording();

#endif
