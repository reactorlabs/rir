#ifndef RECORDING_H
#define RECORDING_H

#include "compiler/pir/closure_version.h"
#include "compiler/pir/pir.h"
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

enum class SpeculativeContextType { Callees, Test, Values };
struct SpeculativeContext {
    SpeculativeContextType type;
    union Value {
        ObservedCallees callees;
        ObservedTest test;
        ObservedValues values;
    } value;

    friend std::ostream& operator<<(std::ostream& out,
                                    const SpeculativeContext& e);

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

  protected:
    virtual void print(std::ostream&) const = 0;
};

class CompilationEvent : public Event {
  public:
    void add_speculative_context(std::vector<SpeculativeContext>&& ctx) {
        speculative_contexts.emplace_back(ctx);
    }

  protected:
    void print(std::ostream& out) const;

  private:
    unsigned long dispatch_context;

    // Recordings of the speculative context, i.e. the type feedback from RIR
    // byte code It is indexed by closures and by each of the recording
    // instruction in the order it is visited in the code. The first element is
    // the function itself.
    std::vector<std::vector<SpeculativeContext>> speculative_contexts;
};

class DeoptEvent : public Event {
  protected:
    void print(std::ostream& out) const {}
};

struct FunRecorder {
    std::string name;
    std::string r_code;
    std::vector<std::unique_ptr<Event>> events;

    friend std::ostream& operator<<(std::ostream& out, const FunRecorder& fr);
};

void record_compile(const SEXP cls, const std::string& name);
void record_deopt(const SEXP cls);

} // namespace recording

} // namespace rir

#endif
