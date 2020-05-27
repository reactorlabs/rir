#pragma once

#include <algorithm>
#include <cassert>
#include <fstream>
#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>

#include "runtime/Assumptions.h"
#include "runtime/TypeFeedback.h"
#include "utils/UUID.h"

namespace rir {

struct Function;

// Records events in a stream or timeline
class EventStream {
  public:
    struct Event {
        virtual ~Event();

        virtual void print(std::ostream& out) = 0;
    };

  private:
    std::unordered_map<UUID, std::string> versionNames;
    std::vector<Event*> events;
    EventStream() {}

  public:
    struct UserEvent : public Event {
        const std::string message;

        explicit UserEvent(const std::string& message) : message(message) {}

        void print(std::ostream& out) override;
    };

    struct StartedPirCompiling : public Event {
        const Function* rirFunction;
        const Assumptions assumptions;

        StartedPirCompiling(const Function* rirFunction,
                            const Assumptions& assumptions)
            : rirFunction(rirFunction), assumptions(assumptions) {}

        void print(std::ostream& out) override;
    };

    struct ReusedPirCompiled : public Event {
        const Function* rirFunction;
        const size_t durationMicros;

        ReusedPirCompiled(const Function* rirFunction, size_t durationMicros)
            : rirFunction(rirFunction), durationMicros(durationMicros) {}

        void print(std::ostream& out) override;
    };

    struct SucceededPirCompiling : public Event {
        const Function* rirFunction;
        const size_t durationMicros;

        SucceededPirCompiling(const Function* rirFunction,
                              size_t durationMicros)
            : rirFunction(rirFunction), durationMicros(durationMicros) {}

        void print(std::ostream& out) override;
    };

    struct FailedPirCompiling : public Event {
        const Function* rirFunction;
        const size_t durationMicros;
        const std::string explanation;

        FailedPirCompiling(const Function* rirFunction, size_t durationMicros,
                           const std::string& explanation)
            : rirFunction(rirFunction), durationMicros(durationMicros),
              explanation(explanation) {}

        void print(std::ostream& out) override;
    };

    struct Deoptimized : public Event {
        const Function* baselineFunction;
        const DeoptReason::Reason deoptReason;

        Deoptimized(const Function* baselineFunction,
                    DeoptReason::Reason deoptReason)
            : baselineFunction(baselineFunction), deoptReason(deoptReason) {}

        void print(std::ostream& out) override;
    };

    static bool isEnabled;
    static EventStream& instance() {
        static EventStream c;
        return c;
    }

    std::string getNameOf(const Function* function);
    void setNameOf(const Function* function, std::string name);

    void recordEvent(Event* event);
    bool hasEvents();
    void reset();
    void print(std::ostream& out);
    void printToFile();
    void flush();
};

} // namespace rir
