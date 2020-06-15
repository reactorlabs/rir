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

        virtual void print(std::ostream& out,
                           const std::vector<Event*>::const_iterator& rest,
                           const std::vector<Event*>::const_iterator& end) = 0;
        virtual bool thisPrintsItself() = 0;
        virtual bool isEndOfCompiling(const UUID& rirFunctionId) = 0;
    };

  private:
    std::unordered_map<UUID, std::string> versionNames;
    std::unordered_map<std::string, unsigned> numVersionsWithName;
    std::vector<Event*> events;
    EventStream() {}

  public:
    struct UserEvent : public Event {
        const std::string message;

        explicit UserEvent(const std::string& message);

        void print(std::ostream& out,
                   const std::vector<Event*>::const_iterator& rest,
                   const std::vector<Event*>::const_iterator& end) override;
        bool thisPrintsItself() override;
        bool isEndOfCompiling(const UUID& rirFunctionId) override;
    };

    struct StartedPirCompiling : public Event {
        const UUID rirFunctionUid;
        const Assumptions assumptions;

        StartedPirCompiling(const Function* rirFunction,
                            const Assumptions& assumptions);

        void print(std::ostream& out,
                   const std::vector<Event*>::const_iterator& rest,
                   const std::vector<Event*>::const_iterator& end) override;
        bool thisPrintsItself() override;
        bool isEndOfCompiling(const UUID& rirFunctionId) override;
    };

    struct ReusedPirCompiled : public Event {
        const UUID rirFunctionUid;
        const size_t durationMicros;

        ReusedPirCompiled(const Function* rirFunction, size_t durationMicros);

        void print(std::ostream& out,
                   const std::vector<Event*>::const_iterator& rest,
                   const std::vector<Event*>::const_iterator& end) override;
        bool thisPrintsItself() override;
        bool isEndOfCompiling(const UUID& rirFunctionId) override;
    };

    struct SucceededPirCompiling : public Event {
        const UUID rirFunctionUid;
        const size_t durationMicros;

        SucceededPirCompiling(const Function* rirFunction,
                              size_t durationMicros);

        void print(std::ostream& out,
                   const std::vector<Event*>::const_iterator& rest,
                   const std::vector<Event*>::const_iterator& end) override;
        bool thisPrintsItself() override;
        bool isEndOfCompiling(const UUID& rirFunctionId) override;
    };

    struct FailedPirCompiling : public Event {
        const UUID rirFunctionUid;
        const size_t durationMicros;
        const std::string explanation;

        FailedPirCompiling(const Function* rirFunction, size_t durationMicros,
                           const std::string& explanation);

        void print(std::ostream& out,
                   const std::vector<Event*>::const_iterator& rest,
                   const std::vector<Event*>::const_iterator& end) override;
        bool thisPrintsItself() override;
        bool isEndOfCompiling(const UUID& rirFunctionId) override;
    };

    struct Deoptimized : public Event {
        const UUID baselineFunctionUid;
        const DeoptReason::Reason deoptReason;

        Deoptimized(const Function* baselineFunction,
                    DeoptReason::Reason deoptReason);

        void print(std::ostream& out,
                   const std::vector<Event*>::const_iterator& rest,
                   const std::vector<Event*>::const_iterator& end) override;
        bool thisPrintsItself() override;
        bool isEndOfCompiling(const UUID& rirFunctionId) override;
    };

    static bool isEnabled;
    static EventStream& instance() {
        static EventStream c;
        return c;
    }

    std::string getNameOf(const UUID& functionUid);
    void setNameOf(const Function* function, std::string name);

    void recordEvent(Event* event);
    bool hasEvents();
    void reset();
    void print(std::ostream& out);
    void printToFile();
    void flush();
};

} // namespace rir
