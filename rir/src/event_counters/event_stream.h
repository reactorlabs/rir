#pragma once

#include <algorithm>
#include <cassert>
#include <fstream>
#include <iostream>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "runtime/Assumptions.h"
#include "runtime/TypeFeedback.h"
#include "utils/UUID.h"

namespace rir {

namespace pir {
class Module;
class ClosureVersion;
}

struct Function;

// Records events in a stream or timeline
class EventStream {
  public:
    enum class CompileEventAssociation {
        NotAssociated,
        IsIntermediateCompileEvent,
        IsStartCompileEvent
    };

    struct Event {
        virtual ~Event();

        virtual void print(std::ostream& out,
                           const std::vector<Event*>::const_iterator& rest,
                           const std::vector<Event*>::const_iterator& end) = 0;
        virtual bool thisPrintsItself() = 0;
        virtual size_t getDuration() = 0;
        virtual CompileEventAssociation getAssociationWith(const UUID& uid) = 0;
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
        size_t getDuration() override;
        CompileEventAssociation getAssociationWith(const UUID& uid) override;
    };

    struct StartedPirCompiling : public Event {
        const UUID versionUid;
        const Assumptions assumptions;

        StartedPirCompiling(const pir::ClosureVersion* version,
                            const Assumptions& assumptions);

        void print(std::ostream& out,
                   const std::vector<Event*>::const_iterator& rest,
                   const std::vector<Event*>::const_iterator& end) override;
        bool thisPrintsItself() override;
        size_t getDuration() override;
        CompileEventAssociation getAssociationWith(const UUID& uid) override;
    };

    struct ReusedPirCompiled : public Event {
        const UUID versionUid;
        const size_t durationMicros;

        ReusedPirCompiled(const pir::ClosureVersion* version,
                          size_t durationMicros);

        void print(std::ostream& out,
                   const std::vector<Event*>::const_iterator& rest,
                   const std::vector<Event*>::const_iterator& end) override;
        bool thisPrintsItself() override;
        size_t getDuration() override;
        CompileEventAssociation getAssociationWith(const UUID& uid) override;
    };

    struct SucceededRir2Pir : public Event {
        const UUID versionUid;
        const size_t pirVersionSize;
        const size_t durationMicros;

        SucceededRir2Pir(const pir::ClosureVersion* version,
                         size_t durationMicros);

        void print(std::ostream& out,
                   const std::vector<Event*>::const_iterator& rest,
                   const std::vector<Event*>::const_iterator& end) override;
        bool thisPrintsItself() override;
        size_t getDuration() override;
        CompileEventAssociation getAssociationWith(const UUID& uid) override;
    };

    struct OptimizedPir : public Event {
        const std::unordered_set<UUID> versionUids;
        const size_t durationMicros;

        OptimizedPir(const pir::Module* module, size_t durationMicros);

        void print(std::ostream& out,
                   const std::vector<Event*>::const_iterator& rest,
                   const std::vector<Event*>::const_iterator& end) override;
        bool thisPrintsItself() override;
        size_t getDuration() override;
        CompileEventAssociation getAssociationWith(const UUID& uid) override;
    };

    struct LoweredPir2Rir : public Event {
        const UUID versionUid;
        const size_t pirVersionSize;
        const size_t durationMicros;

        LoweredPir2Rir(const pir::ClosureVersion* version,
                       size_t durationMicros);

        void print(std::ostream& out,
                   const std::vector<Event*>::const_iterator& rest,
                   const std::vector<Event*>::const_iterator& end) override;
        bool thisPrintsItself() override;
        size_t getDuration() override;
        CompileEventAssociation getAssociationWith(const UUID& uid) override;
    };

    struct LoweredLLVM : public Event {
        const UUID versionUid;
        const size_t durationMicros;

        LoweredLLVM(const pir::ClosureVersion* version, size_t durationMicros);

        void print(std::ostream& out,
                   const std::vector<Event*>::const_iterator& rest,
                   const std::vector<Event*>::const_iterator& end) override;
        bool thisPrintsItself() override;
        size_t getDuration() override;
        CompileEventAssociation getAssociationWith(const UUID& uid) override;
    };

    struct FailedPirCompiling : public Event {
        const UUID uid;
        const bool isPirVersion;
        const size_t durationMicros;
        const std::string explanation;

        FailedPirCompiling(const rir::Function* baselineFunction,
                           size_t durationMicros,
                           const std::string& explanation);
        FailedPirCompiling(const pir::ClosureVersion* version,
                           size_t durationMicros,
                           const std::string& explanation);

        void print(std::ostream& out,
                   const std::vector<Event*>::const_iterator& rest,
                   const std::vector<Event*>::const_iterator& end) override;
        bool thisPrintsItself() override;
        size_t getDuration() override;
        CompileEventAssociation getAssociationWith(const UUID& uid) override;
    };

    struct Deoptimized : public Event {
        const UUID deoptimizedFunctionUid;
        const DeoptReason::Reason deoptReason;

        Deoptimized(const Code* deoptimizedFunctionCode,
                    DeoptReason::Reason deoptReason);

        void print(std::ostream& out,
                   const std::vector<Event*>::const_iterator& rest,
                   const std::vector<Event*>::const_iterator& end) override;
        bool thisPrintsItself() override;
        size_t getDuration() override;
        CompileEventAssociation getAssociationWith(const UUID& uid) override;
    };

    static bool isEnabled;
    static EventStream& instance() {
        static EventStream c;
        return c;
    }

    std::string getNameOf(const UUID& functionUid);
    void setNameOf(const UUID& uid, const std::string& name);
    void setNameOf(const Function* function, const std::string& name);
    void setNameOf(const pir::ClosureVersion* version);

    void recordEvent(Event* event);
    bool hasEvents();
    void reset();
    void print(std::ostream& out);
    void printToFile();
    void flush();
};

} // namespace rir
