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

using Clock = std::chrono::system_clock;
using Timestamp = Clock::time_point;

// Records events in a stream or timeline
class EventStream {
  public:
    enum class Mode : unsigned { NotEnabled, Log, FlameGraph };

    enum class CompileEventAssociation {
        NotAssociated,
        IsIntermediateCompileEvent,
        IsStartCompileEvent
    };

    struct Event {
        Timestamp timestamp;

        explicit Event(Timestamp timestamp);
        virtual ~Event();

        virtual void print(std::ostream& out,
                           const std::vector<Event*>::const_iterator& rest,
                           const std::vector<Event*>::const_iterator& end) = 0;
        virtual void printFlame(std::ostream& out) = 0;
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
        void printFlame(std::ostream& out) override;
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
        void printFlame(std::ostream& out) override;
        bool thisPrintsItself() override;
        size_t getDuration() override;
        CompileEventAssociation getAssociationWith(const UUID& uid) override;
    };

    struct ReusedRir2Pir : public Event {
        const UUID versionUid;

        ReusedRir2Pir(const pir::ClosureVersion* version);

        void print(std::ostream& out,
                   const std::vector<Event*>::const_iterator& rest,
                   const std::vector<Event*>::const_iterator& end) override;
        void printFlame(std::ostream& out) override;
        bool thisPrintsItself() override;
        size_t getDuration() override;
        CompileEventAssociation getAssociationWith(const UUID& uid) override;
    };

    struct ReusedPir2Rir : public Event {
        const UUID versionUid;

        ReusedPir2Rir(const pir::ClosureVersion* version);

        void print(std::ostream& out,
                   const std::vector<Event*>::const_iterator& rest,
                   const std::vector<Event*>::const_iterator& end) override;
        void printFlame(std::ostream& out) override;
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
        void printFlame(std::ostream& out) override;
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
        void printFlame(std::ostream& out) override;
        bool thisPrintsItself() override;
        size_t getDuration() override;
        CompileEventAssociation getAssociationWith(const UUID& uid) override;
    };

    struct Inlined : public Event {
        const UUID ownerUid;
        const UUID inlineeUid;

        Inlined(const pir::ClosureVersion* owner,
                const pir::ClosureVersion* inlinee);

        void print(std::ostream& out,
                   const std::vector<Event*>::const_iterator& rest,
                   const std::vector<Event*>::const_iterator& end) override;
        void printFlame(std::ostream& out) override;
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
        void printFlame(std::ostream& out) override;
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
        void printFlame(std::ostream& out) override;
        bool thisPrintsItself() override;
        size_t getDuration() override;
        CompileEventAssociation getAssociationWith(const UUID& uid) override;
    };

    struct FailedPirCompiling : public Event {
        const UUID uid;
        const bool isPirVersion;
        const std::string explanation;

        FailedPirCompiling(const rir::Function* baselineFunction,
                           const std::string& explanation);
        FailedPirCompiling(const pir::ClosureVersion* version,
                           const std::string& explanation);

        void print(std::ostream& out,
                   const std::vector<Event*>::const_iterator& rest,
                   const std::vector<Event*>::const_iterator& end) override;
        void printFlame(std::ostream& out) override;
        bool thisPrintsItself() override;
        size_t getDuration() override;
        CompileEventAssociation getAssociationWith(const UUID& uid) override;
    };

    struct Deoptimized : public Event {
        const UUID deoptimizedFunctionUid;
        const DeoptReason::Reason deoptReason;
        const unsigned relativePc;

        Deoptimized(const Code* deoptimizedFunctionCode,
                    DeoptReason::Reason deoptReason, unsigned relativePc);

        void print(std::ostream& out,
                   const std::vector<Event*>::const_iterator& rest,
                   const std::vector<Event*>::const_iterator& end) override;
        void printFlame(std::ostream& out) override;
        bool thisPrintsItself() override;
        size_t getDuration() override;
        CompileEventAssociation getAssociationWith(const UUID& uid) override;
    };

    static Mode mode;
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
    void printFlame(std::ostream& out);
    void printToFile();
    void flush();
};

} // namespace rir
