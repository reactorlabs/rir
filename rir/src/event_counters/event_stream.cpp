#include "event_stream.h"

#include "compiler/pir/closure_version.h"
#include "compiler/pir/module.h"
#include "runtime/Function.h"

namespace rir {

using Clock = std::chrono::system_clock;
using Timestamp = Clock::time_point;

#define TimestampToString(timestamp)                                           \
    std::to_string(                                                            \
        std::chrono::duration_cast<std::chrono::milliseconds>(                 \
            std::chrono::time_point_cast<std::chrono::milliseconds>(timestamp) \
                .time_since_epoch())                                           \
            .count())

EventStream::Mode EventStream::mode =
    getenv("ENABLE_EVENT_STREAM")
        ? (EventStream::Mode)(unsigned)atoi(getenv("ENABLE_EVENT_STREAM"))
        : EventStream::Mode::NotEnabled;
bool EventStream::isEnabled = EventStream::mode > EventStream::Mode::NotEnabled;

EventStream::Event::Event(Timestamp timestamp) : timestamp(timestamp) {}

EventStream::Event::~Event() {}

EventStream::UserEvent::UserEvent(const std::string& message)
    : Event(Clock::now()), message(message) {}

EventStream::StartedPirCompiling::StartedPirCompiling(
    const pir::ClosureVersion* version, const Assumptions& assumptions)
    : Event(Clock::now()), versionUid(version->uid), assumptions(assumptions) {}

EventStream::ReusedRir2Pir::ReusedRir2Pir(const pir::ClosureVersion* version)
    : Event(Clock::now()), versionUid(version->uid) {}

EventStream::ReusedPir2Rir::ReusedPir2Rir(const pir::ClosureVersion* version)
    : Event(Clock::now()), versionUid(version->uid) {}

EventStream::SucceededRir2Pir::SucceededRir2Pir(
    const pir::ClosureVersion* version, size_t durationMicros)
    : Event(Clock::now()), versionUid(version->uid),
      pirVersionSize(version->size()), durationMicros(durationMicros) {}

EventStream::OptimizedPir::OptimizedPir(const pir::Module* module,
                                        size_t durationMicros)
    : Event(Clock::now()), versionUids(module->getClosureVersionUids()),
      durationMicros(durationMicros) {}

EventStream::Inlined::Inlined(const pir::ClosureVersion* owner,
                              const pir::ClosureVersion* inlinee)
    : Event(Clock::now()), ownerUid(owner->uid), inlineeUid(inlinee->uid) {}

EventStream::LoweredPir2Rir::LoweredPir2Rir(const pir::ClosureVersion* version,
                                            size_t durationMicros)
    : Event(Clock::now()), versionUid(version->uid),
      pirVersionSize(version->size()), durationMicros(durationMicros) {}

EventStream::LoweredLLVM::LoweredLLVM(const pir::ClosureVersion* version,
                                      size_t durationMicros)
    : Event(Clock::now()), versionUid(version->uid),
      durationMicros(durationMicros) {}

EventStream::FailedPirCompiling::FailedPirCompiling(
    const rir::Function* baselineFunction, const std::string& explanation)
    : Event(Clock::now()), uid(baselineFunction->body()->uid),
      isPirVersion(false), explanation(explanation) {}

EventStream::FailedPirCompiling::FailedPirCompiling(
    const pir::ClosureVersion* version, const std::string& explanation)
    : Event(Clock::now()), uid(version->uid), isPirVersion(true),
      explanation(explanation) {}

EventStream::Deoptimized::Deoptimized(const Code* deoptimizedFunctionCode,
                                      DeoptReason::Reason deoptReason,
                                      unsigned relativePc)
    : Event(Clock::now()), deoptimizedFunctionUid(deoptimizedFunctionCode->uid),
      deoptReason(deoptReason), relativePc(relativePc) {}

void EventStream::UserEvent::print(
    std::ostream& out, const std::vector<Event*>::const_iterator& rest,
    const std::vector<Event*>::const_iterator& end) {
    out << message << std::endl;
}

void EventStream::UserEvent::printFlame(std::ostream& out) {
    // clang-format off
    out << "{\"name\": " << std::quoted(message)
        << ", \"cat\": \"user\""
        << ", \"ph\": \"i\""
        << ", \"ts\": " << TimestampToString(timestamp)
        << ", \"pid\": 1"
        << "}";
    // clang-format on
}

bool EventStream::UserEvent::thisPrintsItself() { return true; }

size_t EventStream::UserEvent::getDuration() { return 0; }

EventStream::CompileEventAssociation
EventStream::UserEvent::getAssociationWith(const UUID& uid) {
    return EventStream::CompileEventAssociation::NotAssociated;
}

void EventStream::StartedPirCompiling::print(
    std::ostream& out, const std::vector<Event*>::const_iterator& rest,
    const std::vector<Event*>::const_iterator& end) {
    // Print this event itself
    out << EventStream::instance().getNameOf(versionUid) << " compile ("
        << assumptions << ") => ";

    // Keep printing associated events until 1) we reach the end of the stream
    // or 2) we reach a new started-compiling event for this version.
    // Also keep track of the total duration
    size_t totalDuration = 0;
    for (std::vector<Event*>::const_iterator it = rest + 1; it != end; it++) {
        bool isDone = false;

        Event* event = *it;
        switch (event->getAssociationWith(versionUid)) {
        case EventStream::CompileEventAssociation::NotAssociated:
            break;
        case EventStream::CompileEventAssociation::IsIntermediateCompileEvent:
            totalDuration += event->getDuration();
            event->print(out, it, end);
            break;
        case EventStream::CompileEventAssociation::IsStartCompileEvent:
            // Don't print this because it's a new compile event chain for this
            // closure
            isDone = true;
            break;
        }

        if (isDone) {
            break;
        }
    }

    out << "done [" << (totalDuration / 1000) << "ms] " << std::endl;
}

static size_t nextEventNumber = 0;

static void printEvent(std::ostream& out, const std::string& eventType,
                       const std::string& type, const std::string& extraLabel,
                       const std::string& extra, const UUID& uid,
                       const Timestamp& timestamp, size_t eventNumber) {
    // clang-format off
    out << "{ \"name\": " << std::quoted(EventStream::instance().getNameOf(uid))
        << ", \"cat\": \"" << type << "\""
        << ", \"args\": {\"" << extraLabel << "\": " << std::quoted(extra) << "}"
        << ", \"ph\": \"" << eventType << "\""
        << ", \"ts\": " << TimestampToString(timestamp) 
        << ", \"pid\": 1"
        << ", \"tid\": " << eventNumber << ""
        << "}";
    // clang-format on
};

static void printInstantEvent(std::ostream& out, const std::string& type,
                              const std::string& extraLabel,
                              const std::string& extra, const UUID& uid,
                              const Timestamp& timestamp) {
    printEvent(out, "X", type, extraLabel, extra, uid, timestamp,
               nextEventNumber++);
}

static void printEventWithDuration(std::ostream& out, const std::string& type,
                                   const std::string& extraLabel,
                                   const std::string& extra, const UUID& uid,
                                   const Timestamp& timestamp,
                                   const size_t durationMicros) {
    size_t eventNumber = nextEventNumber++;
    auto printSubEvent = [&](const std::string& eventType,
                             const Timestamp& timestamp) {
        printEvent(out, eventType, type, extraLabel, extra, uid, timestamp,
                   eventNumber);
    };

    if (durationMicros == 0) {
        printSubEvent("X", timestamp);
    } else {
        const std::chrono::microseconds duration(durationMicros);

        printSubEvent("B", timestamp);
        out << ",\n\t\t";
        printSubEvent("E", timestamp + duration);
    }
}

void EventStream::StartedPirCompiling::printFlame(std::ostream& out) {
    printEventWithDuration(out, "compile", "stage", "started", versionUid,
                           timestamp, getDuration());
}

bool EventStream::StartedPirCompiling::thisPrintsItself() { return true; }

size_t EventStream::StartedPirCompiling::getDuration() { return 0; }

EventStream::CompileEventAssociation
EventStream::StartedPirCompiling::getAssociationWith(const UUID& uid) {
    return (versionUid == uid)
               ? EventStream::CompileEventAssociation::IsStartCompileEvent
               : EventStream::CompileEventAssociation::NotAssociated;
}

void EventStream::ReusedRir2Pir::print(
    std::ostream& out, const std::vector<Event*>::const_iterator& rest,
    const std::vector<Event*>::const_iterator& end) {
    out << EventStream::instance().getNameOf(versionUid) << " reused"
        << std::endl;
}

void EventStream::ReusedRir2Pir::printFlame(std::ostream& out) {
    printInstantEvent(out, "reuse", "stage", "rir", versionUid, timestamp);
}

bool EventStream::ReusedRir2Pir::thisPrintsItself() { return true; }

size_t EventStream::ReusedRir2Pir::getDuration() { return 0; }

EventStream::CompileEventAssociation
EventStream::ReusedRir2Pir::getAssociationWith(const UUID& uid) {
    return EventStream::CompileEventAssociation::NotAssociated;
}

void EventStream::ReusedPir2Rir::print(
    std::ostream& out, const std::vector<Event*>::const_iterator& rest,
    const std::vector<Event*>::const_iterator& end) {
    out << EventStream::instance().getNameOf(versionUid) << " reused pir; ";
}

void EventStream::ReusedPir2Rir::printFlame(std::ostream& out) {
    printInstantEvent(out, "reuse", "stage", "pir", versionUid, timestamp);
}

bool EventStream::ReusedPir2Rir::thisPrintsItself() { return false; }

size_t EventStream::ReusedPir2Rir::getDuration() { return 0; }

EventStream::CompileEventAssociation
EventStream::ReusedPir2Rir::getAssociationWith(const UUID& uid) {
    return (versionUid == uid)
               ? EventStream::CompileEventAssociation::
                     IsIntermediateCompileEvent
               : EventStream::CompileEventAssociation::NotAssociated;
}

void EventStream::SucceededRir2Pir::print(
    std::ostream& out, const std::vector<Event*>::const_iterator& rest,
    const std::vector<Event*>::const_iterator& end) {
    out << "rir2pir [" << (durationMicros / 1000) << "ms] [" << pirVersionSize
        << "instr]; ";
}

void EventStream::SucceededRir2Pir::printFlame(std::ostream& out) {
    printEventWithDuration(out, "compile", "stage", "rir2pir", versionUid,
                           timestamp, getDuration());
}

bool EventStream::SucceededRir2Pir::thisPrintsItself() { return false; }

size_t EventStream::SucceededRir2Pir::getDuration() { return durationMicros; }

EventStream::CompileEventAssociation
EventStream::SucceededRir2Pir::getAssociationWith(const UUID& uid) {
    return (versionUid == uid)
               ? EventStream::CompileEventAssociation::
                     IsIntermediateCompileEvent
               : EventStream::CompileEventAssociation::NotAssociated;
}

void EventStream::OptimizedPir::print(
    std::ostream& out, const std::vector<Event*>::const_iterator& rest,
    const std::vector<Event*>::const_iterator& end) {
    out << "optimized [" << (durationMicros / 1000) << "ms]; ";
}

void EventStream::OptimizedPir::printFlame(std::ostream& out) {
    bool isFirst = true;
    for (UUID versionUid : versionUids) {
        if (!isFirst) {
            out << ",\n\t\t";
        }
        isFirst = false;
        printEventWithDuration(out, "compile", "stage", "optimize", versionUid,
                               timestamp, getDuration());
    }
}

bool EventStream::OptimizedPir::thisPrintsItself() { return false; }

size_t EventStream::OptimizedPir::getDuration() { return durationMicros; }

EventStream::CompileEventAssociation
EventStream::OptimizedPir::getAssociationWith(const UUID& uid) {
    return versionUids.count(uid)
               ? EventStream::CompileEventAssociation::
                     IsIntermediateCompileEvent
               : EventStream::CompileEventAssociation::NotAssociated;
}

void EventStream::Inlined::print(
    std::ostream& out, const std::vector<Event*>::const_iterator& rest,
    const std::vector<Event*>::const_iterator& end) {
    out << "inlined into " << EventStream::instance().getNameOf(ownerUid)
        << "; ";
}

void EventStream::Inlined::printFlame(std::ostream& out) {
    std::stringstream subtypeBuffer;
    subtypeBuffer << "inline into "
                  << std::quoted(EventStream::instance().getNameOf(ownerUid));
    std::string subtype = subtypeBuffer.str();
    printEventWithDuration(out, "compile", "stage", subtype, inlineeUid,
                           timestamp, getDuration());
}

bool EventStream::Inlined::thisPrintsItself() { return false; }

size_t EventStream::Inlined::getDuration() { return 0; }

EventStream::CompileEventAssociation
EventStream::Inlined::getAssociationWith(const UUID& uid) {
    return (inlineeUid == uid)
               ? EventStream::CompileEventAssociation::
                     IsIntermediateCompileEvent
               : EventStream::CompileEventAssociation::NotAssociated;
}

void EventStream::LoweredPir2Rir::print(
    std::ostream& out, const std::vector<Event*>::const_iterator& rest,
    const std::vector<Event*>::const_iterator& end) {
    out << "pir2rir [" << (durationMicros / 1000) << "ms] [" << pirVersionSize
        << "instr]; ";
}

void EventStream::LoweredPir2Rir::printFlame(std::ostream& out) {
    printEventWithDuration(out, "compile", "stage", "pir2rir", versionUid,
                           timestamp, getDuration());
}

bool EventStream::LoweredPir2Rir::thisPrintsItself() { return false; }

size_t EventStream::LoweredPir2Rir::getDuration() { return durationMicros; }

EventStream::CompileEventAssociation
EventStream::LoweredPir2Rir::getAssociationWith(const UUID& uid) {
    return (versionUid == uid)
               ? EventStream::CompileEventAssociation::
                     IsIntermediateCompileEvent
               : EventStream::CompileEventAssociation::NotAssociated;
}

void EventStream::LoweredLLVM::print(
    std::ostream& out, const std::vector<Event*>::const_iterator& rest,
    const std::vector<Event*>::const_iterator& end) {
    out << "llvm [" << (durationMicros / 1000) << "ms]; ";
}

void EventStream::LoweredLLVM::printFlame(std::ostream& out) {
    printEventWithDuration(out, "compile", "stage", "lower-llvm", versionUid,
                           timestamp, getDuration());
}

bool EventStream::LoweredLLVM::thisPrintsItself() { return false; }

size_t EventStream::LoweredLLVM::getDuration() { return durationMicros; }

EventStream::CompileEventAssociation
EventStream::LoweredLLVM::getAssociationWith(const UUID& uid) {
    return (versionUid == uid)
               ? EventStream::CompileEventAssociation::
                     IsIntermediateCompileEvent
               : EventStream::CompileEventAssociation::NotAssociated;
}

void EventStream::FailedPirCompiling::print(
    std::ostream& out, const std::vector<Event*>::const_iterator& rest,
    const std::vector<Event*>::const_iterator& end) {
    if (!isPirVersion) {
        out << EventStream::instance().getNameOf(uid) << " compile ";
    }
    out << "failed (" << explanation << ")" << std::endl;
}

void EventStream::FailedPirCompiling::printFlame(std::ostream& out) {
    printInstantEvent(out, "failed compile", "reason", explanation, uid,
                      timestamp);
}

bool EventStream::FailedPirCompiling::thisPrintsItself() {
    return !isPirVersion;
}

size_t EventStream::FailedPirCompiling::getDuration() { return 0; }

EventStream::CompileEventAssociation
EventStream::FailedPirCompiling::getAssociationWith(const UUID& uid) {
    return (this->uid == uid)
               ? EventStream::CompileEventAssociation::
                     IsIntermediateCompileEvent
               : EventStream::CompileEventAssociation::NotAssociated;
}

void EventStream::Deoptimized::print(
    std::ostream& out, const std::vector<Event*>::const_iterator& rest,
    const std::vector<Event*>::const_iterator& end) {
    out << EventStream::instance().getNameOf(deoptimizedFunctionUid)
        << " deopt (" << getDeoptReasonExplanation(deoptReason) << ")";
    if (relativePc != (unsigned)-1) {
        out << " at " << relativePc;
    }
    out << std::endl;
}

void EventStream::Deoptimized::printFlame(std::ostream& out) {
    printInstantEvent(out, "deopt", "reason",
                      getDeoptReasonExplanation(deoptReason),
                      deoptimizedFunctionUid, timestamp);
}

bool EventStream::Deoptimized::thisPrintsItself() { return true; }

size_t EventStream::Deoptimized::getDuration() { return 0; }

EventStream::CompileEventAssociation
EventStream::Deoptimized::getAssociationWith(const UUID& uid) {
    return EventStream::CompileEventAssociation::NotAssociated;
}

std::string EventStream::getNameOf(const UUID& functionUid) {
    if (!versionNames.count(functionUid)) {
        return "<unknown>";
    } else {
        return versionNames.at(functionUid);
    }
}

void EventStream::setNameOf(const UUID& uid, const std::string& name) {
    if (!versionNames.count(uid)) {
        std::string nonCollidingName =
            numVersionsWithName.count(name)
                ? name + "~" + std::to_string(numVersionsWithName.at(name))
                : name;
        numVersionsWithName[name] = numVersionsWithName[name] + 1;

        versionNames[uid] = nonCollidingName;
    }
}

void EventStream::setNameOf(const Function* function, const std::string& name) {
    setNameOf(function->body()->uid, name);
}

void EventStream::setNameOf(const pir::ClosureVersion* version) {
    setNameOf(version->uid, version->name());
}

void EventStream::recordEvent(Event* event) { events.push_back(event); }
bool EventStream::hasEvents() { return !events.empty(); }
void EventStream::reset() {
    for (Event* event : events) {
        delete event;
    }
    events.clear();
}

void EventStream::print(std::ostream& out) {
    std::vector<Event*>::const_iterator end = events.end();
    for (std::vector<Event*>::const_iterator it = events.begin(); it != end;
         it++) {
        Event* event = *it;
        if (event->thisPrintsItself()) {
            event->print(out, it, end);
        }
    }
}

void EventStream::printFlame(std::ostream& out) {
    out << "{\n\t\"traceEvents\": [\n";
    std::vector<Event*>::const_iterator end = events.end();
    for (std::vector<Event*>::const_iterator it = events.begin(); it != end;
         it++) {
        out << "\t\t";
        Event* event = *it;
        event->printFlame(out);
        if (it + 1 != end) {
            out << ",\n";
        }
    }
    out << "\n\t],\n\t\"displayTimeUnit\": \"ms\"\n}";
}

void EventStream::printToFile() {
    if (!hasEvents() || EventStream::mode == EventStream::Mode::NotEnabled) {
        return;
    }

    std::ofstream file;
    switch (EventStream::mode) {
    case EventStream::Mode::NotEnabled:
        assert(false);
    case EventStream::Mode::Log:
        file.open("event_stream.log");
        print(file);
        break;
    case EventStream::Mode::FlameGraph:
        file.open("event_stream.json");
        printFlame(file);
        break;
    }
    file.close();
}
void EventStream::flush() {
    printToFile();
    reset();
}

} // namespace rir