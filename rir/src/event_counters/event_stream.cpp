#include "event_stream.h"

#include "compiler/pir/closure_version.h"
#include "compiler/pir/module.h"
#include "runtime/Function.h"

namespace rir {

bool EventStream::isEnabled =
    getenv("ENABLE_EVENT_STREAM") && *getenv("ENABLE_EVENT_STREAM") == '1';

EventStream::Event::~Event() {}

EventStream::UserEvent::UserEvent(const std::string& message)
    : message(message) {}

EventStream::StartedPirCompiling::StartedPirCompiling(
    const pir::ClosureVersion* version, const Assumptions& assumptions)
    : versionUid(version->uid), assumptions(assumptions) {}

EventStream::ReusedPirCompiled::ReusedPirCompiled(
    const pir::ClosureVersion* version, size_t durationMicros)
    : versionUid(version->uid), durationMicros(durationMicros) {}

EventStream::SucceededRir2Pir::SucceededRir2Pir(
    const pir::ClosureVersion* version, size_t durationMicros)
    : versionUid(version->uid), pirVersionSize(version->size()),
      durationMicros(durationMicros) {}

EventStream::OptimizedPir::OptimizedPir(const pir::Module* module,
                                        size_t durationMicros)
    : versionUids(module->getClosureVersionUids()),
      durationMicros(durationMicros) {}

EventStream::LoweredPir2Rir::LoweredPir2Rir(const pir::ClosureVersion* version,
                                            size_t durationMicros)
    : versionUid(version->uid), pirVersionSize(version->size()),
      durationMicros(durationMicros) {}

EventStream::LoweredLLVM::LoweredLLVM(const pir::ClosureVersion* version,
                                      size_t durationMicros)
    : versionUid(version->uid), durationMicros(durationMicros) {}

EventStream::FailedPirCompiling::FailedPirCompiling(
    const rir::Function* baselineFunction, size_t durationMicros,
    const std::string& explanation)
    : uid(baselineFunction->body()->uid), isPirVersion(false),
      durationMicros(durationMicros), explanation(explanation) {}

EventStream::FailedPirCompiling::FailedPirCompiling(
    const pir::ClosureVersion* version, size_t durationMicros,
    const std::string& explanation)
    : uid(version->uid), isPirVersion(true), durationMicros(durationMicros),
      explanation(explanation) {}

EventStream::Deoptimized::Deoptimized(const Code* deoptimizedFunctionCode,
                                      DeoptReason::Reason deoptReason)
    : deoptimizedFunctionUid(deoptimizedFunctionCode->uid),
      deoptReason(deoptReason) {}

void EventStream::UserEvent::print(
    std::ostream& out, const std::vector<Event*>::const_iterator& rest,
    const std::vector<Event*>::const_iterator& end) {
    out << message << std::endl;
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

bool EventStream::StartedPirCompiling::thisPrintsItself() { return true; }

size_t EventStream::StartedPirCompiling::getDuration() { return 0; }

EventStream::CompileEventAssociation
EventStream::StartedPirCompiling::getAssociationWith(const UUID& uid) {
    return (versionUid == uid)
               ? EventStream::CompileEventAssociation::IsStartCompileEvent
               : EventStream::CompileEventAssociation::NotAssociated;
}

void EventStream::ReusedPirCompiled::print(
    std::ostream& out, const std::vector<Event*>::const_iterator& rest,
    const std::vector<Event*>::const_iterator& end) {
    out << EventStream::instance().getNameOf(versionUid) << " reused ["
        << (durationMicros / 1000) << "ms]" << std::endl;
}

bool EventStream::ReusedPirCompiled::thisPrintsItself() { return true; }

size_t EventStream::ReusedPirCompiled::getDuration() { return 0; }

EventStream::CompileEventAssociation
EventStream::ReusedPirCompiled::getAssociationWith(const UUID& uid) {
    return EventStream::CompileEventAssociation::NotAssociated;
}

void EventStream::SucceededRir2Pir::print(
    std::ostream& out, const std::vector<Event*>::const_iterator& rest,
    const std::vector<Event*>::const_iterator& end) {
    out << "rir2pir [" << (durationMicros / 1000) << "ms] [" << pirVersionSize
        << "instr]; ";
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

bool EventStream::OptimizedPir::thisPrintsItself() { return false; }

size_t EventStream::OptimizedPir::getDuration() { return durationMicros; }

EventStream::CompileEventAssociation
EventStream::OptimizedPir::getAssociationWith(const UUID& uid) {
    return versionUids.count(uid)
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
    out << "failed (" << explanation << ") [" << (durationMicros / 1000)
        << "ms]" << std::endl;
}

bool EventStream::FailedPirCompiling::thisPrintsItself() {
    return !isPirVersion;
}

size_t EventStream::FailedPirCompiling::getDuration() { return durationMicros; }

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
        << " deopt (" << getDeoptReasonExplanation(deoptReason) << ")"
        << std::endl;
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

void EventStream::printToFile() {
    if (!hasEvents()) {
        return;
    }

    std::ofstream file;
    file.open("event_stream.log");
    print(file);
    file.close();
}
void EventStream::flush() {
    printToFile();
    reset();
}

} // namespace rir