#include "event_stream.h"

#include "runtime/Function.h"

namespace rir {

bool EventStream::isEnabled =
    getenv("ENABLE_EVENT_STREAM") && *getenv("ENABLE_EVENT_STREAM") == '1';

EventStream::Event::~Event() {}

EventStream::UserEvent::UserEvent(const std::string& message)
    : message(message) {}

EventStream::StartedPirCompiling::StartedPirCompiling(
    const Function* rirFunction, const Assumptions& assumptions)
    : rirFunctionUid(rirFunction->body()->uid), assumptions(assumptions) {}

EventStream::ReusedPirCompiled::ReusedPirCompiled(const Function* rirFunction,
                                                  size_t durationMicros)
    : rirFunctionUid(rirFunction->body()->uid), durationMicros(durationMicros) {
}

EventStream::SucceededPirCompiling::SucceededPirCompiling(
    const Function* rirFunction, size_t durationMicros)
    : rirFunctionUid(rirFunction->body()->uid), durationMicros(durationMicros) {
}

EventStream::FailedPirCompiling::FailedPirCompiling(
    const Function* rirFunction, size_t durationMicros,
    const std::string& explanation)
    : rirFunctionUid(rirFunction->body()->uid), durationMicros(durationMicros),
      explanation(explanation) {}

EventStream::Deoptimized::Deoptimized(const Function* baselineFunction,
                                      DeoptReason::Reason deoptReason)
    : baselineFunctionUid(baselineFunction->body()->uid),
      deoptReason(deoptReason) {}

void EventStream::UserEvent::print(std::ostream& out) {
    out << message << std::endl;
}

void EventStream::StartedPirCompiling::print(std::ostream& out) {
    out << "started PIR-compiling "
        << EventStream::instance().getNameOf(rirFunctionUid)
        << " with assumptions " << assumptions << std::endl;
}

void EventStream::ReusedPirCompiled::print(std::ostream& out) {
    out << "avoided PIR-compiling "
        << EventStream::instance().getNameOf(rirFunctionUid)
        << " because we could reuse an existing version (spent "
        << durationMicros << "µs figuring this out)" << std::endl;
}

void EventStream::SucceededPirCompiling::print(std::ostream& out) {
    out << "succeeded PIR-compiling "
        << EventStream::instance().getNameOf(rirFunctionUid) << " in "
        << durationMicros << "µs" << std::endl;
}

void EventStream::FailedPirCompiling::print(std::ostream& out) {
    out << "failed PIR-compiling "
        << EventStream::instance().getNameOf(rirFunctionUid) << " wasting "
        << durationMicros << "µs because " << explanation << std::endl;
}

void EventStream::Deoptimized::print(std::ostream& out) {
    out << "deoptimized "
        << EventStream::instance().getNameOf(baselineFunctionUid) << " because "
        << getDeoptReasonExplanation(deoptReason) << std::endl;
}

std::string EventStream::getNameOf(const UUID& functionUid) {
    if (!versionNames.count(functionUid)) {
        return "<unknown>";
    } else {
        return versionNames.at(functionUid);
    }
}
void EventStream::setNameOf(const Function* function, std::string name) {
    versionNames[function->body()->uid] = name;
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
    for (Event* event : events) {
        event->print(out);
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