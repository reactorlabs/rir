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

void EventStream::UserEvent::print(
    std::ostream& out, const std::vector<Event*>::const_iterator& rest,
    const std::vector<Event*>::const_iterator& end) {
    out << message << std::endl;
}

bool EventStream::UserEvent::thisPrintsItself() { return true; }

bool EventStream::UserEvent::isEndOfCompiling(const UUID& rirFunctionId) {
    return false;
}

void EventStream::StartedPirCompiling::print(
    std::ostream& out, const std::vector<Event*>::const_iterator& rest,
    const std::vector<Event*>::const_iterator& end) {
    // Print this event itself
    out << EventStream::instance().getNameOf(rirFunctionUid) << " compile ("
        << assumptions << ") => ";

    // Find and print the end-compiling event
    for (std::vector<Event*>::const_iterator it = rest; it != end; it++) {
        Event* event = *it;
        if (event->isEndOfCompiling(rirFunctionUid)) {
            // This is the end-compiling event, print it (it prints the trailing
            // newline)
            event->print(out, it, end);
            return;
        }
    }

    // There is no end-compiling event (not expected behavior)
    out << "... unfinished" << std::endl;
}

bool EventStream::StartedPirCompiling::thisPrintsItself() { return true; }

bool EventStream::StartedPirCompiling::isEndOfCompiling(
    const UUID& rirFunctionId) {
    return false;
}

void EventStream::ReusedPirCompiled::print(
    std::ostream& out, const std::vector<Event*>::const_iterator& rest,
    const std::vector<Event*>::const_iterator& end) {
    out << "avoided and reused [" << durationMicros << "µs]" << std::endl;
}

bool EventStream::ReusedPirCompiled::thisPrintsItself() { return false; }

bool EventStream::ReusedPirCompiled::isEndOfCompiling(
    const UUID& rirFunctionId) {
    return true;
}

void EventStream::SucceededPirCompiling::print(
    std::ostream& out, const std::vector<Event*>::const_iterator& rest,
    const std::vector<Event*>::const_iterator& end) {
    out << "succeeded [" << durationMicros << "µs]" << std::endl;
}

bool EventStream::SucceededPirCompiling::thisPrintsItself() { return false; }

bool EventStream::SucceededPirCompiling::isEndOfCompiling(
    const UUID& rirFunctionId) {
    return true;
}

void EventStream::FailedPirCompiling::print(
    std::ostream& out, const std::vector<Event*>::const_iterator& rest,
    const std::vector<Event*>::const_iterator& end) {
    out << "failed (" << explanation << ") [" << durationMicros << "µs]"
        << std::endl;
}

bool EventStream::FailedPirCompiling::thisPrintsItself() { return false; }

bool EventStream::FailedPirCompiling::isEndOfCompiling(
    const UUID& rirFunctionId) {
    return true;
}

void EventStream::Deoptimized::print(
    std::ostream& out, const std::vector<Event*>::const_iterator& rest,
    const std::vector<Event*>::const_iterator& end) {
    out << EventStream::instance().getNameOf(baselineFunctionUid) << " deopt ("
        << getDeoptReasonExplanation(deoptReason) << ")" << std::endl;
}

bool EventStream::Deoptimized::thisPrintsItself() { return true; }

bool EventStream::Deoptimized::isEndOfCompiling(const UUID& rirFunctionId) {
    return false;
}

std::string EventStream::getNameOf(const UUID& functionUid) {
    if (!versionNames.count(functionUid)) {
        return "<unknown>";
    } else {
        return versionNames.at(functionUid);
    }
}

void EventStream::setNameOf(const Function* function, std::string name) {
    std::string nonCollidingName =
        numVersionsWithName.count(name)
            ? name + "@" + std::to_string(numVersionsWithName.at(name))
            : name;
    numVersionsWithName[name]++;

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