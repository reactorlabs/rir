#include "event_stream.h"

#include "runtime/Function.h"

namespace rir {

bool EventStream::isEnabled =
    getenv("ENABLE_EVENT_STREAM") && *getenv("ENABLE_EVENT_STREAM") == '1';

EventStream::Event::~Event() {}

void EventStream::UserEvent::print(std::ostream& out) {
    out << message << std::endl;
}

void EventStream::StartedPirCompiling::print(std::ostream& out) {
    out << "started PIR-compiling "
        << EventStream::instance().getNameOf(rirFunction)
        << " with assumptions " << assumptions << std::endl;
}

void EventStream::ReusedPirCompiled::print(std::ostream& out) {
    out << "avoided PIR-compiling "
        << EventStream::instance().getNameOf(rirFunction)
        << " because we could reuse an existing version (spent "
        << durationMicros << " figuring this out)" << std::endl;
}

void EventStream::SucceededPirCompiling::print(std::ostream& out) {
    out << "succeeded PIR-compiling "
        << EventStream::instance().getNameOf(rirFunction) << " in "
        << durationMicros << "µs" << std::endl;
}

void EventStream::FailedPirCompiling::print(std::ostream& out) {
    out << "failed PIR-compiling "
        << EventStream::instance().getNameOf(rirFunction) << " wasting "
        << durationMicros << "µs because " << explanation << std::endl;
}

void EventStream::Deoptimized::print(std::ostream& out) {
    out << "deoptimized " << EventStream::instance().getNameOf(baselineFunction)
        << " because " << getDeoptReasonExplanation(deoptReason) << std::endl;
}

std::string EventStream::getNameOf(const Function* function) {
    return versionNames.at(function->body()->uid);
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
    file.open("event_stream.csv");
    print(file);
    file.close();
}
void EventStream::flush() {
    printToFile();
    reset();
}

} // namespace rir