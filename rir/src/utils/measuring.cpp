#include <chrono>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <map>
#include <set>
#include <sstream>
#include <string>
#include <unordered_map>
#include <unordered_set>

#include "RangeSet.h"
#include "runtime/Code.h"
#include "runtime/DispatchTable.h"
#include "runtime/Function.h"
#include "utils/measuring.h"

namespace rir {

using TimePoint = std::chrono::time_point<std::chrono::high_resolution_clock>;
using Duration = std::chrono::duration<double>;

struct Measuring::TimingEvent {
    const std::string& name;
    SEXP associated = nullptr;
    TimePoint start;
};

namespace {

struct MeasuringImpl {
    struct TimedEvent {
        TimePoint start;
        TimePoint end;
    };
    struct Timer {
        double timer = 0;
        bool timerActive = false;
        TimePoint start;
        size_t alreadyRunning = 0;
        size_t notStarted = 0;
    };
    std::unordered_map<std::string, std::unordered_map<SEXP, std::vector<TimedEvent>>> timedEvents;
    std::unordered_map<std::string, Timer> timers;
    std::unordered_map<std::string, size_t> events;
    std::unordered_map<SEXP, std::string> associatedLatestDumps;
    TimePoint start;
    TimePoint end;
    size_t threshold = 0;
    const unsigned width = 40;
    const unsigned maxTimedEventsToPrint = 1000;
    bool shouldOutput = false;

    MeasuringImpl() : start(std::chrono::high_resolution_clock::now()) {}

    ~MeasuringImpl() {
        end = std::chrono::high_resolution_clock::now();
        auto logfile = getenv("PIR_MEASURING_LOGFILE");
        if (logfile) {
            std::ofstream fs(logfile);
            if (!fs) {
                std::cerr << "ERROR: Can't open logfile '" << logfile
                          << "'. Writing to std::cerr.\n";
                dump(std::cerr);
            } else {
                dump(fs);
            }
        } else {
            dump(std::cerr);
        }
    }

    void updateAssociatedDump(SEXP associated, bool associatedIsInitialized) {
        std::stringstream s;
        if (!associatedIsInitialized) {
            s << "(not yet initialized)\n";
        } else if (auto d = DispatchTable::check(associated)) {
            d->print(s, true);
        } else if (auto f = Function::check(associated)) {
            f->print(s, true);
        } else if (auto c = Code::check(associated)) {
            c->print(s, true);
        }
        std::string str = s.str();
        if (!str.empty()) {
            associatedLatestDumps[associated] = str;
        }
    }

    void dump(std::ostream& out) {
        if (!shouldOutput)
            return;

        std::chrono::duration<double> totalLifetime = end - start;
        out << "\n---== Measuring breakdown ===---\n\n";
        out << "  Total lifetime: " << format(totalLifetime.count()) << "\n\n";

        {
            std::map<double, std::tuple<std::string, size_t, size_t, double>>
                orderedTimers;
            double totalTimers = 0;
            for (auto& t : timers) {
                auto& key = t.second.timer;
                while (orderedTimers.count(key))
                    key += 1e-20;
                double notStopped = 0;
                if (t.second.timerActive) {
                    Duration duration = end - t.second.start;
                    notStopped = duration.count();
                }
                orderedTimers.emplace(
                    key, std::make_tuple(t.first, t.second.alreadyRunning,
                                         t.second.notStarted, notStopped));
                totalTimers += key;
            }
            if (!orderedTimers.empty()) {
                out << "  Timers (" << format(totalTimers) << " in total, or "
                    << std::setprecision(2)
                    << (totalTimers / totalLifetime.count() * 100) << "%):\n";
                for (auto& t : orderedTimers) {
                    auto& name = std::get<0>(t.second);
                    out << "    " << std::setw(width) << name << "\t"
                        << format(t.first);
                    if (auto& alreadyRunning = std::get<1>(t.second)) {
                        out << "  (started " << alreadyRunning
                            << "x while running)!";
                    }
                    if (auto& notStarted = std::get<2>(t.second)) {
                        out << "  (counted " << notStarted
                            << "x while not running)!";
                    }
                    if (auto& notStopped = std::get<3>(t.second)) {
                        out << "  (not stopped, measured extra "
                            << format(notStopped) << ")!";
                    }
                    out << "\n";
                }
                out << "\n";
            }
        }

        {
            std::map<size_t, std::set<std::string>> orderedEvents;
            for (auto& e : events)
                if (e.second >= threshold)
                    orderedEvents[e.second].insert(e.first);
            if (!orderedEvents.empty()) {
                out << "  Events";
                if (threshold)
                    out << " (threshold " << threshold << ")";
                out << ":\n";
                for (auto& e : orderedEvents) {
                    for (auto& n : e.second) {
                        out << "    " << std::setw(width) << n << "\t"
                            << e.first << " (~ " << readable(e.first) << ")\n";
                    }
                }
            }
        }

        {
            std::map<Duration, std::tuple<std::string, size_t>>
                timedEventSuperSumsOrderedByDuration;
            std::map<Duration, std::tuple<std::string, SEXP, size_t>>
                timedEventSumsOrderedByDuration;
            std::map<TimePoint, std::tuple<std::string, SEXP, Duration>>
                timedEventsOrderedChronologically;
            RangeSet<TimePoint> totalTimedEventsRange;
            size_t totalTimedEventsCount = 0;
            for (auto& a : timedEvents) {
                auto& name = a.first;
                RangeSet<TimePoint> superRange;
                size_t superCount = 0;
                for (auto& b : a.second) {
                    auto& associated = b.first;
                    RangeSet<TimePoint> range;
                    for (auto& e : b.second) {
                        auto duration = e.end - e.start;
                        timedEventsOrderedChronologically.emplace(
                            e.start,
                            std::make_tuple(name, associated, duration));
                        range.insert(e.start, e.end);
                    }
                    auto sum = sumRangeSet(range);
                    timedEventSumsOrderedByDuration.emplace(
                        sum, std::make_tuple(name, associated, b.second.size()));
                    superRange.insert_all(range);
                    superCount += b.second.size();
                }
                auto superSum = sumRangeSet(superRange);
                timedEventSuperSumsOrderedByDuration.emplace(
                    superSum, std::make_tuple(name, superCount));
                totalTimedEventsRange.insert_all(superRange);
                totalTimedEventsCount += superCount;
            }
            if (!timedEventsOrderedChronologically.empty()) {
                auto totalTimedEventsDuration = sumRangeSet(totalTimedEventsRange);
                out << "  Timed events (total count = " << totalTimedEventsCount << ", time = "
                    << format(totalTimedEventsDuration) << ", ratio to total lifetime = "
                    << std::setprecision(2)
                    << (totalTimedEventsDuration.count() / totalLifetime.count() * 100) << "%):\n";
                out << "  Super sums ordered by duration:\n";
                size_t totalCount = 0;
                for (auto it = timedEventSuperSumsOrderedByDuration.rbegin();
                     it != timedEventSuperSumsOrderedByDuration.rend(); ++it) {
                    auto& t = *it;
                    auto& name = std::get<0>(t.second);
                    auto count = std::get<1>(t.second);
                    auto duration = t.first;
                    out << "    " << std::setw((int)width) << name << "\t"
                        << count << "\t" << format(duration);
                    out << "\n";
                    if (totalCount++ > maxTimedEventsToPrint) {
                        out << "    ... (omitted)\n";
                        break;
                    }
                }
                out << "  Sums ordered by duration:\n";
                std::unordered_set<SEXP> printedAssociateds;
                totalCount = 0;
                for (auto it = timedEventSumsOrderedByDuration.rbegin();
                     it != timedEventSumsOrderedByDuration.rend(); ++it) {
                    auto& t = *it;
                    auto& name = std::get<0>(t.second);
                    auto& associated = std::get<1>(t.second);
                    auto count = std::get<2>(t.second);
                    auto duration = t.first;
                    out << "    " << std::setw((int)width) << name << "\t"
                        << associated << "\t" << count << "\t"
                        << format(duration);
                    out << "\n";
                    printedAssociateds.insert(associated);
                    if (totalCount++ > maxTimedEventsToPrint) {
                        out << "    ... (omitted)\n";
                        break;
                    }
                }
                out << "  All ordered chronologically:\n";
                totalCount = 0;
                for (auto& t : timedEventsOrderedChronologically) {
                    auto& name = std::get<0>(t.second);
                    auto& associated = std::get<1>(t.second);
                    auto duration = std::get<2>(t.second);
                    out << "    " << std::setw(width) << name << "\t"
                        << associated << "\t" << format(duration);
                    out << "\n";
                    printedAssociateds.insert(associated);
                    if (totalCount++ > maxTimedEventsToPrint) {
                        out << "    ... (omitted)\n";
                        break;
                    }
                }
                out << "  Associated latest dumps:\n";
                for (auto& a : printedAssociateds) {
                    if (associatedLatestDumps.count(a)) {
                        out << "    " << std::setw(width) << a;
                        out << "\n" << associatedLatestDumps.at(a) << "\n";
                    }
                }
                out << "\n";
            }
        }

        out << std::flush;
    }

    static std::string format(Duration secs) {
        return format(secs.count());
    }

    static std::string format(double secs) {
        std::stringstream ss;
        if (secs < 0.000001)
                ss << secs * 1000 * 1000 * 1000 << " ns";
        else if (secs < 0.001)
            ss << secs * 1000 * 1000 << " µs";
        else if (secs < 1)
            ss << secs * 1000 << " ms";
        else if (secs < 60)
            ss << secs << " secs";
        else if (secs < 60 * 60)
            ss << secs / 60 << " min";
        else
            ss << secs / 60 / 60 << " hrs";
        return ss.str();
    }

    static std::string readable(size_t n) {
        std::stringstream ss;
        if (n > 1000UL * 1000UL * 1000UL * 1000UL)
            ss << (n / 1000 / 1000 / 1000 / 1000) << "T";
        else if (n > 1000UL * 1000UL * 1000UL)
            ss << (n / 1000 / 1000 / 1000) << "G";
        else if (n > 1000UL * 1000UL)
            ss << (n / 1000 / 1000) << "M";
        else if (n > 1000UL)
            ss << (n / 1000) << "K";
        else
            ss << n;
        return ss.str();
    }

    Duration sumRangeSet(const RangeSet<TimePoint>& set) {
        auto sum = Duration::zero();
        for (auto& r : set) {
            sum += r.second - r.first;
        }
        return sum;
    }
};

} // namespace

std::unique_ptr<MeasuringImpl> m = std::make_unique<MeasuringImpl>();

Measuring::TimingEvent* Measuring::startTimingEvent(const std::string& name, SEXP associated) {
    startTimer(name);
    m->shouldOutput = true;
    auto start = std::chrono::high_resolution_clock::now();
    return new Measuring::TimingEvent{name, associated, start};
}

void Measuring::stopTimingEvent(rir::Measuring::TimingEvent* timing,
                                bool associatedIsInitialized) {
    assert(timing);
    countTimer(timing->name);
    m->updateAssociatedDump(timing->associated, associatedIsInitialized);
    auto end = std::chrono::high_resolution_clock::now();
    MeasuringImpl::TimedEvent timed{timing->start, end};
    m->timedEvents[timing->name][timing->associated].push_back(timed);
    delete timing;
}

void Measuring::startTimer(const std::string& name) {
    m->shouldOutput = true;
    auto& t = m->timers[name];
    if (t.timerActive) {
        t.alreadyRunning++;
    } else {
        t.timerActive = true;
        t.start = std::chrono::high_resolution_clock::now();
    }
}

void Measuring::countTimer(const std::string& name) {
    auto end = std::chrono::high_resolution_clock::now();
    m->shouldOutput = true;
    auto& t = m->timers[name];
    if (!t.timerActive) {
        t.notStarted++;
    } else {
        t.timerActive = false;
        std::chrono::duration<double> duration = end - t.start;
        Measuring::addTime(name, duration.count());
    }
}

void Measuring::addTime(const std::string& name, double time) {
    m->shouldOutput = true;
    m->timers[name].timer += time;
}

void Measuring::setEventThreshold(size_t n) {
    m->shouldOutput = true;
    m->threshold = n;
}

void Measuring::countEvent(const std::string& name, size_t n) {
    m->shouldOutput = true;
    m->events[name] += n;
}

void Measuring::reset(bool outputOld) {
    if (m)
        m->shouldOutput = outputOld;
    m.reset(new MeasuringImpl());
}

} // namespace rir
