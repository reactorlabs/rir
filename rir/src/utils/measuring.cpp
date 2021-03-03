#include <chrono>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <map>
#include <set>
#include <string>
#include <unordered_map>
#include <sstream>

#include "measuring.h"

namespace rir {

namespace {

struct MeasuringImpl {
    size_t EVENT_THRESHOLD = 0;
    std::unordered_map<std::string, double> timers;
    std::pair<size_t, size_t> timerMismatches;
    std::unordered_map<std::string, size_t> events;
    std::chrono::time_point<std::chrono::high_resolution_clock> start;
    const unsigned width = 40;

    MeasuringImpl() : start(std::chrono::high_resolution_clock::now()) {}

    void addTime(const std::string& name, double time) { timers[name] += time; }

    void timerAlreadyRunning() { timerMismatches.first++; }
    void timerNotStarted() { timerMismatches.second++; }

    void countEvent(const std::string& name, size_t n) { events[name] += n; }

    ~MeasuringImpl() {
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

    void dump(std::ostream& out) {
        std::chrono::time_point<std::chrono::high_resolution_clock> end =
            std::chrono::high_resolution_clock::now();

        std::chrono::duration<double> duration = end - start;
        out << "=== Measurments breakdown (total lifetime: "
            << format(duration.count()) << ") ===\n";

        {
            std::map<double, std::string> orderedTimers;
            double totalTimer = 0;
            for (auto& t : timers) {
                while (orderedTimers.count(t.second))
                    t.second += 1e-20;
                orderedTimers.emplace(t.second, t.first);
                totalTimer += t.second;
            }
            if (!orderedTimers.empty()) {
                out << "= Timers   (total: " << format(totalTimer) << " ~ "
                    << (totalTimer / duration.count()) * 100 << "%)\n"
                    << "           (rest:  "
                    << format(duration.count() - totalTimer) << " ~ "
                    << ((1. - (totalTimer / duration.count())) * 100) << "%)\n";
                for (auto& t : orderedTimers)
                    out << "" << std::setw(width) << t.second << "\t"
                        << format(t.first) << "\n";
            }
        }

        if (timerMismatches.first || timerMismatches.second) {
            out << "= Timer mismatches\n";
            if (timerMismatches.first)
                out << "" << std::setw(width)
                    << "Timer started when already running"
                    << "\t" << timerMismatches.first << "\n";
            if (timerMismatches.second)
                out << "" << std::setw(width)
                    << "Timer counted when not started"
                    << "\t" << timerMismatches.second << "\n";
        }

        {
            std::map<size_t, std::set<std::string>> orderedEvents;
            for (auto& e : events)
                if (e.second > EVENT_THRESHOLD)
                    orderedEvents[e.second].insert(e.first);
            if (!orderedEvents.empty()) {
                out << "= Events";
                if (EVENT_THRESHOLD)
                    out << "   (threshold: " << EVENT_THRESHOLD << ")";
                out << "\n";
                for (auto& e : orderedEvents) {
                    for (auto& n : e.second) {
                        out << "" << std::setw(width) << n << "\t"
                            << readable(e.first) << " (" << e.first << ")\n";
                    }
                }
            }
        }

        out << std::flush;
    }

    static std::string format(double secs) {
        std::stringstream ss;
        if (secs < 60)
            ss << secs << " s";
        else if (secs < 60 * 60)
            ss << secs / 60 << " m";
        else
            ss << secs / 60 / 60 << " h";
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
};

} // namespace

std::chrono::time_point<std::chrono::high_resolution_clock> startTime;
std::chrono::time_point<std::chrono::high_resolution_clock> endTime;
bool timerActive = false;
std::unique_ptr<MeasuringImpl> m = std::unique_ptr<MeasuringImpl>(
    getenv("PIR_MEASURING") ? new MeasuringImpl : nullptr);

void Measuring::startTimer() {
    if (!m)
        return;
    if (!timerActive) {
        timerActive = true;
        startTime = std::chrono::high_resolution_clock::now();
    } else {
        m->timerAlreadyRunning();
    }
}

void Measuring::countTimer(const std::string& name) {
    if (!m)
        return;
    if (timerActive) {
        endTime = std::chrono::high_resolution_clock::now();
        std::chrono::duration<double> duration = endTime - startTime;
        Measuring::addTime(name, duration.count());
        timerActive = false;
    } else {
        m->timerNotStarted();
    }
}

void Measuring::addTime(const std::string& name, double time) {
    if (!m)
        return;
    m->addTime(name, time);
}

void Measuring::setEventThreshold(size_t n) {
    if (!m)
        return;
    m->EVENT_THRESHOLD = n;
}

void Measuring::countEvent(const std::string& name, size_t n) {
    if (!m)
        return;
    m->countEvent(name, n);
}

} // namespace rir
