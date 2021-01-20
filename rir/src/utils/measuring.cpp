#include <chrono>
#include <iomanip>
#include <iostream>
#include <map>
#include <unordered_map>

#include "measuring.h"

namespace rir {

namespace {

struct MeasuringImpl {
    std::unordered_map<std::string, double> timer;
    std::unordered_map<std::string, size_t> events;
    std::chrono::time_point<std::chrono::high_resolution_clock> start;
    const unsigned width = 64;

    MeasuringImpl() : start(std::chrono::high_resolution_clock::now()) {}

    void addTime(const std::string& name, double time) { timer[name] += time; }

    void countEvent(const std::string& name, size_t n) { events[name] += n; }

    ~MeasuringImpl() {
        std::chrono::time_point<std::chrono::high_resolution_clock> end =
            std::chrono::high_resolution_clock::now();
        std::chrono::duration<double> duration = end - start;
        std::map<double, std::string> ordered;
        double total = 0;
        for (auto t : timer) {
            while (ordered.count(t.second))
                t.second += 1e-20;
            ordered.emplace(t.second, t.first);
            total += t.second;
        }

        std::cerr << "\n\n=== Measurments breakdown (total lifetime: "
                  << format(duration.count()) << ") ===\n\n";
        if (!ordered.empty()) {
            std::cerr << "Timer total: " << format(total) << " ("
                      << (total / duration.count()) * 100 << "%)\n"
                      << "      (rest: " << format(duration.count() - total)
                      << " (" << ((1. - (total / duration.count())) * 100)
                      << "%)\n";
            for (auto t : ordered)
                std::cerr << "" << std::setw(width) << t.second << "\t"
                          << format(t.first) << "\n";
            std::cerr << "\n";
        }
        if (!events.empty()) {
            std::cerr << "    Events:\n";
            for (auto e : events) {
                std::cerr << "" << std::setw(width) << e.first << "\t"
                          << readable(e.second) << " (" << e.second << ")\n";
            }
            std::cerr << "\n";
        }
        std::cerr << std::endl;
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
        if (n > 1000U * 1000U * 1000U * 1000U)
            ss << (n / 1000 / 1000 / 1000 / 1000) << "T";
        else if (n > 1000U * 1000U * 1000U)
            ss << (n / 1000 / 1000 / 1000) << "G";
        else if (n > 1000U * 1000U)
            ss << (n / 1000 / 1000) << "M";
        else if (n > 1000U)
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
        Measuring::countEvent("!!! startTimer: timerActive");
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
        Measuring::countEvent("!!! countTimer: !timerActive");
    }
}

void Measuring::addTime(const std::string& name, double time) {
    if (!m)
        return;
    m->addTime(name, time);
}

void Measuring::countEvent(const std::string& name, size_t n) {
    if (!m)
        return;
    m->countEvent(name, n);
}

} // namespace rir
