#ifndef RIR_COUNTERS_H
#define RIR_COUNTERS_H

#include <algorithm>
#include <cassert>
#include <fstream>
#include <iostream>
#include <string>
#include <vector>

namespace rir {

class EventCounters {
    std::vector<size_t> counters;
    std::vector<std::string> names;
    EventCounters() {}

  public:
    static EventCounters& instance() {
        static EventCounters c;
        return c;
    }
    unsigned registerCounter(const std::string& name) {
#ifndef MEASURE
        assert(false);
#endif
        if (!enabled())
            return 0;
        auto existing = std::find(names.begin(), names.end(), name);
        if (existing != names.end()) {
            return existing - names.begin();
        }
        names.push_back(name);
        counters.push_back(0);
        return counters.size() - 1;
    }
    void count(unsigned counter, size_t n = 1) { counters.at(counter) += n; }
    ~EventCounters() {
        if (enabled()) {
            std::ofstream file;
            file.open("rir_statistics.csv");
            for (unsigned i = 0; i < counters.size(); ++i) {
                file << names.at(i) << ", " << counters.at(i) << "\n";
            }
            file.close();
        }
    }
    static bool enabled() {
        static bool isEnabled = getenv("ENABLE_EVENT_COUNTERS") &&
                                *getenv("ENABLE_EVENT_COUNTERS") == '1';
        return isEnabled;
    }
};

#ifdef MEASURE
#define ENABLE_EVENT_COUNTERS EventCounters::enabled()

static unsigned LlvmEvaled =
    EventCounters::instance().registerCounter("LLVM evaled");
static unsigned RirEvaled =
    EventCounters::instance().registerCounter("RIR evaled");
static unsigned PirOptimized =
    EventCounters::instance().registerCounter("PIR optimized");
static unsigned Unoptimizable =
    EventCounters::instance().registerCounter("unoptimizable");
static unsigned LlvmLowered =
    EventCounters::instance().registerCounter("LLVM lowered");
static unsigned RirLowered =
    EventCounters::instance().registerCounter("RIR lowered");
static unsigned Deopt = EventCounters::instance().registerCounter("deopt");

#endif

} // namespace rir

#endif
