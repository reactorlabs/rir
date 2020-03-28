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
    static bool isEnabled;
    static EventCounters& instance() {
        static EventCounters c;
        return c;
    }
    unsigned registerCounter(const std::string& name) {
#ifndef MEASURE
        assert(false);
#endif
        auto existing = std::find(names.begin(), names.end(), name);
        if (existing != names.end()) {
            return existing - names.begin();
        }
        names.push_back(name);
        counters.push_back(0);
        return counters.size() - 1;
    }
    void count(unsigned counter, size_t n = 1) { counters.at(counter) += n; }
    bool aCounterIsNonzero() {
        for (size_t counter : counters) {
            if (counter > 0) {
                return true;
            }
        }
        return false;
    }
    ~EventCounters() {
        if (aCounterIsNonzero()) {
            std::ofstream file;
            file.open("rir_statistics.csv");
            for (unsigned i = 0; i < counters.size(); ++i) {
                file << names.at(i) << ", " << counters.at(i) << "\n";
            }
            file.close();
        }
    }
};

#ifdef MEASURE
#define ENABLE_EVENT_COUNTERS EventCounters::isEnabled

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
