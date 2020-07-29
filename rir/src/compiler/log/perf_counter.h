#ifndef PIR_PERF_COUNTER_H
#define PIR_PERF_COUNTER_H

#include <iomanip>
#include <iostream>
#include <map>
#include <unordered_map>

namespace rir {
namespace pir {

class CompilerPerf {
    std::unordered_map<std::string, double> passTimer;

  public:
    void addTime(const std::string& name, double time) {
        if (!passTimer.count(name))
            passTimer[name] = 0;
        passTimer.at(name) += time;
    }

    ~CompilerPerf() {
        std::map<double, std::string> ordered;
        double total = 0;
        for (auto t : passTimer) {
            while (ordered.count(t.second))
                t.second += 1e-20;
            ordered.emplace(t.second, t.first);
            total += t.second;
        }

        std::cerr << "=== COMPILER perf Breakdown:\n";
        for (auto t : ordered)
            std::cerr << "" << std::setw(24) << t.second << "\t" << t.first
                      << "\n";
        std::cerr << "" << std::setw(24) << "total"
                  << "\t" << total << "\n";
    }
};

} // namespace pir
} // namespace rir

#endif
