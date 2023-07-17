#ifndef PIR_PASS_SCHEDULER_H
#define PIR_PASS_SCHEDULER_H

#include <chrono>
#include <fstream>
#include <iostream>
#include <set>
#include <string>
#include <vector>

#include "pass.h"

namespace rir {
namespace pir {

class PassesReport {
  public:
    struct PassReport {
        PassReport(const std::string& name, unsigned current_budget,
                   int iteration, const std::string& phase, unsigned run)
            : name(name), current_budget(current_budget), phase(phase),
              iteration(iteration), run_k(run) {}
        std::string name;
        unsigned current_budget;
        std::string phase;
        int iteration;
        unsigned run_k;
        std::chrono::duration<double> exec_time;
    };

    PassesReport() { passes.reserve(100); }

    void add(const PassReport&& pass_report) {
        passes.push_back(pass_report);
        start = std::chrono::high_resolution_clock::now();
    }

    void stop_pass_counter() {
        passes.back().exec_time =
            start - std::chrono::high_resolution_clock::now();
    }

    ~PassesReport() {
        auto profile_file = getenv("PIR_PASS_SCHEDULER_LOGFILE");
        if (profile_file) {
            std::ofstream fs(profile_file);
            if (!fs) {
                std::cerr << "ERROR: Can't open profile logfile '"
                          << profile_file << "'. Writing to std::cerr.\n";
                dump(std::cerr);
            } else {
                dump(fs);
            }
        } else {
            dump(std::cerr);
        }
    }

    void dump(std::ostream& out) {
        out << "pass,current_budget,iteration,phase,run\n";
        for (auto pass : passes) {
            out << pass.name << "," << pass.current_budget << ","
                << pass.exec_time.count() << "," << pass.iteration << ","
                << pass.phase << "," << pass.run_k << "\n";
        }

        out << std::flush;
    }

  private:
    std::chrono::time_point<std::chrono::high_resolution_clock> start;
    std::vector<PassReport> passes;
};

class PassScheduler {
  public:
    struct Phase {
        Phase(const std::string& name, unsigned budget)
            : name(name), budget(budget), once(budget == 0) {}
        std::string name;
        unsigned budget;
        bool once;
        typedef std::vector<std::unique_ptr<const Pass>> Passes;
        Passes passes;
    };
    struct Schedule {
        typedef std::vector<Phase> Phases;
        Phases phases;
    };

    const static PassScheduler& instance();
    const static PassScheduler& quick();

    void run(const std::function<bool(const Pass*, size_t)>& apply) const {
        for (auto& phase : schedule_.phases) {
            auto budget = phase.budget;
            bool changed = false;
            int iteration = 0;
            do {
                changed = false;
                for (auto& pass : phase.passes) {
                    if (!phase.once) {
                        if (budget < pass->cost()) {
                            budget = 0;
                            break;
                        }
                        budget -= pass->cost();
                    }
                    passes_report.add(PassesReport::PassReport(
                        pass->getName(), budget, iteration, phase.name, run_k));
                    changed = apply(pass.get(), iteration);
                    passes_report.stop_pass_counter();
                }
                iteration++;
            } while (changed && budget && !phase.once);
        }
        run_k++;
    }

  private:
    explicit PassScheduler(unsigned optLevel, bool isFinal);

    Schedule schedule_;
    Schedule::Phases::iterator currentPhase;
    mutable PassesReport passes_report;
    inline static unsigned run_k = 0;

    // cppcheck-suppress unusedPrivateFunction
    void add(std::unique_ptr<const Pass>&&);

    template <typename PASS>
    void add() {
        add(std::unique_ptr<const Pass>(new PASS()));
    }

    void nextPhase(const std::string& name, unsigned budget = 0);
};

} // namespace pir
} // namespace rir

#endif
