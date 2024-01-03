#ifndef PIR_PASS_SCHEDULER_H
#define PIR_PASS_SCHEDULER_H

#include <set>
#include <string>

#include "pass.h"

#include <fstream>

//#define PASS_SCHEDULER_DEBUG

namespace rir {
namespace pir {

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

    // const static bool PASS_SCHEDULER_DEBUG = true;

    void run(const std::function<bool(const Pass*, size_t)>& apply) const {

#ifdef PASS_SCHEDULER_DEBUG
        static int invk = 0;
        std::ostream& ost = std::cerr;
#endif

        // std::ofstream ost;
        // ost.open("iters.txt", std::ios_base::app); // append instead of
        // overwrite

#ifdef PASS_SCHEDULER_DEBUG
        ost << " NEW scheduler run !!  "
               "---------------------------------------------------------------"
               "******************************"
            << invk << "\n";
        invk++;
    }
#endif

    for (auto& phase : schedule_.phases) {

#ifdef PASS_SCHEDULER_DEBUG
        ost << phase.name << " - size:  " << phase.passes.size()
            << " // NEW PHASE!!  "
               "---------------------------------------------------------------"
            << "\n";
#endif

        auto budget = phase.budget;
        bool changed = false;
        int iteration = 0;

        do {
            changed = false;

            // first element phase.passes  is always a PhaseMarker
            for (auto& pass : phase.passes) {

#ifdef PASS_SCHEDULER_DEBUG
                if (pass->isPhaseMarker()) {
                    ost << "-------------- new iter started! -------------- "
                        << iteration << " \n";
                }
#endif

                if (!phase.once) {
                    // if (budget < pass->cost()) {
                    //     budget = 0;
                    //     break;
                    // }
                    // budget -= pass->cost();
                }

                bool applyRes = apply(pass.get(), iteration);
                if (applyRes) {
                    changed = true;
                }

#ifdef PASS_SCHEDULER_DEBUG
                // if (iteration >= 20) {
                ost << "invk:" << invk << " - " << phase.name
                    << " - PASS: " << pass->getName() << " - res: " << applyRes
                    << " - iter: " << iteration << (applyRes ? " *****" : "")
                    << "\n";

                ost.flush();
                //}
#endif
            }
            iteration++;

            auto maxIter = 60;
            if (iteration >= maxIter) {
                std::cerr << "more than " << maxIter << " iterations!";
                assert(false);
            }
        } while (changed && budget && !phase.once);
    }
    }

  private:
    explicit PassScheduler(unsigned optLevel, bool isFinal);

    Schedule schedule_;
    Schedule::Phases::iterator currentPhase;

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
