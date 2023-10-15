#ifndef PIR_PASS_SCHEDULER_H
#define PIR_PASS_SCHEDULER_H

#include <set>
#include <string>

#include "pass.h"

#include <fstream>
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

    void run(const std::function<bool(const Pass*, size_t)>& apply) const {
        // static int invk = 0;

        // std::ostream& ost = std::cerr;

        // std::ofstream ost;
        // ost.open("iters.txt", std::ios_base::app); // append instead of
        // overwrite

        // ost <<  " NEW scheduler run !!
        // ---------------------------------------------------------------******************************"
        // << invk   << "\n"; invk++;

        for (auto& phase : schedule_.phases) {

            // ost << phase.name << " - size:  " << phase.passes.size() <<  "
            // NEW PHASE!!
            // ---------------------------------------------------------------"
            // << "\n";

            auto budget = phase.budget;
            bool changed = false;
            int iteration = 0;

            do {
                changed = false;

                // first element phase.passes  is always a PhaseMarker
                for (auto& pass : phase.passes) {
                    // if (pass->isPhaseMarker()) {
                    //     ost << "-------------- new iter started!
                    //     -------------- " << iteration << " \n";
                    // }
                    if (!phase.once) {
                        // if (budget < pass->cost()) {
                        //     budget = 0;
                        //     break;
                        // }
                        // budget -= pass->cost();
                    }

                    // if (iteration >=50 && phase.name == "Initial" &&
                    // pass.get()->getName() != "TypefeedbackCleanup") {
                    //     //std::cerr <<  "skipping " << pass.get()->getName()
                    //     << "\n"; continue;
                    // }

                    bool applyRes = apply(pass.get(), iteration);
                    if (applyRes) {
                        changed = true;
                    }

                    // if (iteration >= 20) {
                    // ost << "invk:" << invk <<" - " << phase.name << " - PASS:
                    // " << pass->getName()
                    //           << " - res: " << applyRes
                    //           << " - iter: " << iteration
                    //           << (applyRes ? " *****" : "") << "\n";

                    // ost.flush();
                    //}
                }
                iteration++;
                if (iteration >= 50) {
                    assert(false && "more than 50 iterations!");
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
