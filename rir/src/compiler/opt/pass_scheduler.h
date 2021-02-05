#ifndef PIR_PASS_SCHEDULER_H
#define PIR_PASS_SCHEDULER_H

#include <set>
#include <string>

#include "pass.h"

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

    const static PassScheduler& instance() {
        static PassScheduler i;
        return i;
    }

    void run(const std::function<bool(const Pass*)>& apply) const {
        for (auto& phase : schedule_.phases) {
            auto budget = phase.budget;
            bool changed = false;
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
                    if (apply(pass.get())) {
                        changed = true;
                    }
                }
            } while (changed && budget && !phase.once);
        }
    }

  private:
    PassScheduler();

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
}
}

#endif
