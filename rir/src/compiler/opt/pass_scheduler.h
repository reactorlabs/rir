#ifndef PIR_PASS_SCHEDULER_H
#define PIR_PASS_SCHEDULER_H

#include "compiler/translations/pir_translator.h"
#include <set>
#include <unordered_map>

namespace rir {
namespace pir {

class PassScheduler {
  public:
    struct Phase {
        Phase(const std::string& name, unsigned budget)
            : name(name), budget(budget), once(budget == 0) {}
        const std::string& name;
        unsigned budget;
        bool once;
        typedef std::vector<std::unique_ptr<const PirTranslator>> Passes;
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

    void run(const std::function<bool(const PirTranslator*)>& apply) const {
        size_t counter = 0;
        size_t lastChange = 1;
        lastPassRun.clear();
        for (auto& phase : schedule_.phases) {
            auto budget = phase.budget;
            bool changed = false;
            do {
                changed = false;
                for (auto& pass : phase.passes) {
                    counter++;
                    if (!phase.once) {
                        if (budget <= pass->cost()) {
                            budget = 0;
                            break;
                        }
                        // We assume that if no change occured since last
                        // running this pass, we may safely skip it (if the last
                        // change was done by this pass, we skip too - the pass
                        // should do everything in a single invocation)
                        if (lastChange <= lastPassRun[pass->getName()]) {
                            continue;
                        }
                        budget -= pass->cost();
                    }
                    lastPassRun[pass->getName()] = counter;
                    if (apply(pass.get())) {
                        changed = true;
                        lastChange = counter;
                    }
                }
            } while (changed && budget && !phase.once);
        }
    }

  private:
    PassScheduler();

    Schedule schedule_;
    Schedule::Phases::iterator currentPhase;

    mutable std::unordered_map<std::string, size_t> lastPassRun;

    void add(std::unique_ptr<const PirTranslator>&&);

    template <typename PASS>
    void add() {
        add(std::unique_ptr<const PirTranslator>(new PASS()));
    }

    void nextPhase(const std::string& name, unsigned budget = 0);
};
}
}

#endif
