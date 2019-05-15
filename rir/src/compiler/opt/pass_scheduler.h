#ifndef PIR_PASS_SCHEDULER_H
#define PIR_PASS_SCHEDULER_H

#include "compiler/translations/pir_translator.h"
#include <set>

namespace rir {
namespace pir {

class PassScheduler {
  public:
    typedef std::vector<std::unique_ptr<const PirTranslator>> Schedule;

    const static PassScheduler& instance() {
        static PassScheduler i;
        return i;
    }

    Schedule::const_iterator begin() const { return schedule_.cbegin(); }
    Schedule::const_iterator end() const { return schedule_.cend(); }

  private:
    PassScheduler();

    Schedule schedule_;

    void add(std::unique_ptr<const PirTranslator>&&);

    template <typename PASS>
    void add() {
        add(std::unique_ptr<const PirTranslator>(new PASS()));
    }
    template <typename PASS>
    void add(const std::string& name) {
        add(std::unique_ptr<const PirTranslator>(new PASS(name)));
    }
};
} // namespace pir
} // namespace rir

#endif
