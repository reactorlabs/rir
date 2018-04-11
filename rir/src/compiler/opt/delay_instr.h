#ifndef PIR_DELAY_INSTR_H
#define PIR_DELAY_INSTR_H

#include "../translations/pir_translator.h"

namespace rir {
namespace pir {

/*
 * DelayInstr tries to schedule instruction right before they are needed.
 *
 */
class Closure;
class DelayInstr : public PirTranslator {
  public:
    DelayInstr() : PirTranslator("Delay Instructions"){};

    void apply(Closure* function) override;
};
}
}

#endif
