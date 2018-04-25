#ifndef PIR_DELAY_INSTR_H
#define PIR_DELAY_INSTR_H

#include "../translations/ir_translator.h"

namespace rir {
namespace pir {

/*
 * DelayInstr tries to schedule instruction right before they are needed.
 *
 */
class DelayInstr : public IRTranslator {
  public:
    DelayInstr(RirCompiler& compiler)
        : IRTranslator(compiler, "Delay Instructions"){};

    void apply(IRCode) override;
};
}
}

#endif
