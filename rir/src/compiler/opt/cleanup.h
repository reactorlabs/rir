#ifndef PIR_CLEANUP_H
#define PIR_CLEANUP_H

#include "../translations/ir_translator.h"

namespace rir {
namespace pir {

class Cleanup : public IRTranslator {
  public:
    Cleanup(RirCompiler& compiler) : IRTranslator(compiler, "cleanup"){};

    void apply(IRCode) override;
};
}
}

#endif
