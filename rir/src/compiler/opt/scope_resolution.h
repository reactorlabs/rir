#ifndef PIR_SCOPE_RESOLUTION_H
#define PIR_SCOPE_RESOLUTION_H

#include "../translations/ir_translator.h"

namespace rir {
namespace pir {

/*
 * Uses scope analysis to get rid of as many `LdVar`'s as possible.
 *
 * Similar to llvm's mem2reg pass, we try to lift as many loads from the R
 * environment, to pir SSA variables.
 *
 */
class ScopeResolution : public IRTranslator {
  public:
    ScopeResolution(RirCompiler& compiler)
        : IRTranslator(compiler, "scope resulotion"){};

    void apply(IRCode) override;
};
}
}

#endif
