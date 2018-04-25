#ifndef PIR_ELIDE_ENV_H
#define PIR_ELIDE_ENV_H

#include "../translations/ir_translator.h"

namespace rir {
namespace pir {

/*
 * ElideEnv removes envrionments which are not needed. It looks at all uses of
 * a `MkEnv` instruction. If the environment does not leak, and none of the
 * uses have any effect (besides changing the unnecessary environment), then it
 * can be removed.
 *
 */
class ElideEnv : public IRTranslator {
  public:
    ElideEnv(RirCompiler& compiler)
        : IRTranslator(compiler, "Elide Environment"){};

    void apply(IRCode) override;
};
}
}

#endif
