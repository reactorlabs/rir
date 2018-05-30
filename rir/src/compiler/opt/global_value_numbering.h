#ifndef PIR_GVN_H
#define PIR_GVN_H

#include "../translations/pir_translator.h"

namespace rir {
namespace pir {

/*
 * This pass searches for dominating force instructions.
 *
 * If we identify such an instruction, and we statically know which promise is
 * being forced, then it inlines the promise code at the place of the
 * dominating force, and replaces all subsequent forces with it's result.
 *
 */
class Closure;
class GlobalValueNumbering : public PirTranslator {
  public:
    GlobalValueNumbering() : PirTranslator("global value numbering"){};

    void apply(Closure* function) override;
};
}
}

#endif
