#ifndef PIR_INLINE_H
#define PIR_INLINE_H

#include "../translations/pir_translator.h"

namespace rir {
namespace pir {

/*
 * Inlines a closure.
 *
 * This pass is intentionally stupid. It does not resolve inner environments,
 * but rather just copies instructions and leads to functions with multiple
 * environments.
 *
 * Later scope resolution and force dominance passes will do the smart parts.
 *
 */
class Closure;
class Inline : public PirTranslator {
  public:
    Inline() : PirTranslator("Inline"){};
    void apply(Closure* function) const final override;
};
}
}

#endif
