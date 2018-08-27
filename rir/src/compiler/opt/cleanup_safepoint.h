#ifndef PIR_CLEANUP_SAFEPOINTS_H
#define PIR_CLEANUP_SAFEPOINTS_H

#include "../translations/pir_translator.h"

namespace rir {
namespace pir {

class Closure;
class CleanupSafepoint : public PirTranslator {
  public:
    CleanupSafepoint() : PirTranslator("Cleanup Safepoint"){};

    void apply(Closure* function) const final override;
};
} // namespace pir
} // namespace rir

#endif
