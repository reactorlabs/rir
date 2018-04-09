#ifndef PIR_CLEANUP_H
#define PIR_CLEANUP_H

#include "../translations/pir_translator.h"

namespace rir {
namespace pir {

class Closure;
class Cleanup : public PirTranslator {
  public:
    Cleanup()
        : PirTranslator("cleanup") {};

    void apply(Closure* function) override;
};
}
}

#endif
