#ifndef PIR_CLEANUP_H
#define PIR_CLEANUP_H

#include "../translations/rir_compiler.h"

namespace rir {
namespace pir {

class Closure;
class Cleanup : public PirTranslator {
  public:
    Cleanup()
        : PirTranslator("cleanup") {};

  protected:
    void applyTranslation(Closure* function);
};
}
}

#endif
