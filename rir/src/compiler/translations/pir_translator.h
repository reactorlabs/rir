#ifndef PIR_TRANSLATOR_H
#define PIR_TRANSLATOR_H

#include "../pir/module.h"
#include "../pir/value.h"
#include "runtime/Function.h"

namespace rir {
namespace pir {

class PirTranslator {
  public:
    PirTranslator(bool verbose) : verbose(verbose) {}

    bool isVerbose();
    void setVerbose(bool);
  private:
      bool verbose;
};
}
}

#endif
