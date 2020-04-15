#ifndef PIR_TRANSLATOR_H
#define PIR_TRANSLATOR_H

#include "../debugging/stream_logger.h"
#include "../pir/module.h"
#include <string>

namespace rir {
namespace pir {

class RirCompiler;
class PirTranslator {
  public:
    explicit PirTranslator(const std::string& name) : name(name) {}

    virtual bool apply(RirCompiler&, ClosureVersion* function,
                       LogStream&) const = 0;
    std::string getName() const { return this->name; }
    virtual ~PirTranslator() {}

    virtual bool isPhaseMarker() const { return false; }

    virtual unsigned cost() const { return 1; }

  protected:
    std::string name;
};
}
}

#endif
