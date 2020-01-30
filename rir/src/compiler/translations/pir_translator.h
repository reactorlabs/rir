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

    virtual void apply(RirCompiler&, ClosureVersion* function,
                       ClosureStreamLogger&) const = 0;
    std::string getName() const { return this->name; }
    virtual ~PirTranslator() {}

    virtual bool isPhaseMarker() const { return false; }

  protected:
    std::string name;
};
}
}

#endif
