#ifndef PIR_TRANSLATOR_H
#define PIR_TRANSLATOR_H

#include "../pir/module.h"
#include <string>

namespace rir {
namespace pir {

class RirCompiler;
class PirTranslator {
  public:
    PirTranslator(std::string name) : name(name) {}

    virtual void apply(Closure* function) = 0;
    std::string getName() { return this->name; }
    virtual ~PirTranslator() {}

  protected:
    std::string name;
};
}
}

#endif
