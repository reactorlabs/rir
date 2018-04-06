#ifndef PIR_TRANSLATOR_H
#define PIR_TRANSLATOR_H

#include "../pir/module.h"
#include <string>

namespace rir {
namespace pir {

class RirCompiler;
class PirTranslator {
  public:
    PirTranslator(std::string name)
        : name(name) {}

    void apply(Closure* function);
    std::string& getName() { return this->name; }
    virtual ~PirTranslator() {}

  protected:
    virtual void applyTranslation(Closure* function) = 0;
    std::string name;
};
}
}

#endif
