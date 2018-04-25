#ifndef IR_TRANSLATOR_H
#define IR_TRANSLATOR_H

#include "../pir/closure.h"
#include "../pir/module.h"
#include <string>

namespace rir {
namespace pir {

class RirCompiler;
struct RirInput;
typedef Closure PirInput;

union IRCode {
    PirInput* pirInput;
    RirInput* rirInput;

    PirInput* getPirInputFormat() { return pirInput; };
    RirInput* getRirInputFormat() { return rirInput; };
};

class IRTranslator {
  public:
    IRTranslator(RirCompiler& cmp, std::string name) : cmp(cmp), name(name) {}

    virtual ~IRTranslator() {}

    virtual void apply(IRCode) = 0;
    std::string getName() { return this->name; }
    RirCompiler& compiler() { return cmp; }

  protected:
    RirCompiler& cmp;
    std::string name;
};
} // namespace pir
} // namespace rir

#endif
