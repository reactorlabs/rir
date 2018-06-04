#ifndef PIR_GVN_H
#define PIR_GVN_H

#include "../translations/pir_translator.h"

namespace rir {
namespace pir {

/*
 * This pass searches for global redundancies in a piece of code.
 * If we identify redundancies we can remove those operations.
 */
class Closure;
class GlobalValueNumbering : public PirTranslator {
  public:
    GlobalValueNumbering() : PirTranslator("global value numbering"){};

    void apply(Closure* function) override;
};
}
}

#endif
