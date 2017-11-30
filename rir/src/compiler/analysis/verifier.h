#ifndef COMPILER_PIR_VERIFIER_H
#define COMPILER_PIR_VERIFIER_H

#include "../pir/pir.h"

namespace rir {
namespace pir {

class Verifier {
    Function* f;

  public:
    Verifier(Function* f) : f(f) {}
    void operator()();
    bool verify(BB*);
    bool verify(Instruction*, BB* bb);
    bool verify(Promise*);
};
}
}

#endif
