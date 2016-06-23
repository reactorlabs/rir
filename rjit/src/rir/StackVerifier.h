#ifndef STACK_VERIFIER_H
#define STACK_VERIFIER_H

#include "interp.h"

namespace rjit {
namespace rir {

class StackVerifier {
public:
    static void verify(::Code * code);
};

} // namespace rir
} // namespace rjit





#endif // STACK_VERIFIER_H
