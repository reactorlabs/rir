#ifndef CODE_VERIFIER_H
#define CODE_VERIFIER_H

#include "interpreter/interp_incl.h"

namespace rir {

/** Various verifications of the ::Code and ::Function objects.
 */
class CodeVerifier {
  public:
    /** Verifies the stack layout of the Code object and updates its ostack and
     * istack requirements.
     */
    static void calculateAndVerifyStack(Code* code);

    /** Verifies that the given function object is valid.
     */
    static void verifyFunctionLayout(SEXP sexp);
};

} // namespace rir

#endif // STACK_VERIFIER_H
