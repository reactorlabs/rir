

#include "Code.h"

#include "interpreter.h"

namespace rjit {
namespace rir {


/** Serializes the rir::Code object into a Function.
 */

// TODO Bytecodes and indices should be unified

SEXP Code::toFunction() {
    // calculate the overall size required (in bytes)
    unsigned size = 0;

    // create the vector
    assert(size % sizeof (int) == 0 and "Are code objects properly padded?");
    SEXP result = Rf_allocVector(INTSXP, size / sizeof (int));
    PROTECT(result);
    OpcodeT * code = reinterpret_cast<OpcodeT*>(INTEGER(result));

    // fill in the header
    ::Function * fHdr = reinterpret_cast<::Function *>(code);
    fHdr->magic = 0xCAFEBABE;
    fHdr->origin = nullptr; // this will be ptr to unoptimized version, but we do not optimize yet
    fHdr->size = size;
    fHdr->codeLength = children.size() + 1;

    // now serialize the codes, one by one, first itself, then all the children
    // keep in mind to update the instructions which deal push promise indices to update them so that they push offsets from the beginning

    UNPROTECT(1);
    return nullptr;
}


} // namespace rir
} // namespace rjit
