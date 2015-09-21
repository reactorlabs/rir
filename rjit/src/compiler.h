#ifndef COMPILER_H
#define COMPILER_H

#include "R.h"
#include "rint.h"


namespace rjit {

    /** NATIVESXP type

      Has the 3-SEXP style of a pairlist, where:
        TAG is Module * so that we can reconstruct the llvm code if we choose to.
        CAR is pointer to the function's executable
        CDR is pointer to a list where first element is the original code of the expression, followed by arbitrary number of other SEXPs created during the compilation for easy garbage collection.
     */

}


#endif // COMPILER_H

