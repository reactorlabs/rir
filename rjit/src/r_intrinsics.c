#include <R.h>
#include <Rinternals.h>

SEXP __jit__cdr(SEXP body) {
    return CDR(body);
}

SEXP __jit__vectorElt(SEXP e, int i) {
    return VECTOR_ELT(e, i);
}
