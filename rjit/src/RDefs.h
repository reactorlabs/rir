#ifndef RDEF
#define RDEF

/*
 * Forward declaration of some commonly used R functions.
 *
 * This file is safe to be included in header files
 *
 */

#include <cstddef>

struct SEXPREC;
typedef SEXPREC* SEXP;

typedef unsigned int SEXPTYPE;

extern "C" {
extern SEXP R_NilValue;

SEXP Rf_install(const char*);

const char* Rf_type2char(SEXPTYPE);
int TYPEOF(SEXP);

void Rf_unprotect(int);
SEXP Rf_protect(SEXP);

SEXP TAG(SEXP);
SEXP CAR(SEXP);
SEXP CDR(SEXP);
}

#define SPECIALSXP 7 /* special forms */
#define BUILTINSXP 8 /* builtin non-special forms */

#endif
