#ifndef RINT
#define RINT

#include <R.h>
#include <Rinternals.h>

#undef length
#undef match
#undef PrintValue
#undef bool

#include <RDefs.h>

// TODO: where would this stuff actually come from?
extern "C" {
extern SEXP R_TrueValue;
extern SEXP R_FalseValue;
extern SEXP Rf_NewEnvironment(SEXP, SEXP, SEXP);
extern Rboolean R_Visible;
extern SEXP forcePromise(SEXP);
}

#endif
