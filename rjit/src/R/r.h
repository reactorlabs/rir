#ifndef RIR_R_H
#define RIR_R_H

#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>

#undef TRUE
#undef FALSE
#undef length

extern SEXP R_TrueValue;
extern SEXP R_FalseValue;


#endif
