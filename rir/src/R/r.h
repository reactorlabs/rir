#ifndef RIR_R_H
#define RIR_R_H

#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>

// r print statement
#include <R_ext/Print.h>

#undef TRUE
#undef FALSE
#undef length
#undef eval

extern SEXP R_TrueValue;
extern SEXP R_FalseValue;
extern SEXP R_LogicalNAValue;


#endif
