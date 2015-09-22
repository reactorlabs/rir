#ifndef RDEF
#define RDEF

struct SEXPREC;
typedef SEXPREC* SEXP;

extern "C" SEXP Rf_install(const char*);

#endif
