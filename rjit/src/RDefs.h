#ifndef RDEF
#define RDEF

#include <cstddef>

struct SEXPREC;
typedef SEXPREC* SEXP;

typedef unsigned int SEXPTYPE;

extern "C" {
SEXP Rf_install(const char*);

const char* Rf_type2char(SEXPTYPE);
}

#endif
