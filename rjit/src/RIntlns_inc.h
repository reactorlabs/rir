#ifndef RINT_INC_H
#define RINT_INC_H

/*
 * this file contains our own wrapper around USE_RINTERNAL
 * to add a feature extend the class below and add the feature to RIntlns.cpp
 * this file should be safe to include in headers
 */

struct SEXPREC;
typedef SEXPREC* SEXP;

namespace rjit {
class Rinternals {
  public:
    static int primoffset(SEXP);
};
}

#endif
