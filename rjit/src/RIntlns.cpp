#include "RIntlns_inc.h"

#define USE_RINTERNALS 1
#include <Rinternals.h>
#undef USE_RINTERNALS

namespace rjit {

int Rinternals::primoffset(SEXP s) { return s->u.primsxp.offset; }
}
