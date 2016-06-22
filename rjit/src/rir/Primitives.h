#ifndef RIR_PRIMITIVES_H
#define RIR_PRIMITIVES_H

#include "RDefs.h"
#include "RIntlns_inc.h"
#include "Runtime.h"

namespace rjit {
namespace rir {

class Primitives {
  public:
    static SEXP compilePrimitive(SEXP fun) {
        return instance().cachedCompilePrimitive(fun);
    }

  private:
    bool PrimitivesCacheOccupied[1024];
    SEXP PrimitivesCache[1024];

    static Primitives& instance() {
        static Primitives singleton;
        return singleton;
    }

    inline SEXP cachedCompilePrimitive(SEXP fun) {
        if (Rinternals::typeof(fun) == SPECIALSXP ||
            Rinternals::typeof(fun) == BUILTINSXP) {
            int idx = Rinternals::primoffset(fun);
            if (PrimitivesCacheOccupied[idx])
                return PrimitivesCache[idx];
        }

        return doCompilePrimitive(fun);
    }

    SEXP doCompilePrimitive(SEXP fun);
};

} // rir
} // rjit

#endif
