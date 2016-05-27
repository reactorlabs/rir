#ifndef RIR_PRIMITIVES_H
#define RIR_PRIMITIVES_H

#include "RIntlns_inc.h"
#include "Runtime.h"
#include "RDefs.h"

namespace rjit {
namespace rir {

class Primitives {
  public:
    static SEXP compilePrimitive(SEXP fun) {
        return instance().cachedCompilePrimitive(fun);
    }

  private:
    std::array<bool, 1024> PrimitivesCacheOccupied;
    std::array<SEXP, 1024> PrimitivesCache;

    static Primitives& instance() {
        static Primitives singleton;
        return singleton;
    }

    inline SEXP cachedCompilePrimitive(SEXP fun) {
        if (TYPEOF(fun) == SPECIALSXP || TYPEOF(fun) == BUILTINSXP) {
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
