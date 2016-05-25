#ifndef RIR_PRIMITIVES_H
#define RIR_PRIMITIVES_H

#include "RDefs.h"
#include "Runtime.h"

namespace rjit {
namespace rir {

class Primitives {
  public:
    static BCClosure* compilePrimitive(SEXP fun) {
        return instance().cachedCompilePrimitive(fun);
    }

  private:
    std::array<bool, 1024> PrimitivesCacheOccupied;
    std::array<BCClosure*, 1024> PrimitivesCache;

    static Primitives& instance() {
        static Primitives singleton;
        return singleton;
    }

    inline BCClosure* cachedCompilePrimitive(SEXP fun) {
        if (TYPEOF(fun) == SPECIALSXP || TYPEOF(fun) == BUILTINSXP) {
            int idx = fun->u.primsxp.offset;
            if (PrimitivesCacheOccupied[idx])
                return PrimitivesCache[idx];
        }

        return doCompilePrimitive(fun);
    }

    BCClosure* doCompilePrimitive(SEXP fun);
};

} // rir
} // rjit

#endif
