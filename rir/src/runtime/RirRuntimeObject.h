#ifndef RIR_RUNTIME_OBJECT_H
#define RIR_RUNTIME_OBJECT_H

/** Superclass for all RIR objects embedded inside an EXTERNALSXP R object.
 *  This is used to expose SEXPs in RIR objects to R's garbage collector.
 *
 *  RIR objects that want to expose some of their internal SEXPs to the
 *  GC to trace need to place those SEXPs consecutively one after another.
 *
 *  gc_area_start is the offset in bytes to the first exposed SEXP,
 *  relative to the start of the RIR object (i.e. INTEGER(obj)).
 *
 *  gc_area_length is the number of exposed SEXPs.
 */

#include "R/r.h"
#include <cassert>
#include <cstdint>

namespace rir {

struct rir_header {
    // offset, in bytes, from the beginning of this object, to the first SEXP
    uint32_t gc_area_start;

    // number of SEXPs for the GC to trace
    uint32_t gc_area_length;

    // magic number to differentiate RIR objects
    uint32_t magic;

    // TODO:  Later maybe also add type of the object here and have just
    //        one EXTERNALSXP with a union of other types.
    // For now just make sure that this header is in all RIR
};

template <typename BASE, uint32_t MAGIC>
struct RirRuntimeObject {
    rir_header info;

    SEXP container() const {
        // cppcheck-suppress thisSubtraction
        SEXP result = (SEXP)((uintptr_t)this - sizeof(VECTOR_SEXPREC));
        assert(TYPEOF(result) == EXTERNALSXP &&
               "RIR object not embedded in container, or corrupt.");
        return result;
    }

    static BASE* unpack(SEXP s) {
        BASE* b = (BASE*)INTEGER(s);
        assert(
            b->info.magic == MAGIC &&
            "Trying to unpack the wrong type of embedded RIR runtime object.");
        return b;
    }

    static BASE* check(SEXP s) {
        if (TYPEOF(s) != EXTERNALSXP) {
            return nullptr;
        }
        BASE* b = (BASE*)INTEGER(s);
        return b->info.magic == MAGIC ? b : nullptr;
    }

  protected:
    void setEntry(size_t pos, SEXP v) {
        EXTERNALSXP_SET_ENTRY(this->container(), pos, v);
    }

    SEXP getEntry(size_t pos) const {
        return EXTERNALSXP_ENTRY(this->container(), pos);
    }

    RirRuntimeObject(uint32_t gc_area_start, uint32_t gc_area_length)
        : info{gc_area_start, gc_area_length, MAGIC} {
        uint8_t* start = (uint8_t*)this + gc_area_start;
        memset(start, 0, gc_area_length * sizeof(SEXP));
    }
};

} // namespace rir

#endif
