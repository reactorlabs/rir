#ifndef RIR_HEADER_H
#define RIR_HEADER_H

#include <cstdint>

/** Header for all RIR objects embedded inside an EXTERNALSXP R object.
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

namespace rir {

struct rir_header {
    uint32_t gc_area_start;  /// First SEXP to be marked by the GC
    uint32_t gc_area_length;  /// Number of SEXPs to expose to the GC
    uint32_t magic;  /// Magic number to tell different RIR objects apart

    // TODO:  Later maybe also add type of the object here and have just
    //        one EXTERNALSXP with a union of other types.
    // For now just make sure that this header is in all RIR
};

}

#endif
