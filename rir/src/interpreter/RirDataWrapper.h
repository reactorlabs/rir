#ifndef RIR_DATA_WRAPPER_H
#define RIR_DATA_WRAPPER_H

/** RirDataWrappers are wrappers (usually over SEXP) of relevant
 * RIR level information that must be shared to the gnu-r
 * interpreter. For instance for the interpreter to lazily
 * create stuff depending on RIR information
 */

#include "R/r.h"
#include <cassert>
#include <cstdint>

namespace rir {

#define RIR_DATA_WRAPPER_MAGIC 0xda7a0403

struct data_header {
    // magic number to differentiate RIR objects needs sync with gnur context.c
    const uint32_t magic;

    // magic number to differentiate different RIR data wrappers
    const uint32_t wrapper;

    // #SEXPs stored in an vector* at the begging of the struct to be preserved
    const uint32_t gc;
};

template <typename BASE, uint32_t MAGIC>
struct RirDataWrapper {
    data_header info;

    static BASE* check(void* s) {
        BASE* b = (BASE*)s;
        return b->info.wrapper == MAGIC ? b : nullptr;
    }

    static BASE* unpack(void* s) {
        BASE* b = (BASE*)s;
        assert(
            b->info.wrapper == MAGIC &&
            "Trying to unpack the wrong type of embedded RIR runtime object.");
        return b;
    }

  protected:
    explicit RirDataWrapper(uint32_t count, uint32_t gc)
        : info{RIR_DATA_WRAPPER_MAGIC, MAGIC, gc} {
        uint8_t* start = (uint8_t*)this + sizeof(data_header);
        memset(start, 0, count * sizeof(void*));
    }
};

} // namespace rir

#endif
