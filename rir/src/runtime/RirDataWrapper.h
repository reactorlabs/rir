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

struct data_header {
    // magic number to differentiate RIR objects
    // needs to be in sync with gnur context.c
    uint32_t magic;
};

template <typename BASE, uint32_t MAGIC>
struct RirDataWrapper {
    data_header info;

    static BASE* check(SEXP s) {
        BASE* b = (BASE*)s;
        return b->info.magic == MAGIC ? b : nullptr;
    }

    static BASE* unpack(void* s) {
        BASE* b = (BASE*)s;
        assert(
            b->info.magic == MAGIC &&
            "Trying to unpack the wrong type of embedded RIR runtime object.");
        return b;
    }

  protected:
    explicit RirDataWrapper(uint32_t count) : info{MAGIC} {
        uint8_t* start = (uint8_t*)this + sizeof(uint32_t);
        memset(start, 0, count * sizeof(void*));
    }
};

} // namespace rir

#endif
