#ifndef RIR_DATA_WRAPPER_H
#define RIR_DATA_WRAPPER_H

/*
 * RirDataWrappers are wrappers (usually over SEXP) of relevant RIR level
 * information that must be shared to the gnu-r interpreter. For instance for
 * the interpreter to lazily create stuff depending on RIR information. Every
 * wrapper needs to store how many objects that need to be traced by the gc it
 * stores. Then each subclass, must implement a function `gcData` that anwers a
 * pointer to an array of the corresponding size so that gnu-r can walk through
 * all the data.
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

/*
 */

template <typename BASE, uint32_t MAGIC>
struct RirDataWrapper {
    data_header info;

    /*
     * At this point we are only wrapping SEXPs.
     * So, the parameter must be a SEXP or a RirDataWrapper.
     */
    static BASE* cast(void* s) {
        BASE* b = reinterpret_cast<BASE*>(s);
        if (b->info.wrapper == MAGIC)
            return reinterpret_cast<BASE*>(s);
        return nullptr;
    }

  protected:
    explicit RirDataWrapper(uint32_t gc)
        : info{RIR_DATA_WRAPPER_MAGIC, MAGIC, gc} {}
};

} // namespace rir

#endif
