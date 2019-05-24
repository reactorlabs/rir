#ifndef RIR_ARGS_LAZY_H
#define RIR_ARGS_LAZY_H

#include "RirDataWrapper.h"

#include "interp_incl.h"

#include <cassert>
#include <cstdint>
#include <functional>

namespace rir {

static constexpr size_t LAZY_ARGS_MAGIC = 0x1a27a000;

/**
 * ArgsLazyCreation holds the information needed to recreate the
 * arguments list needed by gnu-r contexts whenever a function
 * is called, lazily. In RCNTXT the field that hold the list is
 * promargs.
 */

struct ArgsLazyData : public RirDataWrapper<ArgsLazyData, LAZY_ARGS_MAGIC> {
    ArgsLazyData() = delete;
    ArgsLazyData(const ArgsLazyData&) = delete;
    ArgsLazyData& operator=(const ArgsLazyData&) = delete;

    ArgsLazyData(CallContext* callCtx, InterpreterInstance* cmpCtx)
        : RirDataWrapper(0), callContext(callCtx), compilationContext(cmpCtx){};

    CallContext* callContext;
    InterpreterInstance* compilationContext;

    SEXP createArgsLists() {
        return createLegacyArgsListFromStackValues(*callContext, false,
                                                   compilationContext);
    }
};
} // namespace rir

#endif
