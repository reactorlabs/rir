#ifndef RIR_ARGS_LAZY_H
#define RIR_ARGS_LAZY_H

#include "RirDataWrapper.h"
#include <cassert>
#include <cstdint>
#include <functional>

struct CallContext;
struct Context;
SEXP createLegacyArgsListFromStackValues(const CallContext& call,
                                         bool eagerCallee, Context* ctx);

namespace rir {

typedef std::function<SEXP(void*)> LazyFunction;

#define LAZY_ARGS_MAGIC 0x1a27a000

/**
 * ArgsLazyCreation holds the information needed to recreate the
 * arguments list needed by gnu-r contexts whenever a function
 * is called, lazily. In RCNTXT the field that hold the list is
 * promargs.
 */

struct ArgsLazyData : public RirDataWrapper<ArgsLazyData, LAZY_ARGS_MAGIC> {
    ArgsLazyData() = delete;
    ArgsLazyData(const CallContext* callCtx, Context* cmpCtx)
        : RirDataWrapper(2), callContext(callCtx), compilationContext(cmpCtx){};

    const CallContext* callContext;
    Context* compilationContext;

    SEXP createArgsLists() {
        return createLegacyArgsListFromStackValues(*callContext, false,
                                                   compilationContext);
    }
};
} // namespace rir

#endif
