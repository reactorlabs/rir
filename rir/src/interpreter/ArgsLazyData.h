#ifndef RIR_ARGS_LAZY_H
#define RIR_ARGS_LAZY_H

#include "runtime/RirRuntimeObject.h"

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

struct ArgsLazyDataContent
    : public RirRuntimeObject<ArgsLazyDataContent, LAZY_ARGS_MAGIC> {
    ArgsLazyDataContent() = delete;
    ArgsLazyDataContent(const ArgsLazyDataContent&) = delete;
    ArgsLazyDataContent& operator=(const ArgsLazyDataContent&) = delete;

    ArgsLazyDataContent(size_t nargs, size_t nargsOrig, const R_bcstack_t* args,
                        const Immediate* argOrderOrig, const Immediate* names,
                        SEXP formals, bool staticCall, bool eagerCallee,
                        InterpreterInstance* cmpCtx)
        : RirRuntimeObject(sizeof(ArgsLazyDataContent), 0), nargs(nargs),
          nargsOrig(nargsOrig), args(args), argOrderOrig(argOrderOrig),
          names(names), formals(formals), staticCall(staticCall),
          eagerCallee(eagerCallee), compilationContext(cmpCtx) {}

    size_t nargs;
    size_t nargsOrig;
    const R_bcstack_t* args;
    const Immediate* argOrderOrig;
    const Immediate* names;
    SEXP formals;
    bool staticCall;
    bool eagerCallee;
    InterpreterInstance* compilationContext;

    SEXP createArgsList() {
        return createLegacyArgsListFromStackValues(
            nargs, nargsOrig, args, argOrderOrig, names, formals, staticCall,
            eagerCallee, compilationContext);
    }
};

struct ArgsLazyData {
    // This needs to come first and provides a SEXPREC header to not confuse
    // the R garbage collector.
    VECTOR_SEXPREC fakeSEXP;
    ArgsLazyDataContent content;

    ArgsLazyData() = delete;
    ArgsLazyData(const ArgsLazyData&) = delete;
    ArgsLazyData& operator=(const ArgsLazyData&) = delete;

    ArgsLazyData(size_t nargs, size_t nargsOrig, const R_bcstack_t* args,
                 const Immediate* argOrderOrig, const Immediate* names,
                 SEXP formals, bool staticCall, bool eagerCallee,
                 InterpreterInstance* cmpCtx)
        : content(nargs, nargsOrig, args, argOrderOrig, names, formals,
                  staticCall, eagerCallee, cmpCtx) {
        fakeSEXP.attrib = R_NilValue;
        fakeSEXP.gengc_next_node = R_NilValue;
        fakeSEXP.gengc_prev_node = R_NilValue;
        fakeSEXP.sxpinfo.gcgen = 1;
        fakeSEXP.sxpinfo.mark = 1;
        fakeSEXP.sxpinfo.named = 2;
        fakeSEXP.sxpinfo.type = EXTERNALSXP;
    }
};

} // namespace rir

#endif
