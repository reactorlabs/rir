#ifndef DISPATCHER_H
#define DISPATCHER_H

#include "wrappers.h"
#include "intrinsics.h"


namespace rjit {
    class Handler {
        /*@handler, how to make the handlers */
        void genericGetVar(genericGetVar ins) {
            // conditions are expressed as assertions, they are picked by the dispatcher script
            assert(ins.symbol() == nullptr and "Whatever here");
        }

    };



} // namespace rjit


#endif // DISPATCHER_H

