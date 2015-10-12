#include "StackScan.h"
#include "StackMap.h"

#include <execinfo.h>
#include <iostream>

#include "RIntlns.h"

#include <dlfcn.h>
#include <execinfo.h>
#include <stdlib.h>
#include <unwind.h>

extern void* __libc_stack_end;

namespace rjit {

// FIXME: we need robust stack scanning with markers on stack instead of this
//        hack
void StackScan::stackScanner(void (*forward_node)(SEXP)) {

    struct layout {
        struct layout* bp;
        void* ret;
    };

    void* _bp = __builtin_frame_address(0);

    struct layout* bp = (struct layout*)_bp;

    unsigned num = 0;

    while (true) {
        if ((void*)bp < _bp ||
#ifndef __APPLE__
            (void*)bp > __libc_stack_end ||
#endif
            ((long)bp & 3))
            break;

        uintptr_t pos = (uintptr_t)bp->ret;

        if (StackMap::isStatepoint(pos)) {
            uintptr_t frame = (uintptr_t)(bp + 1);

            const auto& R = StackMap::getStatepoint(pos);
            for (const auto& Loc : R.locations()) {
                if (Loc.getKind() == StackMapParserT::LocationKind::Direct) {
                    // Statepoint args should be spilled =>
                    // reg is == 7 (rsp)
                    assert(Loc.getDwarfRegNum() == 7);

                    uintptr_t value = frame + Loc.getOffset();

                    assert(!value || *(int*)value);

                    forward_node(*(SEXP*)value);
                }
            }
        }
        num++;
        bp = bp->bp;
    }
}
}
