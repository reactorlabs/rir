#include "BC.h"

#include "Pool.h"
#include <iostream>
#include <iomanip>

#include "../RList.h"
#include "CodeStream.h"
#include "RIntlns.h"
#include "CodeHandle.h"

namespace rjit {
namespace rir {

void CodeHandle::print() {
    BC_t* pc = (BC_t*)bc();

    unsigned * s = src(code);
    while ((uintptr_t)pc < (uintptr_t)endBc()) {
        if (*s != 0) {
            Rprintf("          # (idx %u) : ", *s);
            Rf_PrintValue(src_pool_at(globalContext(), *s));
        }
        Rprintf(" %5x ", ((uintptr_t)pc - (uintptr_t)bc()));
        BC bc = BC::advance(&pc);
        bc.print();
        ++s;
    }
}

}
}
