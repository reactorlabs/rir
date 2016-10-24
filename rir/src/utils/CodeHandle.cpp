#include "ir/BC.h"

#include "Pool.h"
#include <iostream>
#include <iomanip>

#include "R/RList.h"
#include "ir/CodeStream.h"
#include "R/r.h"
#include "CodeHandle.h"
#include "FunctionHandle.h"
#include "ir/CodeEditor.h"

namespace rir {

void CodeHandle::print() {
    BC_t* pc = (BC_t*)bc();

    while ((uintptr_t)pc < (uintptr_t)endBc()) {
        unsigned s = getSrcIdxAt(code, (OpcodeT*)pc, true);
        if (s != 0) {
            Rprintf("          # (idx %u) : ", s);
            Rf_PrintValue(src_pool_at(globalContext(), s));
        }
        Rprintf(" %5d ", ((uintptr_t)pc - (uintptr_t)bc()));
        BC bc = BC::advance(&pc);
        bc.print_(&code->callSites[bc.immediate.call_id]);
    }
}

FunctionHandle CodeHandle::function() {
    return (SEXP)((uintptr_t)::function(code) - FUNCTION_OFFSET);
}

fun_idx_t CodeHandle::idx() {
        fun_idx_t i = 0;
        for (auto c : function()) {
            if (c == code)
                return i;
            ++i;
        }
        assert(false);
        return -1;
}

}

C_OR_CPP void c_printCode(Code * c) {
    rir::CodeHandle(c).print();
}

