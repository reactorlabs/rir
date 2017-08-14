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
    Opcode* pc = (Opcode*)bc();

    while ((uintptr_t)pc < (uintptr_t)endBc()) {
        unsigned s = getSrcIdxAt(code, (OpcodeT*)pc, true);
        if (s != 0) {
            Rprintf("          # (idx %u) : ", s);
            Rf_PrintValue(src_pool_at(globalContext(), s));
        }
        Rprintf(" %5d ", ((uintptr_t)pc - (uintptr_t)bc()));
        BC bc = BC::advance(&pc);
        if (bc.isCallsite()) {
            CallSite cs = bc.callSite(code);
            bc.print(cs);
        } else {
            bc.print();
        }
    }
}

FunctionHandle CodeHandle::function() {
    return ::function2store(::code2function(code));
}

FunIdxT CodeHandle::idx() {
    FunIdxT i = 0;
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

