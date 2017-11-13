#include "Code.h"
#include "utils/Pool.h"
#include "ir/CodeEditor.h"
#include "ir/BC.h"

namespace rir {
Code::Code(SEXP ast, unsigned cs, unsigned sourceSize, unsigned csl,
           unsigned offset, bool isDefaultArg) {
    magic = CODE_MAGIC;
    header = offset;
    src = src_pool_add(globalContext(), ast);
    codeSize = cs;
    srcLength = sourceSize;
    skiplistLength = calcSkiplistLength(sourceSize);
    callSiteLength = csl;
    perfCounter = 0;
    isDefaultArgument = isDefaultArg;
}

void Code::print() {
    Rprintf("Code object (%p offset %x (hex))\n", this, header);
    Rprintf("  Source:      %u (index to src pool)\n", src);
    Rprintf("  Magic:       %x (hex)\n", magic);
    Rprintf("  Stack (o):   %u\n", stackLength);
    Rprintf("  Code size:   %u [B]\n", codeSize);
    Rprintf("  Default arg? %s\n", isDefaultArgument ? "yes" : "no");
    if (magic != CODE_MAGIC)
        Rf_error("Wrong magic number -- corrupted IR bytecode");

    Rprintf("\n  Skiplist:  %u \n", skiplistLength);
    unsigned* sl = skiplist();
    for (unsigned i = 0; i < skiplistLength; ++i) {
        if (*(sl + 1) != (unsigned)-1)
            Rprintf("    pc: %u -> src_idx: %u\n", *sl, *(sl + 1));
        sl += 2;
    }
    Rprintf("\n");
    Opcode* pc = code();

    while (pc < endCode()) {
        unsigned s = getSrcIdxAt(pc, true);
        if (s != 0) {
            Rprintf("          # (idx %u) : ", s);
            Rf_PrintValue(src_pool_at(globalContext(), s));
        }
        Rprintf(" %5d ", ((uintptr_t)pc - (uintptr_t)code()));
        BC bc = BC::advance(&pc);
        if (bc.isCallsite()) {
            CallSite* cs = bc.callSite(this);
            bc.print(cs);
        } else {
            bc.print();
        }
    }
}
}
