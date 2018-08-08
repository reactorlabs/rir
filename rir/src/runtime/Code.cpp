#include "Code.h"
#include "R/Printing.h"
#include "ir/BC.h"
#include "utils/Pool.h"

namespace rir {
Code::Code(SEXP ast, unsigned cs, unsigned sourceLength, unsigned offset,
           bool isDefaultArg, size_t localsCnt)
    : magic(CODE_MAGIC), header(offset),
      src(src_pool_add(globalContext(), ast)), localsCount(localsCnt),
      codeSize(cs), srcLength(sourceLength), perfCounter(0),
      isDefaultArgument(isDefaultArg) {}

void Code::disassemble() {
    Opcode* pc = code();

    while (pc < endCode()) {
        BC bc = BC::decode(pc);

        Rprintf(" %5d ", ((uintptr_t)pc - (uintptr_t)code()));

        unsigned s = getSrcIdxAt(pc, true);
        if (s != 0)
            Rprintf("   ; %s\n       ",
                    dumpSexp(src_pool_at(globalContext(), s)).c_str());

        // Print call ast
        switch (bc.bc) {
        case Opcode::call_implicit_:
        case Opcode::named_call_implicit_:
        case Opcode::call_:
        case Opcode::named_call_:
            Rprintf(
                "   ; %s\n       ",
                dumpSexp(Pool::get(bc.immediate.callFixedArgs.ast)).c_str());
            break;
        case Opcode::static_call_:
            Rprintf("   ; %s\n       ",
                    dumpSexp(Pool::get(bc.immediate.staticCallFixedArgs.ast))
                        .c_str());
            break;
        default: {}
        }

        bc.print();

        pc = BC::next(pc);
    }
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

    Rprintf("\n");
    disassemble();
}
}
