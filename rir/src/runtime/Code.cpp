#include "Code.h"
#include "R/Printing.h"
#include "ir/BC.h"
#include "utils/Pool.h"

#include <iomanip>

namespace rir {
Code::Code(SEXP ast, unsigned cs, unsigned sourceLength, unsigned offset,
           bool isDefaultArg, size_t localsCnt)
    : magic(CODE_MAGIC), header(offset),
      src(src_pool_add(globalContext(), ast)), localsCount(localsCnt),
      codeSize(cs), srcLength(sourceLength), perfCounter(0),
      isDefaultArgument(isDefaultArg) {}

void Code::disassemble(std::ostream& out) const {
    Opcode* pc = code();

    while (pc < endCode()) {
        BC bc = BC::decode(pc);

        out << std::setw(5) << ((uintptr_t)pc - (uintptr_t)code());

        unsigned s = getSrcIdxAt(pc, true);
        if (s != 0)
            out << "   ; " << dumpSexp(src_pool_at(globalContext(), s))
                << "\n       ";

        // Print call ast
        switch (bc.bc) {
        case Opcode::call_implicit_:
        case Opcode::named_call_implicit_:
        case Opcode::call_:
        case Opcode::named_call_:
            out << "   ; "
                << dumpSexp(Pool::get(bc.immediate.callFixedArgs.ast)).c_str()
                << "\n       ";
            break;
        case Opcode::static_call_:
            out << "   ; "
                << dumpSexp(Pool::get(bc.immediate.staticCallFixedArgs.ast))
                       .c_str();
            break;
        default: {}
        }

        bc.print(out);

        pc = BC::next(pc);
    }
}

void Code::print(std::ostream& out) const {
    out << "Code object (" << this << " offset " << std::hex << header
        << " (hex))"
        << "\n";
    out << "   Source: " << src << " index to src pool\n";
    out << "   Magic: " << std::hex << magic << "(hex)\n";
    out << "   Stack (o): " << stackLength << "\n";
    out << "   Code size: " << codeSize << "[B]\n";

    out << "   Default arg? " << (isDefaultArgument ? "yes\n" : "no") << "\n";
    if (magic != CODE_MAGIC) {
        out << "Wrong magic number -- corrupted IR bytecode";
        Rf_error("Wrong magic number -- corrupted IR bytecode");
    }

    out << "\n";
    disassemble(out);
}
}
