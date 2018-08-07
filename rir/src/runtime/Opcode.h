#ifndef RIR_OPCODE_H
#define RIR_OPCODE_H

#include "R/r.h"
#include "common.h"
#include "ir/BC_inc.h"

#include <cstdint>
#include <cassert>

/** Moves the pc to next instruction, based on the current instruction length
 */
RIR_INLINE rir::Opcode* advancePc(rir::Opcode* pc) {
    if (*pc == rir::Opcode::call_implicit_) {
        pc += 1 + sizeof(Immediate);
        Immediate nargs = *((Immediate*)pc);
        pc += (1 + nargs) * sizeof(Immediate);
        return pc;
    }
    switch (*pc++) {
#define DEF_INSTR(name, imm, ...)                                              \
    case rir::Opcode::name:                                                    \
        pc += sizeof(Immediate) * imm;                                         \
        break;
#include "ir/insns.h"
    default:
        assert(false && "Unknown instruction");
    }
    return pc;
}

static unsigned pad4(unsigned sizeInBytes) {
    unsigned x = sizeInBytes % 4;
    return (x != 0) ? (sizeInBytes + 4 - x) : sizeInBytes;
}

#endif
