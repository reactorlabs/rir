#ifndef RIR_OPCODE_H
#define RIR_OPCODE_H

#include "R/r.h"
#include "ir/BC_inc.h"

#include <cstdint>
#include <cassert>

// TODO force inlining for clang & gcc
#define INLINE __attribute__((always_inline)) inline static

#ifdef ENABLE_SLOWASSERT
#define SLOWASSERT(what) assert(what)
#else
#define SLOWASSERT(what)                                                       \
    {}
#endif

/** Moves the pc to next instruction, based on the current instruction length
 */
INLINE rir::Opcode* advancePc(rir::Opcode* pc) {
    switch (*pc++) {
#define DEF_INSTR(name, imm, ...)                                              \
    case rir::Opcode::name:                                                    \
        pc += sizeof(ArgT) * imm;                                              \
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
