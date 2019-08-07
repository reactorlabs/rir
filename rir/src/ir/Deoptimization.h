#ifndef RIR_DEOPTIMIZATION_H
#define RIR_DEOPTIMIZATION_H

#include <R/r.h>
#include <iostream>

namespace rir {
#pragma pack(push)
#pragma pack(1)

enum class Opcode : uint8_t;
struct Code;

struct FrameInfo {
    Opcode* pc;
    Code* code;
    size_t stackSize;

    static FrameInfo deserialize(const Opcode* anchor, SEXP refTable,
                                 R_inpstream_t inp);
    void serialize(const Opcode* anchor, SEXP refTable,
                   R_outpstream_t out) const;
};

struct DeoptMetadata {
    void print(std::ostream& out) const;
    size_t numFrames;
    FrameInfo frames[];

    // Must be manually deallocated
    static SEXP deserialize(const Opcode* anchor, SEXP refTable,
                            R_inpstream_t inp);
    void serialize(const Opcode* anchor, SEXP refTable,
                   R_outpstream_t out) const;
};

#pragma pack(pop)
} // namespace rir

#endif
