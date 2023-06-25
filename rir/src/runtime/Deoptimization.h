#ifndef RIR_DEOPTIMIZATION_H
#define RIR_DEOPTIMIZATION_H

#include <R/r.h>
#include <iostream>

class ByteBuffer;

namespace rir {
#pragma pack(push)
#pragma pack(1)

enum class Opcode : uint8_t;
struct Code;

struct FrameInfo {
    Opcode* pc;
    Code* code;
    size_t stackSize;
    bool inPromise;

    void deserialize(ByteBuffer& buf);
    void serialize(ByteBuffer& buf) const;
};

struct DeoptMetadata {
    static DeoptMetadata* deserialize(ByteBuffer& buf);
    void serialize(ByteBuffer& buf) const;
    void print(std::ostream& out) const;
    size_t numFrames;
    FrameInfo frames[];
};

#pragma pack(pop)

} // namespace rir

#endif
