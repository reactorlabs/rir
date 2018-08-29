#ifndef RIR_DEOPTIMIZATION_H
#define RIR_DEOPTIMIZATION_H

#include <iostream>

namespace rir {
#pragma pack(push)
#pragma pack(1)

enum class Opcode : uint8_t;
struct Code;

struct FrameInfo {
    Opcode* pc;
    Code* code;
};

struct DeoptMetadata {
    FrameInfo frames[1];

    void print(std::ostream& out) const;
};

#pragma pack(pop)
} // namespace rir

#endif
