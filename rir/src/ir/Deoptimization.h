#ifndef RIR_DEOPTIMIZATION_H
#define RIR_DEOPTIMIZATION_H

#include <iostream>

namespace rir {
#pragma pack(push)
#pragma pack(1)

struct FrameInfo {
    void* pc;
    void* code;
};

struct DeoptMetadata {
    FrameInfo frames[1];

    void print(std::ostream& out) const {
        for (auto f : frames)
            out << f.code << "@" << f.pc << " ";
    }
};

#pragma pack(pop)
} // namespace rir

#endif
