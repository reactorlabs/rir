#ifndef RIR_DEOPTIMIZATION_H
#define RIR_DEOPTIMIZATION_H

#include <R/r.h>
#include <iostream>

#include "loweringPatches.h"

namespace rir {
#pragma pack(push)
#pragma pack(1)

enum class Opcode : uint8_t;
struct Code;

struct FrameInfo {
    #if TRY_PATCH_DEOPTMETADATA == 1
    Opcode* pc;
    uintptr_t offset;
    Code* code;
    char hast[1000];
    int index;
    size_t stackSize;
    bool inPromise;

    FrameInfo() {}
    FrameInfo(uintptr_t offset, const char * hast1, int index, size_t stackSize, bool promise)
        : offset(offset), hast(""), index(index), stackSize(stackSize), inPromise(promise) {
            strcpy(hast, hast1);
            code = 0;
            pc = 0;
        }
    FrameInfo(Opcode* pc, Code* code, size_t stackSize, bool promise)
        : pc(pc), offset(0), code(code), hast(""), index(0), stackSize(stackSize), inPromise(promise) {}
    #else
    Opcode* pc;
    Code* code;
    size_t stackSize;
    bool inPromise;

    FrameInfo() {}
    FrameInfo(Opcode* pc, Code* code, size_t stackSize, bool promise)
        : pc(pc), code(code), stackSize(stackSize), inPromise(promise) {}
    #endif
};

struct DeoptMetadata {
    void print(std::ostream& out) const;
    size_t numFrames;
    FrameInfo frames[];
};

#pragma pack(pop)
} // namespace rir

#endif
