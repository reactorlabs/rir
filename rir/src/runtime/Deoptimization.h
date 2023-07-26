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
    void internRecursive() const;
    /// Adds the code object's container to the code's extra pool, so it gets
    /// gc-collected when the SEXP does
    void gcAttach(Code* outer) const;
};

struct DeoptMetadata {
    SEXP container() const;
    static DeoptMetadata* deserialize(ByteBuffer& buf);
    void serialize(ByteBuffer& buf) const;
    void internRecursive() const;
    /// Adds the container and the frame code objects' containers to the code's
    /// extra pool, so it gets gc-collected when the SEXP does
    void gcAttach(Code* outer) const;
    void print(std::ostream& out) const;
    size_t numFrames;
    FrameInfo frames[];
};

#pragma pack(pop)

} // namespace rir

#endif
