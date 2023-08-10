//
// Created by Jakob Hain on 8/9/23.
//

#pragma once

#include "R/r_incl.h"
#include "utils/ByteBuffer.h"
#include <queue>

namespace rir {

/* class Deserializer {
    struct Elem {
        SEXP sexp;
        bool isAst;
    };
    using Worklist = std::queue<Elem>;

    /// Underlying byte-buffer which we read data to
    ByteBuffer& buffer;
    /// Next SEXPs to process.
    ///
    /// When serializing, instead of recursing, we add nested SEXPs to this
    /// queue, serialize their outer structure, then process them later. When
    /// deserializing, we return allocated-but-empty SEXPs and deserialize their
    /// contents later.
    Worklist& worklist;

    Deserializer(ByteBuffer& buffer, Worklist& worklist)
        : buffer(buffer), worklist(worklist) {}

    friend SEXP deserializeSrcRoot(ByteBuffer& buffer);
  public:
    /// Write raw data, can't contain any references
    template<typename T> T readBytesOf() {
        T c;
        buffer.getBytes((uint8_t*)&c, sizeof(c));
        return c;
    }
    /// Write raw data, can't contain any references
    void readBytes(void* data, size_t size) {
        buffer.getBytes((uint8_t*)data, size);
    }
    /// Read SEXP. ASTs read differently and faster
    SEXP read(bool isAst = false);
    /// Read SEXP in source pool ([src_pool_add])
    SEXP readSrc(unsigned idx);
    /// Read SEXP which could be nullptr
    SEXP readNullable(bool isAst = false) {
        auto isNull = !readBytesOf<bool>();
        if (isNull) {
            return nullptr;
        } else {
            return read(isAst);
        }
    }
}; */

} // namespace rir
