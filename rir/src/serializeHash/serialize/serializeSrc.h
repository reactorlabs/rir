//
// Created by Jakob Hain on 8/9/23.
//

#pragma once

#include "R/r_incl.h"
#include "runtime/DispatchTable.h"
#include "runtime/Function.h"
#include "utils/ByteBuffer.h"
#include <queue>

namespace rir {

/* class Serializer {
    struct Elem {
        SEXP sexp;
        bool isAst;
    };
    using Worklist = std::queue<Elem>;

    /// Underlying byte-buffer which we write data to
    ByteBuffer& buffer;
    /// Next SEXPs to process.
    ///
    /// When serializing, instead of recursing, we add nested SEXPs to this
    /// queue, serialize their outer structure, then process them later. When
    /// deserializing, we return allocated-but-empty SEXPs and deserialize their
    /// contents later.
    Worklist& worklist;

    Serializer(ByteBuffer& buffer, Worklist& worklist)
        : buffer(buffer), worklist(worklist) {}

    friend void serializeSrcRoot(SEXP root, ByteBuffer& buffer);
  public:
    /// Write raw data, can't contain any references
    template<typename T> void writeBytesOf(T c) {
        buffer.putBytes((uint8_t*)&c, sizeof(c));
    }
    /// Write raw data, can't contain any references
    void writeBytes(const void* data, size_t size) {
        buffer.putBytes((uint8_t*)data, size);
    }
    /// Write SEXP. ASTs write differently and faster
    void write(SEXP s, bool isAst = false);
    /// Write SEXP in source pool ([src_pool_at])
    void writeSrc(unsigned idx);
    /// Write SEXP which could be nullptr
    void writeNullable(SEXP s, bool isAst = false) {
        writeBytesOf<bool>(s != nullptr);
        if (s) {
            write(s, isAst);
        }
    }
}; */

} // namespace rir
