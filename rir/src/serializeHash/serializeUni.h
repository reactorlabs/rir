
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

/// Details about serialized children to 1) optimize and 2) filter what gets
/// serialized and deserialized (e.g. when hashing, we leave out some data
/// because we want the hash to be semi-consistent).
///
/// Some of these flags only apply to serialized data (readBytes and
/// writeBytes), some apply to serialized SEXPs (read SEXP and write SEXP)
enum class SerialFlag {
    /// Data, if SEXP, is not necessarily an AST (ASTs are serialized differently)
    MaybeNotAst,
    /// Data might be an SEXP (sanity check)
    MaybeSexp,
    /// Data is hashed
    Hashed,
    /// Data is serialized in source
    InSource,
    /// Data is serialized in feedback
    InFeedback,

    FIRST = MaybeNotAst,
    LAST = InFeedback
};

/// Wrapper so you can't construct non-sensical collections of flags
class SerialFlags {
    EnumSet<SerialFlag> inner;

    SerialFlags() : inner() {}
    template <typename... Args>
    explicit SerialFlags(Args... args) : inner() {
        for (auto f : {args...}) {
            inner.set(f);
        }
    }
    explicit SerialFlags(EnumSet<SerialFlag> inner) : inner(inner) {}

  public:
    bool contains(SerialFlag f) const { return inner.contains(f); }

    /// All flags are set. Flags are only unset in children.
    static SerialFlags Inherit;
    /// AST, not guaranteed RIR, hashed, in source, not in feedback
    static SerialFlags Ast;
    /// Not an SEXP, not hashed, in source, not in feedback
    static SerialFlags DtContext;
    /// Not an AST, guaranteed rir, hashed, in source, in feedback
    static SerialFlags DtBaseline;
    /// Not an AST, guaranteed RIR, not hashed, not in feedback, not in source
    static SerialFlags DtOptimized;
    /// Not an AST, guaranteed rir, hashed, in source, in feedback
    static SerialFlags FunBody;
    /// Not an AST, guaranteed rir, hashed, in source, in feedback
    static SerialFlags FunDefaultArg;
    /// Not an SEXP, hashed, in source, not in feedback
    static SerialFlags FunMiscBytes;
    /// Not an AST, guaranteed rir, hashed, in source, not in feedback
    static SerialFlags CodeArglistOrder;
    /// Child promise in extra pool
    ///
    /// Not an AST, guaranteed rir, hashed, in source, in feedback
    static SerialFlags CodePromise;
    /// Data is part of a record_ bytecode. SEXP is a recorded call in extra pool.
    ///
    /// Not an AST, not guaranteed rir, not hashed, not in source, in feedback
    static SerialFlags CodeFeedback;
    /// Unclassified SEXP in extra pool: original bytecode, any pool entry in
    /// native code.
    ///
    /// Not an AST, not guaranteed rir, hashed, not in source, not in feedback
    static SerialFlags CodePoolUnknown;
    /// Code kind (i.e. whether the code is native) and native code.
    ///
    /// Not an SEXP, hashed, not in source, not in feedback
    static SerialFlags CodeNative;
    /// Not an SEXP, hashed, in source, not in feedback
    static SerialFlags CodeMiscBytes;
};

/// Serialized SEXP with flags
struct SerialElem {
    SEXP sexp = nullptr;
    SerialFlags flags;
};
/// Queue of elements to serialize. Not every serializer uses this, but most do
using SerialWorklist = std::queue<SerialElem>;

/// Abstract class to serialize or hash an SEXP
class Serializer {
  protected:
    Serializer() = default;

  public:
    /// Write raw data, can't contain any references
    virtual void writeBytes(const void* data, size_t size, SerialFlags flags) = 0;
    /// Write sizeof(int) bytes of raw data, can't contain any references
    virtual void writeInt(int data, SerialFlags flags) = 0;
    /// Write raw data, can't contain any references
    template <typename T>
    inline void writeBytesOf(T c, SerialFlags flags) {
        if (sizeof(c) == sizeof(int)) {
            writeInt(*reinterpret_cast<int*>(&c), flags);
        } else {
            writeBytes((void*)&c, sizeof(c), flags);
        }
    }
    /// Write SEXP (recurse). If non-trivial, will actually write the SEXP
    /// contents later
    virtual void write(SEXP s, SerialFlags flags) = 0;
    /// Write SEXP which could be nullptr
    void writeNullable(SEXP s, SerialFlags flags) {
        writeBytesOf<bool>(s != nullptr, flags);
        if (s) {
            write(s, flags);
        }
    }
    /// Write SEXP in constant pool ([cp_pool_at])
    void writeConst(unsigned idx);
    /// Write SEXP in source pool ([src_pool_at])
    void writeSrc(unsigned idx);
};

/// Abstract class to deserialize an SEXP
class Deserializer {
  protected:
    Deserializer() = default;

  public:
    /// Read raw data, can't contain any references
    virtual void readBytes(void* data, size_t size, SerialFlags flags) = 0;
    /// Read sizeof(int) bytes of raw data, can't contain any references
    virtual int readInt(SerialFlags flags) = 0;
    /// Read raw data, can't contain any references
    template <typename T>
    inline T readBytesOf(SerialFlags flags) {
        if (sizeof(T) == sizeof(int)) {
            auto result = readInt(flags);
            return *reinterpret_cast<T*>(&result);
        } else {
            T result;
            readBytes((void*)&result, sizeof(result), flags);
            return result;
        }
    }
    /// Read SEXP (recurse). If non-trivial, the returned SEXP may be an empty
    /// container which gets filled with deserialized data later
    virtual SEXP read(SerialFlags flags) = 0;
    /// Read SEXP which could be nullptr
    SEXP readNullable(SerialFlags flags) {
        if (readBytesOf<bool>(flags)) {
            return read(flags);
        } else {
            return nullptr;
        }
    }
    /// Read SEXP in constant pool ([cp_pool_add])
    unsigned readConst();
    /// Read SEXP in source pool ([src_pool_add])
    unsigned readSrc();
};

} // namespace rir
