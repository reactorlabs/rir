
//
// Created by Jakob Hain on 8/9/23.
//

#pragma once

#include "R/r_incl.h"
#include "utils/ByteBuffer.h"
#include "utils/EnumSet.h"
#include <queue>
#include <unordered_map>

namespace rir {

#define DESERIALIZE(lhs, fun, flags) if (deserializer.willRead(flags)) lhs = deserializer.fun(flags)

/// Details about serialized children to 1) optimize and 2) filter what gets
/// serialized and deserialized (e.g. when hashing, we leave out some data
/// because we want the hash to be semi-consistent).
///
/// The flags are additive: having a flag only enables more types and categories
/// of data to be serialized. Most flags control what is serialized. However,
/// `MaybeNotAst` instead allows us to use an optimal serialization algorithm
/// for ASTs when hashing; and `MaybeSexp` is simply a sanity check that allows
/// calling `write(SEXP)` without an assertion failure (and may be removed
/// later, since it's not necessary, not really useful, and most of the time
/// coult be chacked by the compiler).
enum class SerialFlag {
    /// Data is serialized when computing hash
    Hashed,
    /// Data, if SEXP, is not necessarily an AST (ASTs are hashed differently)
    MaybeNotAst,
    /// Data, if SEXP, is not necessarily a recorded call. Recorded calls are
    /// always serialized via hash, but other data is serialized inline on the
    /// client, since the client doesn't remember every SEXP.
    MaybeNotRecordedCall,
    /// Data might be an SEXP (sanity check: assertions fail if we serialize an
    /// SEXP without this flag)
    MaybeSexp,
    /// Data is serialized in source.
    InSource,
    /// Data is serialized in feedback.
    InFeedback,

    FIRST = Hashed,
    LAST = InFeedback
};

/// Wrapper so you can't construct non-sensical collections of flags
class SerialFlags {
    static unsigned nextId;
    unsigned id_;
    EnumSet<SerialFlag> flags;

    SerialFlags(bool hashed, bool maybeNotAst, bool maybeNotRecordedCall,
                bool maybeSexp, bool inSource, bool inFeedback)
        : id_(nextId++), flags() {
        if (hashed) flags.set(SerialFlag::Hashed);
        if (maybeNotAst) flags.set(SerialFlag::MaybeNotAst);
        if (maybeNotRecordedCall) flags.set(SerialFlag::MaybeNotRecordedCall);
        if (maybeSexp) flags.set(SerialFlag::MaybeSexp);
        if (inSource) flags.set(SerialFlag::InSource);
        if (inFeedback) flags.set(SerialFlag::InFeedback);
    }

  public:
    bool contains(SerialFlag f) const { return flags.contains(f); }
    /// Each serial flag has its own identifier which is used for santity
    /// checks, since these are static singletons.
    unsigned id() const { return id_; }

    static SerialFlags Inherit;
    static SerialFlags Ast;
    static SerialFlags DtContext;
    static SerialFlags DtBaseline;
    static SerialFlags DtOptimized;
    static SerialFlags FunBody;
    static SerialFlags FunDefaultArg;
    static SerialFlags FunStats;
    static SerialFlags FunMiscBytes;
    static SerialFlags CodeArglistOrder;
    /// In source, but nearly always if not always will be serialized as a ref
    /// because we've already starter serializing the outer function.
    static SerialFlags CodeOuterFun;
    /// Child promise in extra pool
    static SerialFlags CodePromise;
    /// Data is part of a record_ bytecode. SEXP is a recorded call in extra pool.
    static SerialFlags CodeFeedback;
    /// Unclassified SEXP in extra pool: original bytecode, any pool entry in
    /// native code.
    static SerialFlags CodePoolUnknown;
    /// Code kind (i.e. whether the code is native) and native code.
    ///
    /// Technically in source, will rarely if ever actually be in source: unless
    /// we compile a push_ bc which pushes a native code promise, not even a
    /// dispatch table with native code
    static SerialFlags CodeNative;
    static SerialFlags CodeAst;
    static SerialFlags CodeMisc;
};

/// Serialized SEXP with flags
struct SerialElem {
    SEXP sexp = nullptr;
    SerialFlags flags;
};
/// Queue of elements to serialize. Not every serializer uses this, but most do
typedef std::queue<SerialElem> SerialWorklist;
/// Map of SEXP to ref which will be written in its place if it gets serialized
/// again
typedef std::unordered_map<SEXP, unsigned> SerializedRefs;
/// Vector of SEXPs (map of int to SEXP) which will be returned in place of the
/// serialized refs
typedef std::vector<SEXP> DeserializedRefs;

/// Abstract class to serialize or hash an SEXP
class AbstractSerializer {
  protected:
    AbstractSerializer() = default;

    /// Serial ref table. Returns nullptr if we don't recurse
    virtual SerializedRefs* refs() = 0;
    /// Write SEXP contents
    void writeInline(SEXP s);

  public:
    /// Whether we will write the data with the given flags. Can be used to
    /// optimize by removing null-op calls.
    virtual bool willWrite(const SerialFlags& flags) const = 0;
    /// Write raw data, can't contain any references
    virtual void writeBytes(const void* data, size_t size,
                            const SerialFlags& flags) = 0;
    /// Write raw data, can't contain any references
    void writeBytes(const void* data, size_t size) {
        writeBytes(data, size, SerialFlags::Inherit);
    }
    /// Write sizeof(int) bytes of raw data, can't contain any references
    virtual void writeInt(int data, const SerialFlags& flags) = 0;
    /// Write sizeof(int) bytes of raw data, can't contain any references
    void writeInt(int data) { writeInt(data, SerialFlags::Inherit); }
    /// Write raw data, can't contain any references
    template <typename T>
    inline void writeBytesOf(T c,
                             const SerialFlags& flags = SerialFlags::Inherit) {
        if (sizeof(c) == sizeof(int)) {
            writeInt(*reinterpret_cast<int*>(&c), flags);
        } else {
            writeBytes((void*)&c, sizeof(c), flags);
        }
    }
    /// Write SEXP (recurse). If non-trivial, may actually write the SEXP
    /// contents later instead of actually recursing
    virtual void write(SEXP s, const SerialFlags& flags) = 0;
    /// Write SEXP (recurse). If non-trivial, may actually write the SEXP
    /// contents later instead of actually recursing
    void write(SEXP s) { write(s, SerialFlags::Inherit); }
    /// Write SEXP which could be nullptr
    void writeNullable(SEXP s,
                       const SerialFlags& flags = SerialFlags::Inherit) {
        writeBytesOf<bool>(s != nullptr, flags);
        if (s) {
            write(s, flags);
        }
    }
    /// Write SEXP in constant pool ([cp_pool_at])
    void writeConst(unsigned idx,
                    const SerialFlags& flags = SerialFlags::Inherit);
    /// Write SEXP in source pool ([src_pool_at])
    void writeSrc(unsigned idx, const SerialFlags& flags = SerialFlags::Ast);
};

/// Abstract class to deserialize an SEXP
class AbstractDeserializer {
  protected:
    AbstractDeserializer() = default;

    /// Serial ref table. Returns nullptr if we don't recurse
    virtual DeserializedRefs* refs() = 0;
    /// Read SEXP
    SEXP readInline();

  public:
    /// Whether we will write the data with the given flags. Otherwise we will
    /// set the data to 0/null. Can be used to optimize by removing null-op
    /// calls AND needed when the data isn't null by default.
    virtual bool willRead(const SerialFlags& flags) const = 0;
    /// Read raw data, can't contain any references
    virtual void readBytes(void* data, size_t size,
                           const SerialFlags& flags) = 0;
    /// Read raw data, can't contain any references
    void readBytes(void* data, size_t size) {
        readBytes(data, size, SerialFlags::Inherit);
    }
    /// Read sizeof(int) bytes of raw data, can't contain any references
    virtual int readInt(const SerialFlags& flags) = 0;
    /// Read sizeof(int) bytes of raw data, can't contain any references
    int readInt() { return readInt(SerialFlags::Inherit); }
    /// Read raw data, can't contain any references
    template <typename T>
    inline T readBytesOf(const SerialFlags& flags = SerialFlags::Inherit) {
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
    /// container which gets filled with deserialized data later, instead of
    /// actually recursing
    virtual SEXP read(const SerialFlags& flags) = 0;
    /// Read SEXP (recurse). If non-trivial, the returned SEXP may be an empty
    /// container which gets filled with deserialized data later, instead of
    /// actually recursing
    SEXP read() { return read(SerialFlags::Inherit); }
    /// Read SEXP which could be nullptr
    SEXP readNullable(const SerialFlags& flags = SerialFlags::Inherit) {
        if (readBytesOf<bool>(flags)) {
            return read(flags);
        } else {
            return nullptr;
        }
    }
    /// Read SEXP in constant pool ([cp_pool_add])
    unsigned readConst(const SerialFlags& flags = SerialFlags::Inherit);
    /// Read SEXP in source pool ([src_pool_add])
    unsigned readSrc(const SerialFlags& flags = SerialFlags::Ast);
    virtual void addRef(SEXP s) {
        if (refs()) {
            refs()->push_back(s);
        }
    }
};

} // namespace rir
