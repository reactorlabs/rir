#ifndef RIR_CODE_H
#define RIR_CODE_H

#include "ArglistOrder.h"
#include "PirTypeFeedback.h"
#include "RirRuntimeObject.h"
#include "bc/BC_inc.h"

#include <cassert>
#include <cstdint>
#include <ostream>
#include <time.h>

#ifndef __ARM_ARCH
#include <asm/msr.h>
#endif

namespace rir {

typedef SEXP FunctionSEXP;
typedef SEXP CodeSEXP;

#define CODE_MAGIC 0xc0de0000
#define NATIVE_CODE_MAGIC 0xc0deffff

/**
 * Code holds a sequence of instructions; for each instruction
 * it records the index of the source AST. Code is part of a
 * Function.
 *
 * Each Code object is embedded inside a SEXP, and needs to
 * be unpacked. The Function object has an array of SEXPs
 * pointing to Code objects.
 *
 * Instructions are variable size; Code knows how many bytes
 * are required for instructions.
 *
 * The number of indices of source ASTs stored in Code equals
 * the number of instructions.
 *
 * Instructions and AST indices are allocated one after the
 * other in the Code's data section with padding to ensure
 * alignment of indices.
 */
#pragma pack(push)
#pragma pack(1)

struct InterpreterInstance;
struct Code;
typedef SEXP (*NativeCode)(Code*, void*, SEXP, SEXP);

struct Code : public RirRuntimeObject<Code, CODE_MAGIC> {
    friend class FunctionWriter;
    friend class CodeVerifier;

    enum class Kind { Bytecode, Native } kind;

    // extra pool, pir type feedback, arg reordering info
    static constexpr size_t NumLocals = 4;

    Code(Kind kind, FunctionSEXP fun, SEXP src, unsigned srcIdx,
         unsigned codeSize, unsigned sourceSize, size_t localsCnt,
         size_t bindingsCacheSize);
    ~Code();

  private:
    Code() : Code(Kind::Bytecode, nullptr, 0, 0, 0, 0, 0, 0) {}
    static Code* New(Kind kind, Immediate ast, size_t codeSize, size_t sources,
                     size_t locals, size_t bindingCache);
    /*
     * This array contains the GC reachable pointers. Currently there are three
     * of them.
     * 0 : the extra pool for attaching additional GC'd object to the code
     * 1 : pir type feedback
     * 2 : call argument reordering metadata
     * 3 : rir function
     */
    SEXP locals_[NumLocals];

  public:
    static Code* NewBytecode(Immediate ast, size_t codeSize, size_t sources,
                             size_t locals, size_t bindingCache);
    static Code* NewNative(Immediate ast);

    constexpr static size_t MAX_CODE_HANDLE_LENGTH = 64;

  private:
    char lazyCodeHandle_[MAX_CODE_HANDLE_LENGTH] = "\0";
    NativeCode nativeCode_;
    NativeCode lazyCompile();

  public:
    void lazyCodeHandle(const std::string& h) {
        assert(h != "");
        assert(kind == Kind::Native);
        auto l = h.length() + 1;
        if (l > MAX_CODE_HANDLE_LENGTH) {
            assert(false);
            l = MAX_CODE_HANDLE_LENGTH;
        }
        memcpy(&lazyCodeHandle_, h.c_str(), l);
        lazyCodeHandle_[MAX_CODE_HANDLE_LENGTH - 1] = '\0';
    }
    NativeCode nativeCode() {
        if (nativeCode_)
            return nativeCode_;
        if (kind == Kind::Bytecode || *lazyCodeHandle_ == '\0')
            return nullptr;
        return lazyCompile();
    }

    bool isCompiled() const { return kind == Kind::Native && nativeCode_; }
    // For Kind::Native there is an in-between state when the Code is already
    // placed in a Function but its code handle isn't yet filled by the
    // finalizer of PirJitLLVM. We need to prevent such instances from being
    // evaluated (if we trigger some code in the backend, eg. during printing).
    // The current workaround is to skip them during dispatch.
    bool pendingCompilation() const {
        return kind == Kind::Native && *lazyCodeHandle_ == '\0';
    }

    static unsigned pad4(unsigned sizeInBytes) {
        unsigned x = sizeInBytes % 4;
        return (x != 0) ? (sizeInBytes + 4 - x) : sizeInBytes;
    }

    enum Flag {
        NoReflection,

        FIRST = NoReflection,
        LAST = NoReflection
    };

    EnumSet<Flag> flags;

    unsigned src; /// AST of the function (or promise) represented by the code

    SEXP trivialExpr; /// If this code object is a trivial expression

    unsigned stackLength; /// Number of slots in stack required

    const unsigned localsCount; /// Number of slots for local variables

    const unsigned bindingCacheSize; /// Number of different(ldVars|stVars)

    unsigned codeSize; /// bytes of code (not padded)

    unsigned srcLength; /// number of sources attached

    unsigned extraPoolSize; /// Number of elements in the per code constant pool

    uint8_t data[]; /// the instructions

    /*
     * The Layout of data[] is actually:
     *
     *   Content       Format            Bytesize
     *   ---------------------------------------------------------------------
     *   code stream   BC                pad4(codeSize)
     *
     *   srcList       cp_idx (ast)      srcLength * sizeof(SrclistEntry)
     *
     */

    // The source list contains pcOffset to src index
    struct SrclistEntry {
        unsigned pcOffset;
        unsigned srcIdx;
    };

    /** Returns a pointer to the instructions in c.  */
    Opcode* code() const { return (Opcode*)data; }
    Opcode* endCode() const { return (Opcode*)((uintptr_t)code() + codeSize); }

    // Usually SEXP pointers are loaded through the const pool. But sometimes
    // we want to be able to attach things to the code objects which:
    // 1. should get collected when the code is not longer needed
    // 2. is added at runtime
    // Those elements can be added to the extra pool.
    unsigned addExtraPoolEntry(SEXP v);
    SEXP getExtraPoolEntry(unsigned i) const {
        assert(i < extraPoolSize);
        return VECTOR_ELT(getEntry(0), i);
    }

    Code* getPromise(size_t idx) const {
        return unpack(getExtraPoolEntry(idx));
    }

    PirTypeFeedback* pirTypeFeedback() const {
        SEXP map = getEntry(1);
        if (!map)
            return nullptr;
        return PirTypeFeedback::unpack(map);
    }
    void pirTypeFeedback(PirTypeFeedback* map) {
        setEntry(1, map->container());
    }

    ArglistOrder* arglistOrder() const {
        SEXP data = getEntry(2);
        if (!data)
            return nullptr;
        return ArglistOrder::unpack(data);
    }

    rir::Function* function() const;
    void function(rir::Function*);

    void arglistOrder(ArglistOrder* data) { setEntry(2, data->container()); }
    SEXP arglistOrderContainer() const { return getEntry(2); }

    size_t size() const {
        return sizeof(Code) + pad4(codeSize) + srcLength * sizeof(SrclistEntry);
    }

    static size_t size(unsigned codeSize, unsigned sources) {
        return sizeof(Code) + pad4(codeSize) + sources * sizeof(SrclistEntry);
    }

    unsigned getSrcIdxAt(const Opcode* pc, bool allowMissing) const;

    static Code* deserialize(Function* rirFunction, SEXP refTable, R_inpstream_t inp);
    static Code* deserialize(SEXP refTable, R_inpstream_t inp) {
        return deserialize(nullptr, refTable, inp);
    }
    void serialize(bool includeFunction, SEXP refTable, R_outpstream_t out) const;
    void serialize(SEXP refTable, R_outpstream_t out) const {
        serialize(true, refTable, out);
    }
    void disassemble(std::ostream&, const std::string& promPrefix) const;
    void disassemble(std::ostream& out) const { disassemble(out, ""); }
    void print(std::ostream&) const;

    static size_t extraPtrOffset() {
        static Code* c = (Code*)malloc(sizeof(Code));
        assert(c);
        return (uintptr_t)&c->locals_ - (uintptr_t)c;
    }

  private:
    SrclistEntry* srclist() const {
        return (SrclistEntry*)(data + pad4(codeSize));
    }
};

#pragma pack(pop)

} // namespace rir

#endif
