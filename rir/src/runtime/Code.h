#ifndef RIR_CODE_H
#define RIR_CODE_H

#include "RirHeader.h"
#include "ir/BC_inc.h"

#include <cassert>
#include <cstdint>
#include <ostream>

namespace rir {

typedef SEXP FunctionSEXP;
typedef SEXP CodeSEXP;

struct Function;
struct FunctionSignature;

#define CODE_MAGIC 0x3111eeee

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

static unsigned pad4(unsigned sizeInBytes) {
    unsigned x = sizeInBytes % 4;
    return (x != 0) ? (sizeInBytes + 4 - x) : sizeInBytes;
}

struct Code {
    friend struct Function;
    friend class FunctionWriter;
    friend class CodeVerifier;

    Code() = delete;

    Code(FunctionSEXP fun, size_t index, SEXP ast, unsigned codeSize,
         unsigned sourceSize, bool isDefaultArg, size_t localsCnt);

    // expose Code as an SEXP to the R gc
    rir::rir_header info;

  private:
    FunctionSEXP function_;

  public:
    size_t index; /// index of this Code object in the Function array

    // TODO comment these
    unsigned src; /// AST of the function (or promise) represented by the code

    unsigned stackLength; /// Number of slots in stack required

    // TODO: for now, only use 1 slot for everything (to store current env)
    unsigned localsCount; /// Number of slots for local variables

    unsigned codeSize; /// bytes of code (not padded)

    unsigned srcLength; /// number of sources attached

    unsigned perfCounter;

    unsigned isDefaultArgument : 1; /// is this a compiled default value
                                    /// of a formal argument
    unsigned free : 31;

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

    SEXP container() {
        SEXP result = (SEXP)((uintptr_t)this - sizeof(VECTOR_SEXPREC));
        assert(TYPEOF(result) == EXTERNALSXP &&
               "Code object not embedded in SEXP container, or corrupt.");
        return result;
    }

    static Code* unpack(SEXP s) {
        Code* c = (Code*)INTEGER(s);
        assert(c->info.magic == CODE_MAGIC &&
               "This container does not contain a Code object.");
        return c;
    }

    static Code* check(SEXP s) {
        if (TYPEOF(s) != EXTERNALSXP) {
            return nullptr;
        }
        Code* c = (Code*)INTEGER(s);
        return c->info.magic == CODE_MAGIC ? c : nullptr;
    }

    /** Returns a pointer to the instructions in c.  */
    Opcode* code() const { return (Opcode*)data; }
    Opcode* endCode() const { return (Opcode*)((uintptr_t)code() + codeSize); }

    Function* function();

    size_t size() const {
        return sizeof(Code) + pad4(codeSize) + srcLength * sizeof(SrclistEntry);
    }

    static size_t size(unsigned codeSize, unsigned sources) {
        return sizeof(Code) + pad4(codeSize) + sources * sizeof(SrclistEntry);
    }

    unsigned getSrcIdxAt(const Opcode* pc, bool allowMissing) const;

    void disassemble(std::ostream&) const;
    void print(std::ostream&) const;

  private:
    SrclistEntry* srclist() const {
        return (SrclistEntry*)(data + pad4(codeSize));
    }
};

#pragma pack(pop)

class FunctionCodeIterator {
    Function const* const function;
    size_t index;

  public:
    FunctionCodeIterator(Function const* const function, size_t index);
    void operator++();
    bool operator!=(FunctionCodeIterator other);
    Code* operator*();
};

class ConstFunctionCodeIterator {
    Function const* const function;
    size_t index;

  public:
    ConstFunctionCodeIterator(Function const* const function, size_t index);
    void operator++();
    bool operator!=(ConstFunctionCodeIterator other);
    const Code* operator*();
};
}

#endif
