#ifndef RIR_CODE_H
#define RIR_CODE_H

#include "R/r.h"
#include "ir/BC_inc.h"

#include <cassert>
#include <cstdint>

namespace rir {

struct Function;
struct FunctionSignature;

// Offset between function SEXP and Function* struct
// This is basically sizeof(SEXPREC_ALIGN)
#define FUNCTION_OFFSET 40

// Code magic constant is intended to trick the GC into believing that it is
// dealing with already marked SEXP.
// Note: gcgen needs to be 1, otherwise the write barrier will trigger and
//       named count is 2 to make it stable
//  It also has an unique bitpattern for gp (0xee) so that we can keep it apart
//  from functions
#define CODE_MAGIC (unsigned)0x1100ee9a

/**
 * Code holds a sequence of instructions; for each instruction
 * it records the index of the source AST. Code is part of a
 * Function.
 *
 * Code objects are allocated contiguously within the data
 * section of a Function. The Function header can be found,
 * at an offset from the start of each Code object
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
    friend class FunctionWriter;
    friend class CodeVerifier;

    Code() = delete;

    Code(SEXP ast, unsigned codeSize, unsigned sourceSize, unsigned offset,
         bool isDefaultArg, size_t localsCnt);

    // Magic number that attempts to be PROMSXP already marked by the GC
    unsigned magic;

    unsigned header; /// offset to Function object

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

    /** Returns a pointer to the instructions in c.  */
    Opcode* code() { return (Opcode*)data; }

    Opcode* endCode() { return (Opcode*)((uintptr_t)code() + codeSize); }

    Function* function() { return (Function*)((uintptr_t) this - header); }

    size_t size() {
        return sizeof(Code) + pad4(codeSize) + srcLength * sizeof(SrclistEntry);
    }

    static size_t size(unsigned codeSize, unsigned sources) {
        return sizeof(Code) + pad4(codeSize) + sources * sizeof(SrclistEntry);
    }

    unsigned getSrcIdxAt(Opcode* pc, bool allowMissing) {
        if (srcLength == 0) {
            assert(allowMissing);
            return 0;
        }

        SrclistEntry* sl = srclist();
        Opcode* start = reinterpret_cast<Opcode*>(data);
        auto pcOffset = pc - start;

        if (srcLength == 1) {
            auto sidx = sl[0].pcOffset == pcOffset ? sl[0].srcIdx : 0;
            SLOWASSERT(allowMissing || sidx);
            return sidx;
        }

        // Binary search through src list
        int lower = 0;
        int upper = srcLength - 1;
        int finger = upper / 2;
        unsigned sidx = 0;

        while (lower <= upper) {
            if (sl[finger].pcOffset == pcOffset) {
                sidx = sl[finger].srcIdx;
                break;
            }
            if (sl[finger].pcOffset < pcOffset)
                lower = finger + 1;
            else
                upper = finger - 1;
            finger = lower + (upper - lower) / 2;
        }
        SLOWASSERT(sidx == 0 || sl[finger].pcOffset == pcOffset);
        SLOWASSERT(allowMissing || sidx);

        return sidx;
    }

    void print();
    void disassemble();

    Code* next() { return (Code*)((uintptr_t) this + this->size()); }

  private:
    SrclistEntry* srclist() { return (SrclistEntry*)(data + pad4(codeSize)); }
};

#pragma pack(pop)

class CodeHandleIterator {
    Code* code;

  public:
    CodeHandleIterator(Code* code) { this->code = code; }

    void operator++() { code = (Code*)((uintptr_t)code + code->size()); }

    bool operator!=(CodeHandleIterator other) { return code != other.code; }

    Code* operator*() { return code; }
};
}

#endif
