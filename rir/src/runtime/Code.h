#ifndef RIR_CODE_H
#define RIR_CODE_H

#include "Opcode.h"
#include "R/r.h"

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

    unsigned skiplistLength; /// number of skiplist entries

    unsigned srcLength; /// number of instructions

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
     *   skiplist      (instr offset,    2 * skiplistLength * sizeof(unsigned)
     *                  src index)
     *
     *   srcList       cp_idx (ast)      srcLength * sizeof(unsigned)
     *
     */

    /** Returns a pointer to the instructions in c.  */
    Opcode* code() { return (Opcode*)data; }

    Opcode* endCode() { return (Opcode*)((uintptr_t)code() + codeSize); }

    Function* function() { return (Function*)((uintptr_t) this - header); }

    size_t size() {
        return sizeof(Code) + pad4(codeSize) + srcLength * sizeof(unsigned) +
               skiplistLength * 2 * sizeof(unsigned);
    }

    static size_t size(unsigned codeSize, unsigned sourcesSize) {
        return sizeof(Code) + pad4(codeSize) + sourcesSize * sizeof(unsigned) +
               calcSkiplistLength(sourcesSize) * 2 * sizeof(unsigned);
    }

    unsigned getSrcIdxAt(Opcode* pc, bool allowMissing) {

        unsigned* sl = skiplist();
        unsigned sl_i = 0;
        Opcode* start = reinterpret_cast<Opcode*>(data);

        SLOWASSERT(allowMissing || *sl <= pc - start);

        if (sl[0] > pc - start)
            return 0;

        while (sl[sl_i] <= pc - start && sl_i < 2 * skiplistLength)
            sl_i += 2;

        // we need to determine index of the current instruction
        rir::Opcode* x = code() + sl[sl_i - 2];
        // find the pc of the current instructions
        unsigned insIdx = sl[sl_i - 1];

        while (x != pc) {
            x = advancePc(x);
            ++insIdx;
            if (insIdx == srcLength) {
                SLOWASSERT(allowMissing);
                return 0;
            }
        }
        unsigned sidx = raw_src()[insIdx];
        SLOWASSERT(allowMissing || sidx);

        return sidx;
    }

    void print();

    Code* next() { return (Code*)((uintptr_t) this + this->size()); }

  private:
    static size_t calcSkiplistLength(unsigned sourcesSize) {
        size_t s = 0.03 * sourcesSize;
        return s ? s : 1;
    }

    unsigned* skiplist() { return (unsigned*)(data + pad4(codeSize)); }

    unsigned* raw_src() {
        return (unsigned*)((char*)skiplist() +
                           skiplistLength * 2 * sizeof(unsigned));
    }
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
