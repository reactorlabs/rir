#ifndef RIR_INTERPRETER_DATA_C_H
#define RIR_INTERPRETER_DATA_C_H

#include <stdint.h>
#include <assert.h>
#include "../config.h"

#include "R/r.h"


#ifdef __cplusplus
extern "C" {
#else
#define bool int
#define true 1
#define false 0
#endif

#ifdef ENABLE_SLOWASSERT
#define SLOWASSERT(what) assert(what)
#else
#define SLOWASSERT(what)                                                       \
    {}
#endif

#if defined(__GNUC__) && (! defined(NO_THREADED_CODE))
#  define THREADED_CODE
#endif

// TODO force inlining for clang & gcc
#define INLINE __attribute__((always_inline)) inline static

// Function magic constant is designed to help to distinguish between Function
// objects and normal EXTERNALSXPs. Normally this is not necessary, but a very
// creative user might try to assign arbitrary EXTERNAL to a closure which we
// would like to spot. Of course, such a creative user might actually put the
// magic in his vector too...
#define FUNCTION_MAGIC (unsigned)0xCAFEBABE

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

/** How many bytes do we need to align on 4 byte boundary?
 */
INLINE unsigned pad4(unsigned sizeInBytes) {
    unsigned x = sizeInBytes % 4;
    return (x != 0) ? (sizeInBytes + 4 - x) : sizeInBytes;
}

// we cannot use specific sizes for enums in C
typedef uint8_t OpcodeT;

/** Any argument to BC must be the size of this type. */
// TODO add static asserts somewhere in C++
typedef uint32_t ArgT;

// type  for constant & ast pool indices
typedef uint32_t Immediate;

// type  signed immediate values (unboxed ints)
typedef uint32_t SignedImmediate;

// type of relative jump offset (all jumps are relative)
typedef int32_t JumpOffset;

typedef unsigned FunctionIndex;
typedef unsigned ArgumentsCount;

// enums in C are not namespaces so I am using OP_ to disambiguate
typedef enum {
#define DEF_INSTR(name, ...) name,
#include "ir/insns.h"
    numInsns_
} Opcode;

/**
 * Aliases for readability.
 */
typedef SEXP FunctionSEXP;
typedef SEXP ClosureSEXP;
typedef SEXP PromiseSEXP;
typedef SEXP IntSEXP;

// all sizes in bytes,
// length in element sizes

// ============
// Please do not change those without also changing how they are handled in the
// CodeEditor
// Indicates an argument is missing
#define MISSING_ARG_IDX ((unsigned)-1)
// Indicates an argument does not correspond to a valid CodeObject
#define DOTS_ARG_IDX ((unsigned)-2)
// Maximum valid entry for a CodeObject offset/idx entry 
#define MAX_ARG_IDX ((unsigned)-3)

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

typedef struct Code {
    unsigned magic; ///< Magic number that attempts to be PROMSXP already marked
    /// by the GC

    unsigned header; /// offset to Function object

    // TODO comment these
    unsigned src; /// AST of the function (or promise) represented by the code

    unsigned stackLength; /// Number of slots in stack required

    unsigned codeSize; /// bytes of code (not padded)

    unsigned skiplistLength; /// number of skiplist entries

    unsigned srcLength; /// number of instructions

    unsigned callSiteLength; /// length of the call site information

    unsigned perfCounter;

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
     *   callSites     CallSiteStruct    callSiteLength
     *
     *
     *
     * CallSiteStructs are laid out next to each other. Since they are variable
     * length, the call instruction refers to them by offset.
     *
     */
} Code;

const static unsigned CallSiteProfile_maxTaken = 1 << 28;
const static unsigned CallSiteProfile_maxTargets = 4;
typedef struct {
    uint32_t taken : 28;
    uint32_t takenOverflow : 1;
    uint32_t numTargets : 2;
    uint32_t targetsOverflow : 1;
    SEXP targets[3];
} CallSiteProfile;

typedef struct {
    uint32_t call;

    uint32_t hasNames : 1;
    uint32_t hasSelector : 1;
    uint32_t hasTarget : 1;
    uint32_t hasImmediateArgs : 1;
    uint32_t hasProfile : 1;
    uint32_t free : 27;

    // This is duplicated in the BC instruction, not sure how to avoid
    // without making accessing the payload a pain...
    uint32_t nargs;

    // This is not always needed, but maybe it does not pay off to put it
    // in the payload just to save 4 bytes...
    uint32_t trg;

    uint32_t payload[];

    /*
     * Layout of args is:
     *
     * nargs * promise offset    if hasImmediateArgs
     * nargs * cp_idx of names   if hasNames
     * CallSiteProfile           if hasProfile
     *
     */

} CallSiteStruct;

#pragma pack(pop)

INLINE uint32_t* CallSite_selector(CallSiteStruct* cs) {
    assert(cs->hasSelector);
    return &cs->trg;
}

INLINE uint32_t* CallSite_target(CallSiteStruct* cs) {
    assert(cs->hasTarget);
    return &cs->trg;
}

INLINE uint32_t* CallSite_names(CallSiteStruct* cs) {
    assert(cs->hasNames);
    return &cs->payload[(cs->hasImmediateArgs ? cs->nargs : 0)];
}

INLINE uint32_t* CallSite_args(CallSiteStruct* cs) {
    assert(cs->hasImmediateArgs);
    return cs->payload;
}

INLINE CallSiteProfile* CallSite_profile(CallSiteStruct* cs) {
    assert(cs->hasProfile);
    return (CallSiteProfile*)&cs
        ->payload[(cs->hasImmediateArgs ? cs->nargs : 0) +
                  (cs->hasNames ? cs->nargs : 0)];
}

INLINE unsigned CallSite_size(bool hasImmediateArgs, bool hasNames,
                              bool hasProfile, uint32_t nargs) {
    return sizeof(CallSiteStruct) +
           sizeof(uint32_t) *
               ((hasImmediateArgs ? nargs : 0) + (hasNames ? nargs : 0)) +
           +(hasProfile ? sizeof(CallSiteProfile) : 0);
}

INLINE unsigned CallSite_sizeOf(CallSiteStruct* cs) {
    return CallSite_size(cs->hasImmediateArgs, cs->hasNames, cs->hasProfile,
                         cs->nargs);
}

/** Returns whether the SEXP appears to be valid promise, i.e. a pointer into
 * the middle of the linearized code.
 */
INLINE Code* isValidCodeObject(SEXP s) {
    if (TYPEOF(s) != EXTERNALSXP)
        return nullptr;
    Code* c = (Code*)s;
    if (c->magic != CODE_MAGIC)
        return nullptr;
    return c;
}

/** Returns a pointer to the instructions in c.  */
INLINE OpcodeT* code(Code* c) { return (OpcodeT*)c->data; }

/** Returns a pointer to the source AST indices in c.  */
INLINE unsigned* skiplist(Code* c) {
    return (unsigned*)(c->data + pad4(c->codeSize));
}

INLINE unsigned* raw_src(Code* c) {
    return (unsigned*)((char*)skiplist(c) +
                       c->skiplistLength * 2 * sizeof(unsigned));
}

INLINE char* callSites(Code* c) {
    return (char*)raw_src(c) + c->srcLength * sizeof(unsigned);
}

INLINE CallSiteStruct* CallSite_get(Code* code, uint32_t idx) {
    return (CallSiteStruct*)&callSites(code)[idx];
}

/** Moves the pc to next instruction, based on the current instruction length
 */
INLINE OpcodeT* advancePc(OpcodeT* pc) {
    switch (*pc++) {
#define DEF_INSTR(name, imm, ...)                                              \
    case name:                                                                 \
        pc += sizeof(ArgT) * imm;                                              \
        break;
#include "ir/insns.h"
    default:
        assert(false && "Unknown instruction");
    }
    return pc;
}

INLINE unsigned getSrcIdxAt(Code* c, OpcodeT* pc, bool allowMissing) {

    unsigned* sl = skiplist(c);
    unsigned sl_i = 0;
    OpcodeT* start = c->data;

    SLOWASSERT(allowMissing || *sl <= pc - start);

    if (sl[0] > pc - start)
        return 0;

    while (sl[sl_i] <= pc - start && sl_i < 2 * c->skiplistLength)
        sl_i += 2;

    // we need to determine index of the current instruction
    OpcodeT* x = code(c) + sl[sl_i - 2];
    // find the pc of the current instructions
    unsigned insIdx = sl[sl_i - 1];

    while (x != pc) {
        x = advancePc(x);
        ++insIdx;
        if (insIdx == c->srcLength) {
            SLOWASSERT(allowMissing);
            return 0;
        }
    }
    unsigned sidx = raw_src(c)[insIdx];
    SLOWASSERT(allowMissing || sidx);

    return sidx;
}

/** Returns the next Code in the current function. */
INLINE Code* next(Code* c) {
    return (
        Code*)(c->data + pad4(c->codeSize) + c->srcLength * sizeof(unsigned) +
               c->skiplistLength * 2 * sizeof(unsigned) + c->callSiteLength);
}

// TODO removed src reference, now each code has its own

/** A Function holds the RIR code for some GNU R function.
 *  Each function start with a header and a sequence of
 *  Code objects for the body and all of the promises
 *  in the code.
 *
 *  The header start with a magic constant. This is a
 *  temporary hack so that it is possible to differentiate
 *  an R int vector from a Function. Eventually, we will
 *  add a new SEXP type for this purpose.
 *
 *  The size of the function, in bytes, includes the size
 *  of all of its Code objects and is padded to a word
 *  boundary.
 *
 *  A Function may be the result of optimizing another
 *  Function, in which case the origin field stores that
 *  Function as a SEXP pointer.
 *
 *  A Function has a source AST, stored in src.
 *
 *  A Function has a number of Code objects, codeLen, stored
 *  inline in data.
 */
#pragma pack(push)
#pragma pack(1)
typedef struct Function {
    unsigned magic; /// used to detect Functions 0xCAFEBABE

    unsigned size; /// Size, in bytes, of the function and its data

    unsigned invocationCount;

    unsigned envLeaked : 1;
    unsigned envChanged : 1;
    unsigned deopt : 1;
    unsigned markOpt : 1;
    unsigned spare : 28;

    FunctionSEXP origin; /// Same Function with fewer optimizations,
                         //   NULL if original

    FunctionSEXP next;

    unsigned codeLength; /// number of Code objects in the Function

    // We can get to this by searching, but this is faster and so worth the
    // extra four bytes
    unsigned foffset; ///< Offset to the code of the function (last code)

    uint8_t data[]; // Code objects stored inline
} Function;
#pragma pack(pop)

/** Returns the EXTERNALSXP for the Function object. */
INLINE SEXP function2store(Function* f) {
    SEXP result = (SEXP)((uintptr_t)f - sizeof(VECTOR_SEXPREC));
    assert(TYPEOF(result) == EXTERNALSXP &&
           "Either wrong memory altogether or bad counting");
    return result;
}

/** Returns the Function object an SEXP. */
INLINE Function* sexp2function(SEXP s) {
    return (Function*)INTEGER(s);
}

INLINE Function* isValidFunctionObject(SEXP s) {
    if (TYPEOF(s) != EXTERNALSXP)
        return nullptr;
    Function* f = sexp2function(s);
    if (f->magic != FUNCTION_MAGIC)
        return nullptr;
    return f;
}

/** Returns the first code object associated with the function.
 */
INLINE Code* begin(Function* f) { return (Code*)f->data; }

/** Returns the end of the function as code object, for interation purposes.
 */
INLINE Code* end(Function* f) { return (Code*)((uintptr_t)f + f->size); }

/** Returns a pointer to the Function to which code object c belongs. */
INLINE Function* code2function(Code* c) {
    return (Function*)((uintptr_t)c - c->header);
}

/** Returns a pointer to the code of the function (the last code object). */
INLINE Code* bodyCode(Function* f) {
    return (Code*)((uintptr_t)f + f->foffset);
}

/** Returns the code object with given offset */
INLINE Code* codeAt(Function* f, unsigned offset) {
    return (Code*)((uintptr_t)f + offset);
}

const static uint32_t NO_DEOPT_INFO = (uint32_t)-1;

#ifdef __cplusplus
}
#endif

#endif // RIR_INTERPRETER_C_H
