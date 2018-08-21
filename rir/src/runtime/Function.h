#ifndef RIR_FUNCTION_H
#define RIR_FUNCTION_H

#include "Code.h"
#include "FunctionSignature.h"
#include "R/r.h"
#include "RirHeader.h"

namespace rir {

/**
 * Aliases for readability.
 */
typedef SEXP FunctionSEXP;

// Function magic constant is designed to help to distinguish between Function
// objects and normal EXTERNALSXPs. Normally this is not necessary, but a very
// creative user might try to assign arbitrary EXTERNAL to a closure which we
// would like to spot. Of course, such a creative user might actually put the
// magic in his vector too...
#define FUNCTION_MAGIC (unsigned)0xCAFEBABE

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
struct Function {
  public:
    Function() {
        info.gc_area_start = sizeof(rir_header); // just after the header
        info.gc_area_length = 2;                 // origin, next
        info.magic = FUNCTION_MAGIC;
        origin_ = nullptr;
        next_ = nullptr;
        size = sizeof(Function);
        signature = nullptr;
        invocationCount = 0;
        deopt = false;
        markOpt = false;
        codeLength = 0;
        foffset = 0;
    }

    SEXP container() {
        SEXP result = (SEXP)((uintptr_t) this - sizeof(VECTOR_SEXPREC));
        assert(TYPEOF(result) == EXTERNALSXP &&
               "Cannot get function container. Is it embedded in a SEXP?");
        return result;
    }

    static Function* check(SEXP s) {
        Function* f = (Function*)INTEGER(s);
        return f->info.magic == FUNCTION_MAGIC ? f : nullptr;
    }

    static Function* unpack(SEXP s) {
        Function* f = (Function*)INTEGER(s);
        assert(f->info.magic == FUNCTION_MAGIC &&
               "This container does not conatin a Function");
        return f;
    }

    Code* first() { return (Code*)data; }
    Code* codeEnd() { return (Code*)((uintptr_t) this + size); }

    const Code* first() const { return (Code*)data; }
    const Code* codeEnd() const { return (Code*)((uintptr_t)this + size); }

    Code* body() { return (Code*)((uintptr_t) this + foffset); }

    Code* codeAt(unsigned offset) const {
        Code* c = (Code*)((uintptr_t) this + offset);
        assert(c->magic == CODE_MAGIC && "Invalid code offset");
        return c;
    }

    CodeHandleIterator begin() { return CodeHandleIterator(first()); }
    CodeHandleIterator end() { return CodeHandleIterator(codeEnd()); }
    ConstCodeHandleIterator begin() const {
        return ConstCodeHandleIterator(first());
    }
    ConstCodeHandleIterator end() const {
        return ConstCodeHandleIterator(codeEnd());
    }

    unsigned indexOf(Code* code) {
        unsigned idx = 0;
        for (Code* c : *this) {
            if (c == code)
                return idx;
            ++idx;
        }
        assert(false);
        return 0;
    }

    FunctionSEXP origin() { return origin_; }

    void origin(Function* s) {
        EXTERNALSXP_SET_ENTRY(container(), 0, s->container());
    }

    FunctionSEXP next() { return next_; }

    void next(Function* s) {
        EXTERNALSXP_SET_ENTRY(container(), 1, s->container());
    }

    void registerInvocation() {
        if (invocationCount < UINT_MAX)
            invocationCount++;
    }

    rir::rir_header info; /// for exposing SEXPs to GC

  private:
    FunctionSEXP origin_; /// Same Function with fewer optimizations,
                          //   NULL if original
    FunctionSEXP next_;

  public:
    unsigned size; /// Size, in bytes, of the function and its data

    FunctionSignature* signature; /// pointer to this version's signature

    unsigned invocationCount;

    unsigned deopt : 1;
    unsigned markOpt : 1;
    unsigned spare : 30;

    unsigned codeLength; /// number of Code objects in the Function

    // We can get to this by searching, but this is faster and so worth the
    // extra four bytes
    unsigned foffset; ///< Offset to the code of the function (last code)

    uint8_t data[]; // Code objects stored inline
};
#pragma pack(pop)
}

#endif
