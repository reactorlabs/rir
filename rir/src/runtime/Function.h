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
    friend class FunctionCodeIterator;
    friend class ConstFunctionCodeIterator;

    static constexpr size_t CODEOBJ_OFFSET = 2;

    Function(size_t functionSize, const std::vector<SEXP>& codeVec)
        : size(functionSize), signature(nullptr), invocationCount(0),
          deopt(false), markOpt(false), codeLength(codeVec.size()),
          origin_(nullptr), next_(nullptr) {
        // GC area starts just before the end of Function
        info.gc_area_start =
            sizeof(Function) - CODEOBJ_OFFSET * sizeof(FunctionSEXP);
        info.gc_area_length = CODEOBJ_OFFSET + codeLength;
        info.magic = FUNCTION_MAGIC;
        // initialize codeObjects array to nullptr first!
        for (size_t i = 0; i < codeLength; ++i) {
            codeObjects[i] = nullptr;
        }
        for (size_t i = 0; i < codeLength; ++i) {
            Code* c = Code::unpack(codeVec[i]);
            // Set c->function_ to point to this Function's container
            EXTERNALSXP_SET_ENTRY(c->container(), 0, this->container());
            // Set codeObjects[i] to point to Code object c's container
            EXTERNALSXP_SET_ENTRY(container(), CODEOBJ_OFFSET + i,
                                  c->container());
        }
    }

    SEXP container() {
        SEXP result = (SEXP)((uintptr_t) this - sizeof(VECTOR_SEXPREC));
        assert(TYPEOF(result) == EXTERNALSXP &&
               "Cannot get function container. Is it embedded in a SEXP?");
        return result;
    }

    static Function* unpack(SEXP s) {
        Function* f = (Function*)INTEGER(s);
        assert(f->info.magic == FUNCTION_MAGIC &&
               "This container does not contain a Function");
        return f;
    }

    static Function* check(SEXP s) {
        if (TYPEOF(s) != EXTERNALSXP) {
            return nullptr;
        }
        Function* f = (Function*)INTEGER(s);
        return f->info.magic == FUNCTION_MAGIC ? f : nullptr;
    }

    Code* body() { return Code::unpack(codeObjects[codeLength - 1]); }

    Code* codeAt(unsigned index) const {
        return Code::unpack(codeObjects[index]);
    }

    FunctionCodeIterator begin() { return FunctionCodeIterator(this, 0); }
    FunctionCodeIterator end() {
        return FunctionCodeIterator(this, codeLength);
    }
    ConstFunctionCodeIterator begin() const {
        return ConstFunctionCodeIterator(this, 0);
    }
    ConstFunctionCodeIterator end() const {
        return ConstFunctionCodeIterator(this, codeLength);
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

    Code* findDefaultArg(size_t index) const {
        while (index < codeLength) {
            Code* c = Code::unpack(codeObjects[index++]);
            if (c->isDefaultArgument) {
                return c;
            }
        }
        assert(index == codeLength && "Did not find default arg so all Code "
                                      "objects should have been iterated "
                                      "over.");
        return nullptr;
    }

    rir::rir_header info; /// for exposing SEXPs to GC

    unsigned size; /// Size, in bytes, of the function and its data

    FunctionSignature* signature; /// pointer to this version's signature

    unsigned invocationCount;

    unsigned deopt : 1;
    unsigned markOpt : 1;
    unsigned spare : 30;

    unsigned codeLength; /// number of Code objects in the Function

  private:
    // !!! SEXPs traceable by the GC must be declared here !!!
    // !!!   *before* the CodeSEXP array.                  !!!
    // !!! Furthermore, you need to update                 !!!
    // !!!     the CODEOBJ_OFFSET constant.                !!!

    FunctionSEXP origin_; /// Same Function with fewer optimizations,
                          //   NULL if original
    FunctionSEXP next_;

    CodeSEXP codeObjects[]; /// Pointers to CodeSEXPs (Code objects embedded
                            //   inside EXTERNALSXP)
};
#pragma pack(pop)
}

#endif
