#ifndef RIR_BYTECODE_H_
#define RIR_BYTECODE_H_


#include "RIntlns.h"

#include "Code.h"
#include "Function.h"


namespace rjit {
namespace rir {

enum class CallingConvention : int {
    envLazy,
    stackLazy,
    stackEager,
};


/** Wrapper around a BCODESXP containing rir bytecode.

  R's bytecode SEXPs have two portions we can use - the constant pool and the code.

  The code is used to serialize the header (with special version so that rir and R bytecodes can be distinguished), followed by the rir bytecodes, which then are followed by position indices of the ast map.

  The constant pool has first the ast of the function, followed by any child RBytecode objects (promises, and promises of the promises recursively), so that these have the same indices in the constant pool as they had in the Function object, then followed by the asts from ast map.
 */
class RBytecode {
public:

    static constexpr int MAGIC_VERSION = 0xff;

    struct Header {
        /** Magic version number so that rir bytecode can be distinguished from R's bytecode. */
        int const version = MAGIC_VERSION;

        /** Expected calling convention. */
        CallingConvention cc;

        /** Number of child RBytecodes stored in the constant pool (promises). */
        int nchildren;

        /** Size of the code. */
        int codeSize;

        Header(CallingConvention cc, int nchildren, int codeSize):
            cc(cc),
            nchildren(nchildren),
            codeSie(codeSize) {
        }
    };

    static_assert(sizeof(Header) == 4 * sizeof(int), "Header should not have any padding");
    static_assert(sizeof(Header) % sizeof(BC_t) == 0, "To make sure alignment won't cause us trouble");

    RBytecode(Bytecode const &) = default;

    RBytecode & operator = (Bytecode const &) = default;

    /** Returns the header of the bytecode object.
     */
    Header const & header() const {
        return * reinterpret_cast<Header*>(INTEGER(code_()));
    }

    /** Returns the code part of the bytecode object.
     */
    BC_t const * code() const {
        return reinterpret_cast<BC_t *>(reinterpret_cast<Header*>(INTEGER(code_())) + 1);
    }

    /** Returns the index-th code object in the RBytecode.

      NOTE that this function follows the Function's data layout signature, i.e. the first child has index of 1, not 0. If 0 is used as an argument, the actual bytecode object can be returned, but currently an assertion fails.
     */
    RBytecode child(size_t index) {
        if (index == 0) {
            assert(false and "This should probably not happen");
            return this;
        }
        assert(index <= header().nchildren and "Not enough children");
        return RBytecode(GET_VECTOR_ELT(asts_, index));
    }

    /** Serializes a code object into RBytecode.
     */
    static RBytecode serialize(Code * code, CallingConvention cc) {
        return serialize_(code, cc, 0);
    }

    RBytecode serialize(Function * f, CallingConvention cc) {
        // serialize the main bytecode and prepare room for the promises
        RBytecode result(serialize_(f->code[0], cc, f->code.size() -1));
        PROTECT(result.data_);

        // serialize the promises
        SEXP asts = result.asts_();
        for (size_t i = 1, e = f->code.size(); i != e; ++i)
            SET_VECTOR_ELT(asts, i, serialize_(f->code[i], cc, 0));

        UNPROTECT(result.data_);
        return result;
    }

    /** Returns a pointer to the actual rir bytecode array contained in the RBytecode object
     */
    BC_t const * code() const {
        return reinterpret_cast<BC_t *>(reinterpret_cast<uint8_t *>(INTEGER(code_())) + sizeof(Header));
    }

private:

    static RBytecode serialize_(Code * code, CallingConvention cc, int nchildren) {
        // compute the required code size in bytes
        int size = sizeof(Header) + code->size + code->astMap.size;
        if (size % 4 != 0)
            size += 4 - (size % 4);

        // serialize the bytecode part and the header
        SEXP code = allocVector(INTSXP, size / 4);
        PROTECT(code);
        Header * hdr = reinterpret_cast<Header*>(INTEGER(code));
        *hdr = Header(cc, nchildren, code->size);
        // move past header
        hdr += 1;
        // copy the code
        memcpy(hdr, code->bc, code->size);
        // get pointer to the area right after the code and copy the positions
        uint8_t * astpos = reinterpret_cast<uint8_t *>(hdr) + code->size;
        memcpy(astpos, code->ast->pos, code->ast->size);

        // serialize the constant pool part
        SEXP consts = allocVector(VECSXP, 1 + nchildren + code->astMap.size);
        PROTECT(consts);
        // first item is the ast itself
        SET_VECTOR_ELT(consts_, 0, code->ast);
        // no promises in single code object
        // serialize the ast parts of the ast map into the constant pool right after the default ast
        for (size_t i = 0, e = code->ast->size; i < e; ++i)
            SET_VECTOR_ELT(consts, i + 1 + nchildren, code->astMap->ast[i]);

        // create the bytecode object
        SEXP result = Bytecode(code, consts);
        UNPROTECT(2);
        return result;
    }


    RBytecode(SEXP code, SEXP asts) {
        SEXP cs = p(constants);
        SEXP bc = p(code);
        // note that R_bcEncode is not required as threaded code makes no difference to us
        data_ = cons(code, asts);
        SET_TYPEOF(data_, BCODESXP);
    }



    SEXP code_() {
        return BCODE_CODE(data_);
    }

    SEXP asts_() {
        return BCODE_CONSTS(data_);
    }


    SEXP data_;
};


class RFunction {


    SEXP data_;
};

class RPromise {

    SEXP data_;
};



static_assert(sizeof(RBytecode) == sizeof(SEXP), "RBytecode is assumed to be only a wrapper");
static_assert(sizeof(RFunction) == sizeof(SEXP), "RFunction is assumed to be only a wrapper");
static_assert(sizeof(RPromise) == sizeof(SEXP), "RPromise is assumed to be only a wrapper");





}
}

#endif
