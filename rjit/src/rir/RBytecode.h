#ifndef RIR_BYTECODE_H_
#define RIR_BYTECODE_H_


#include "RIntlns.h"

#include "Code.h"
#include "Function.h"


namespace rjit {
namespace rir {

/** TODO We basically want the functionality of mkPROMSXP, line 2333 memory.c.

  As a hack I am just using one of the old rjit bytecodes which did exactly the same.
*/
extern "C" SEXP createPromise(SEXP code, SEXP rho);



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
            codeSize(codeSize) {
        }

    };

    static_assert(sizeof(Header) == 4 * sizeof(int), "Header should not have any padding");
    static_assert(sizeof(Header) % sizeof(BC_t) == 0, "To make sure alignment won't cause us trouble");

    RBytecode(SEXP from):
        data_(from) {
        assert(TYPEOF(from) == BCODESXP and "Creating RBytecode from something that is not bytecode");
        assert(version() == MAGIC_VERSION and "Invalid version - perhaps not rir bytecode");
    }

    RBytecode(RBytecode const &) = default;

    RBytecode & operator = (RBytecode const &) = default;

    /** RBytecode automatically converts to SEXP.
     */
    operator SEXP() {
        return data_;
    }

    /** Returns the header of the bytecode object.
     */
    Header const & header() const {
        return * reinterpret_cast<Header *>(INTEGER(code_()));
    }

    /** Returns the version of the bytecode.
     */
    int version() const {
        // not using the header so that we do not assume anything about the underlying sexp
        return INTEGER(code_())[0];
    }

    /** Returns the code part of the bytecode object.
     */
    BC_t const * code() const {
        return reinterpret_cast<BC_t *>(reinterpret_cast<Header *>(INTEGER(code_())) + 1);
    }

    /** Returns the index-th code object in the RBytecode.

      NOTE that this function follows the Function's data layout signature, i.e. the first child has index of 1, not 0. If 0 is used as an argument, the actual bytecode object can be returned, but currently an assertion fails.
     */
    RBytecode child(size_t index) {
        if (index == 0) {
            assert(false and "This should probably not happen");
            return *this;
        }
        assert(index <= static_cast<size_t>(header().nchildren) and "Not enough children");
        return RBytecode(VECTOR_ELT(asts_(), index));
    }

    /** Serializes a code object into RBytecode.
     */
    static RBytecode serialize(Code * code, CallingConvention cc) {
        return serialize_(code, cc, 0);
    }

    static RBytecode serialize(Function * f, CallingConvention cc) {
        // serialize the main bytecode and prepare room for the promises
        RBytecode result(serialize_(f->code[0], cc, f->code.size() -1));
        PROTECT(result.data_);

        // serialize the promises
        SEXP asts = result.asts_();
        for (size_t i = 1, e = f->code.size(); i != e; ++i)
            SET_VECTOR_ELT(asts, i, serialize_(f->code[i], cc, 0));

        UNPROTECT(1);
        return result;
    }

private:

    static RBytecode serialize_(Code * c, CallingConvention cc, int nchildren) {
        // compute the required code size in bytes
        int size = sizeof(Header) + c->size + c->astMap.size;
        if (size % 4 != 0)
            size += 4 - (size % 4);

        // serialize the bytecode part and the header
        SEXP code = allocVector(INTSXP, size / 4);
        PROTECT(code);
        Header * hdr = reinterpret_cast<Header*>(INTEGER(code));
        new (hdr) Header(cc, nchildren, c->size);
        // move past header
        hdr += 1;
        // copy the code
        memcpy(hdr, c->bc, c->size);
        // get pointer to the area right after the code and copy the positions
        uint8_t * astpos = reinterpret_cast<uint8_t *>(hdr) + c->size;
        memcpy(astpos, c->astMap.pos, c->astMap.size);

        // serialize the constant pool part
        SEXP consts = allocVector(VECSXP, 1 + nchildren + c->astMap.size);
        PROTECT(consts);
        // first item is the ast itself
        SET_VECTOR_ELT(consts, 0, c->ast);
        // no promises in single code object
        // serialize the ast parts of the ast map into the constant pool right after the default ast
        for (size_t i = 0, e = c->astMap.size; i < e; ++i)
            SET_VECTOR_ELT(consts, i + 1 + nchildren, c->astMap.ast[i]);

        // create the bytecode object
        SEXP result = RBytecode(code, consts);
        UNPROTECT(2);
        return result;
    }


    RBytecode(SEXP code, SEXP asts) {
        // note that R_bcEncode is not required as threaded code makes no difference to us
        data_ = cons(code, asts);
        SET_TYPEOF(data_, BCODESXP);
    }



    SEXP code_() const {
        return BCODE_CODE(data_);
    }

    SEXP asts_() const {
        return BCODE_CONSTS(data_);
    }


    SEXP data_;
};


/** Wrapper around CLOSXP.
 */
class RFunction {
public:


    RFunction(SEXP from):
        data_(from) {
        assert(TYPEOF(from) == CLOSXP and "Function can only be created from Closure SEXPs");
    }

    /** Creates the closure from given body, formals and environment.
     */
    static RFunction create(RBytecode body, SEXP formals, SEXP env = nullptr) {
        PROTECT(body);
        PROTECT(formals);
        PROTECT(env);
        SEXP closure = allocSExp(CLOSXP);
        SET_FORMALS(closure, formals);
        SET_BODY(closure, body);
        if (env == nullptr)
            SET_CLOENV(closure, R_GlobalEnv);
        else
            SET_CLOENV(closure, env);
        UNPROTECT(3);
        return RFunction(closure);
    }

    operator SEXP() {
        return data_;
    }

    /** Returns the calling convention of the function.
     */
    CallingConvention callingConvention() const {
        return RBytecode(body_()).header().cc;
    }



private:

    SEXP body_() const {
        return BODY(data_);
    }

    SEXP formals_() const {
        return FORMALS(data_);
    }

    SEXP data_;
};

/** Wrapper around PROMSXP
 */
class RPromise {
public:

    RPromise(SEXP from):
        data_(from) {
        assert(TYPEOF(from) == PROMSXP and "Can only be created from promises");
    }

    /** Creates a promise from given RBytecode and environment.
     */
    static RPromise create(RFunction code, SEXP env) {
        return RPromise(createPromise(code, env));
    }

private:

    SEXP data_;
};



static_assert(sizeof(RBytecode) == sizeof(SEXP), "RBytecode is assumed to be only a wrapper");
static_assert(sizeof(RFunction) == sizeof(SEXP), "RFunction is assumed to be only a wrapper");
static_assert(sizeof(RPromise) == sizeof(SEXP), "RPromise is assumed to be only a wrapper");





}
}

#endif
