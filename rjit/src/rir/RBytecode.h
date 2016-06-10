#ifndef RIR_BYTECODE_H_
#define RIR_BYTECODE_H_


#include "RIntlns.h"

#include "Code.h"


namespace rjit {
namespace rir {

/** TODO We basically want the functionality of mkPROMSXP, line 2333 memory.c.

  As a hack I am just using one of the old rjit bytecodes which did exactly the same.
*/
extern "C" SEXP createPromise(SEXP code, SEXP rho);



/** Wrapper around a BCODESXP containing rir bytecode.
 */
class RBytecode {
public:

    /** Function bytecode version. Contains serialized bytecodes for the function and all its promises (recursively), as well as the ast maps.
     */
    static constexpr int MAGIC_VERSION = 0xff;

    RBytecode(SEXP from):
        data_(from) {
        assert(TYPEOF(from) == BCODESXP and "Creating RBytecode from something that is not bytecode");
        assert(version() >= MAGIC_VERSION and "Invalid version - perhaps not rir bytecode");
    }

    RBytecode(RBytecode const &) = default;

    RBytecode & operator = (RBytecode const &) = default;

    /** RBytecode automatically converts to SEXP.
     */
    operator SEXP() {
        return data_;
    }

    Code::CC callingConvention() const {
        return header_().cc;
    }

    /** Returns the version of the bytecode.
     */
    int version() const {
        // not using the header so that we do not assume anything about the underlying sexp
        return INTEGER(code_())[0];
    }

    /** Returns the ast of the code object.
     */
    SEXP ast() const {
        return VECTOR_ELT(consts_(), header_().nchildren);
    }

    /** Returns pointer to the rir bytecode contained in the object.
     */
    BC_t * bytecode() const {
        return reinterpret_cast<BC_t *>(INTEGER(code_()) + sizeof(Header) / sizeof(int));
    }

    size_t bytecodeSize() const {
        return header_().codeSize;
    }

    size_t astMapSize() const {
        return INTEGER(code_())[sizeof(Header) / sizeof(int) + toIntSize(header_().codeSize)];
    }

    unsigned * astMapPos() const {
        return reinterpret_cast<unsigned *>(INTEGER(code_())[sizeof(Header) / sizeof(int) + toIntSize(header_().codeSize) + 1]);
    }

    static RBytecode serialize(Code * from, Code::CC cc) {
        size_t nchildren = from->children.size();
        // bytecode objects for children + ast for code + astMap SEXPs
        size_t constsSize = nchildren + 1 + from->astMap.size;
        // header + code + astMap size + astMap positions
        size_t codeSize = sizeof(Header) / sizeof(int) + toIntSize(from->size) + 1 + from->astMap.size;


        // create the code and consts objects
        SEXP code = allocVector(INTSXP, codeSize);
        PROTECT(code);
        SEXP consts = allocVector(VECSXP, constsSize);
        PROTECT(consts);

        // store the header
        new (INTEGER(code)) Header(cc, nchildren, from->size);
        // store the code
        int idx = sizeof(Header) / sizeof(int);
        memcpy(INTEGER(code) + idx, from->bc, from->size);
        idx += toIntSize(from->size);
        // store the astmap size
        INTEGER(code)[sizeof(Header) / sizeof(int) + toIntSize(from->size)] = from->astMap.size;
        ++idx;
        // store the astmap positions
        memcpy(INTEGER(code) + idx, from->astMap.pos, from->astMap.size * sizeof(int));
        idx += from->astMap.size;

        assert(idx == codeSize and "Some weirdness in serializing code");

        // serialize the promises
        for (idx = 0; idx < nchildren; ++idx)
            SET_VECTOR_ELT(consts, idx, serialize(from->children[idx], Code::CC::envLazy));
        // serialize the code ast
        SET_VECTOR_ELT(consts, idx++, from->ast);
        // serialize the astMap SEXPs
        for (size_t i = 0; i < from->astMap.size; ++i)
            SET_VECTOR_ELT(consts, idx++, from->astMap.ast[i]);

        assert(idx == constsSize and "Some weirdness in serializing consts");

        RBytecode result(code, consts);
        UNPROTECT(2);
        return result;
    }

    Code * deserialize() {
        size_t csize = bytecodeSize();
        size_t asize = astMapSize();
        size_t nchildren = header_().nchildren;

        BC_t * bc = new BC_t[csize];
        unsigned * astPos = new unsigned[asize];
        SEXP * astAst = new SEXP[asize];

        memcpy(bc, bytecode(), csize);
        memcpy(astPos, astMapPos(), asize * sizeof(int));

        for (size_t i = 0; i < asize; ++i)
            astAst[i] = VECTOR_ELT(consts_(), i + nchildren + 1);

        Code * result = new Code(csize, bc, ast(), asize, astPos, astAst);
        result->children.resize(nchildren);
        for (size_t i = 0; i < nchildren; ++i)
            result->addCode(i, RBytecode(VECTOR_ELT(consts_(), i)).deserialize());

        return result;
    }

private:

    static int toIntSize(int offset) {
        static_assert(sizeof(int) == 4, "Integer is four bytes long and R's integers are integers!");
        if (offset % 4 != 0)
            offset += 4 - (offset % 4);
        return offset / 4;
    }

    struct Header {
        /** Bytecode magic version. */
        int version;

        /** Expected calling convention. */
        Code::CC cc;

        /** Number children code objects (first code entries). */
        int nchildren;

        /** Size of the code in bytes. */
        int codeSize;

        Header(Code::CC cc, int nchildren, int codeSize):
            version(MAGIC_VERSION),
            cc(cc),
            nchildren(nchildren),
            codeSize(codeSize) {
        }

    };

    static_assert(sizeof(Header) == 4 * sizeof(int), "Header should not have any padding");
    static_assert(sizeof(Header) % sizeof(BC_t) == 0, "To make sure alignment won't cause us trouble");


    RBytecode(SEXP code, SEXP asts) {
        // note that R_bcEncode is not required as threaded code makes no difference to us
        data_ = cons(code, asts);
        SET_TYPEOF(data_, BCODESXP);
    }


    Header const & header_() const {
        return * reinterpret_cast<Header const *>(INTEGER(code_()));
    }


    SEXP code_() const {
        return BCODE_CODE(data_);
    }

    SEXP consts_() const {
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
