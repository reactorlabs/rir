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

    /** R bytecode version when the BCODESXP contains a promisse bytecode, i.e. its constant pool links to the function bytecode and its single bytecode after the instruction is the offset to the parent bytecode.
      */
    static constexpr int MAGIC_VERSION_PROMISE = 0xfe;

    /** Function bytecode version. Contains serialized bytecodes for the function and all its promises (recursively), as well as the ast maps.
     */
    static constexpr int MAGIC_VERSION_FUNCTION = 0xff;

    RBytecode(SEXP from):
        data_(from) {
        assert(TYPEOF(from) == BCODESXP and "Creating RBytecode from something that is not bytecode");
        assert(version() >= MAGIC_VERSION_PROMISE and "Invalid version - perhaps not rir bytecode");
    }

    RBytecode(RBytecode const &) = default;

    RBytecode & operator = (RBytecode const &) = default;

    /** RBytecode automatically converts to SEXP.
     */
    operator SEXP() {
        return data_;
    }

    CallingConvention callingConvention() const {
        return header_().cc;
    }

    /** Returns the version of the bytecode.
     */
    int version() const {
        // not using the header so that we do not assume anything about the underlying sexp
        return INTEGER(code_())[0];
    }

    bool isFunctionBytecode() const {
        return version() == MAGIC_VERSION_FUNCTION;
    }

    int size() const {
        return * (reinterpret_cast<int const*>(code()) - 1);
    }

    BC_t const * code() const {
        if (isFunctionBytecode()) {
            // version + header + code size offset
            return reinterpret_cast<BC_t const *>(INTEGER(code_()) + sizeof(Header) / sizeof(int) + 2);
        } else {
            return RBytecode(consts_()).codeByOffset(INTEGER(code_())[1]);
        }
    }

    BC_t const * codeByOffset(int offset) const {
        assert (isFunctionBytecode());
        return reinterpret_cast<BC_t const *>(INTEGER(code_())[offset]);
    }

    /** Serializes a function into RBytecode.

        in consts:
            ast for the code
            promise bytecodes
            promise asts
            astMap for code (asts)
            astMaps for promises (asts)

        in code:
            magic version function
            calling convention
            offset for ast positions
            size of funcion code (in bytes, not rounded to ints)
            function code (align to sizeof int)
            size of promise x
            code for promise x (in bytes, not rounded to ints)
            astMap size for function code
            astMap for function code (positions)
            astMap size for promise x
            astMap for promise x (positions)
     */
    static RBytecode serialize(Function * f, CallingConvention cc) {
        // ast + promise bytecodes & their asts, no promise slot for function
        size_t constsSize = f->code.size() * 2 - 1;
        // version + header
        size_t codeSize = 1 + sizeof(Header) / sizeof(int);
        // size of the debug data
        size_t astSize = 0;
        // calculate code and ast sizes for all code segments
        for (size_t i = 0, e = f->code.size(); i != e; ++i) {
            // astMap
            constsSize += f->code[i]->astMap.size;
            // code size + actual code
            codeSize += 1 + toIntSize(f->code[i]->size);
            // ast size + ast positions
            astSize += 1 + f->code[i]->astMap.size;
        }
        // store the offset to the ast part of the code buffer
        int astOffset = codeSize;
        // total code buffer length is the code buffer + ast buffer
        codeSize += astSize;

        // create the code and consts objects
        SEXP code = allocVector(INTSXP, codeSize);
        PROTECT(code);
        SEXP consts = allocVector(VECSXP, constsSize);
        PROTECT(consts);
        RBytecode result(code, consts);
        UNPROTECT(2);
        PROTECT(result);

        int * rawCode = INTEGER(code);

        // add function's ast to constant pool
        SET_VECTOR_ELT(consts, 0, f->code[0]->ast);

        // serialize the version, header and main function code
        *(rawCode++) = MAGIC_VERSION_FUNCTION;
        new (reinterpret_cast<Header*>(rawCode)) Header(cc, f->code.size(), astOffset);
        rawCode += sizeof(Header) / sizeof(int);
        *(rawCode++) = f->code[0]->size;
        memcpy(rawCode, f->code[0]->bc, f->code[0]->size);
        rawCode += toIntSize(f->code[0]->size);

        // serialize the code, bytecodes and asts of the promises
        for (size_t i = 1, e = f->code.size(); i != e; ++i) {
            Code * c = f->code[i];
            *(rawCode++) = c->size;
            SET_VECTOR_ELT(consts, i, serializePromise(rawCode - INTEGER(code), result));
            SET_VECTOR_ELT(consts, i + f->code.size() - 1, c->ast);
            memcpy(rawCode, c->bc, c->size);
            rawCode += toIntSize(c->size);
        }

        // offset to constants after asts for all codes and bytecodes for promises
        size_t constsOffset = f->code.size() * 2;
        // serialize the astMaps for the function and promises
        for (Code * c : f->code) {
            *(rawCode++) = c->astMap.size;
            memcpy(rawCode, c->astMap.pos, c->astMap.size * sizeof(int));
            rawCode += c->astMap.size;
            for (size_t i = 0, e = c->astMap.size; i != e; ++i)
                SET_VECTOR_ELT(consts, constsOffset++, c->astMap.ast[i]);
        }

        assert(constsOffset == (constsSize + 1) and "Weirness in serialized constants");
        assert(rawCode - INTEGER(code) == static_cast<int>(codeSize) and "Weirdness in serialized code");

        UNPROTECT(1);
        return result;
    }

    /** Deserializes the code object into a Function.

      It is only possible to deserialize the master

     */
    Function * deserialize() {
        assert(isFunctionBytecode() and "Only function bytecodes can be deserialized");
        Function * result = new Function();
        size_t ncode = header_().ncode;
        result->code.resize(ncode);

        // deserialize the function code object and its ast map
        size_t codeOffset = 1 + sizeof(Header) / sizeof(int);
        size_t astPosOffset = header_().astOffset;
        size_t astAstIndex = ncode * 2 - 1; // all promises and asts
        result->code[0] = deserializeCode(codeOffset, astPosOffset, 0, astAstIndex);

        int * code = INTEGER(code_());

        // deserialize the promises
        size_t i = 1;
        while (i < ncode) {
            codeOffset += toIntSize(code[codeOffset]) + 1;
            astAstIndex += code[astPosOffset];
            astPosOffset += code[astPosOffset];
            result->code[i] = deserializeCode(codeOffset, astPosOffset, i, astAstIndex);
            ++i;
        }
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
        /** Expected calling convention. */
        CallingConvention cc;

        /** Number code objects stored in the code buffer. */
        int ncode;

        /** Offset for ast positions (i.e. after all codes were serialized */
        int astOffset;

        Header(CallingConvention cc, int ncode, int astOffset):
            cc(cc),
            ncode(ncode),
            astOffset(astOffset) {
        }

    };

    static_assert(sizeof(Header) == 3 * sizeof(int), "Header should not have any padding");
    static_assert(sizeof(Header) % sizeof(BC_t) == 0, "To make sure alignment won't cause us trouble");


    /** Serializes a code object into RBytecode.

      in consts:
          consts points to parent bytecode

      in code:
          magic version promise
          offset of code object in parent code buffer
     */
    static RBytecode serializePromise(int offset, RBytecode parent) {
        // serialize the bytecode part and the header
        SEXP code = allocVector(INTSXP, 2);
        PROTECT(code);

        INTEGER(code)[0] = MAGIC_VERSION_PROMISE;
        INTEGER(code)[1] = offset;

        // create the bytecode object as the code containing the offset and constant pool is link to the parent bytecode
        SEXP result = RBytecode(code, parent);
        UNPROTECT(1);
        return result;
    }

    Code * deserializeCode(size_t codeOffset, size_t astPosOffset, size_t astIndex, size_t astAstIndex) {
        SEXP consts = consts_();

        int * code = INTEGER(code_()) + codeOffset;
        size_t codeSize = *(code++);

        BC_t * bc = new BC_t[codeSize];
        memcpy(bc, code, codeSize);

        int * codeAst = INTEGER(code_()) + astPosOffset;
        size_t astSize = *(codeAst++);

        unsigned * pos = new unsigned[astSize];
        memcpy(pos, codeAst, astSize * sizeof(unsigned));

        SEXP * ast = new SEXP[astSize];
        for (size_t i = 0; i < astSize; ++i)
            ast[i] = VECTOR_ELT(consts, astAstIndex++);

        return new Code(codeSize, bc, VECTOR_ELT(consts, astIndex), astSize, pos, ast);
    }




    RBytecode(SEXP code, SEXP asts) {
        // note that R_bcEncode is not required as threaded code makes no difference to us
        data_ = cons(code, asts);
        SET_TYPEOF(data_, BCODESXP);
    }


    Header const & header_() const {
        if (isFunctionBytecode())
            return * reinterpret_cast<Header const *>(INTEGER(code_()) + 1);
        else
            return RBytecode(consts_()).header_();
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
