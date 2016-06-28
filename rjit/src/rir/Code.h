#ifndef RIR_CODE
#define RIR_CODE

#include <cassert>
#include <map>

#include "BC_inc.h"

namespace rjit {
namespace rir {

// CodeObject, holds a bytecode array and the associated debug information
// Use CodeStream to build bytecode
//
class Code {
  public:
    /** Calling convention.
     */
    enum class CC : char {
        envLazy,
        stackLazy,
        stackEager,
    };

  private:
    friend class RBytecode;
    friend class CodeStream;


    Code(size_t size, BC_t* bc, SEXP ast, size_t astSize, unsigned* astPos,
         SEXP* astAst)
        : size(size), bc(bc), ast(ast) {}

    size_t linearizedSize() const {
        size_t result = sizeof(::Code); // the header
        result += pad4(size);
        result += sources.size() * sizeof(unsigned); // the source asts array
        for (Code * c : children)
            result += c->linearizedSize();
        return result;
    }

    ::Code * linearizeTo(uint8_t * & stream, ::Function * start, Context * ctx);

  public:
    size_t size;
    BC_t* bc;
    SEXP ast;

    std::vector<SEXP> sources;

    /** Promises used in the code object. */
    std::vector<Code*> children;

    Code() : size(0), bc(nullptr), ast(nullptr) {}

    Code(size_t size, BC_t* bc, SEXP ast, std::map<unsigned, SEXP>& astMap)
        : size(size), bc(bc), ast(ast){};
    ~Code() { delete bc; }

    void print();

    BC_t* end() { return (BC_t*)((uintptr_t)bc + size); }

    SEXP getAst(size_t index) { return sources[index]; }

    SEXP getAst(BC_t * pc) {
        assert(false); // TODO
        return nullptr;
    }

    fun_idx_t next() {
        children.push_back(nullptr);
        assert(children.size() < MAX_FUN_IDX);
        return children.size() - 1;
    }

    void addCode(fun_idx_t pos, Code* c) {
        assert(pos < children.size() and children[pos] == nullptr);
        children[pos] = c;
    }

    Code& operator=(Code&& from) {
        delete bc;
        size = from.size;
        bc = from.bc;
        ast = from.ast;
        sources = std::move(from.sources);
        children = std::move(from.children);
        from.size = 0;
        from.bc = nullptr;
        return *this;
    }


    SEXP linearize(Context * ctx);

    /** Serializes the code object into a Function SEXP.
     */
    SEXP toFunction();

    unsigned calcSize(unsigned size);

    size_t createCode();

    size_t codeSize(size_t count);
};
}
}
#endif
