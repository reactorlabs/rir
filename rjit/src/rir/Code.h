#ifndef RIR_CODE
#define RIR_CODE

namespace rjit {
namespace rir {

// CodeObject, holds a bytecode array and the associated debug information
// Use CodeStream to build bytecode
//
class Code {
  private:
    class AstMap {
        size_t size;
        unsigned* pos;
        SEXP* ast;

      public:
        AstMap(std::map<unsigned, SEXP>& astMap) {
            size = astMap.size();
            pos = new unsigned[size];
            ast = new SEXP[size];
            unsigned i = 0;
            for (auto e : astMap) {
                pos[i] = e.first;
                ast[i] = e.second;
                i++;
            }
        }

        ~AstMap() {
            delete pos;
            delete ast;
        }

        SEXP at(unsigned p) {
            if (size == 0)
                return nullptr;

            size_t f = 0;

            while (f < size && pos[f] < p)
                f++;

            if (pos[f] != p)
                return nullptr;

            return ast[f];
        }
    };

  public:
    size_t size;
    BC_t* bc;
    SEXP ast;
    AstMap astMap;

    Code(size_t size, BC_t* bc, SEXP ast, std::map<unsigned, SEXP>& astMap)
        : size(size), bc(bc), ast(ast), astMap(astMap){};
    ~Code() { delete bc; }

    void print();

    BC_t* end() { return (BC_t*)((uintptr_t)bc + size); }

    SEXP getAst(BC_t* pc) { return astMap.at((uintptr_t)pc - (uintptr_t)bc); }
};
}
}
#endif
