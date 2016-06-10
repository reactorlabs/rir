#ifndef RIR_CODE_STREAM_H
#define RIR_CODE_STREAM_H

#include <cstring>
#include <map>
#include <vector>

#include "BC.h"
#include "Pool.h"

#include "Code.h"

namespace rjit {
namespace rir {

typedef jmp_t Label;

class CodeStream {

    friend class Compiler;

    std::vector<char>* code;

    unsigned pos = 0;
    unsigned size = 1024;

    Code* current;

    Code* parent;
    SEXP ast;
    fun_idx_t insertPoint;

    std::map<unsigned, SEXP> astMap;

    unsigned nextLabel = 0;
    std::map<unsigned, Label> patchpoints;
    std::vector<unsigned> label2pos;

  public:
    Code* getCurrentCode() { return current; }

    Label mkLabel() {
        assert(nextLabel < MAX_JMP);
        label2pos.resize(nextLabel + 1);
        return nextLabel++;
    }

    void patchpoint(Label l) {
        patchpoints[pos] = l;
        insert((jmp_t)0);
    }

    CodeStream(Code* parent, SEXP ast)
        : current(new Code()), parent(parent), ast(ast),
          insertPoint(parent->next()) {
        code = new std::vector<char>(1024);
    }

    CodeStream(SEXP ast)
        : current(new Code()), parent(nullptr), ast(ast), insertPoint(-1) {
        code = new std::vector<char>(1024);
    }

    CodeStream& operator<<(const BC& b) {
        b.write(*this);
        return *this;
    }

    CodeStream& operator<<(Label label) {
        label2pos[label] = pos;
        return *this;
    }

    fun_idx_t finalize() {
        assert(parent);
        parent->addCode(insertPoint, toCode());
        return insertPoint;
    }

    template <typename T>
    void insert(T val) {
        size_t s = sizeof(T);
        if (pos + s >= size) {
            size += 1024;
            code->resize(size);
        }
        *reinterpret_cast<T*>(&(*code)[pos]) = val;
        pos += s;
    }

    void addAst(SEXP ast) { astMap[pos] = ast; }

    Code* toCode() {
        assert(current->size == 0 and current->bc == nullptr);
        size_t size = pos;

        current->size = size;
        current->bc = toBc();
        current->astMap = Code::AstMap(astMap);
        current->ast = ast;
        ast = nullptr;
        astMap.clear();
        return current;
    }

  private:
    BC_t* toBc() {
        BC_t* res = (BC_t*)new char[pos];
        memcpy((void*)res, (void*)&(*code)[0], pos);

        for (auto p : patchpoints) {
            unsigned pos = p.first;
            unsigned target = label2pos[p.second];
            jmp_t j = target - pos - sizeof(jmp_t);
            *(jmp_t*)((uintptr_t)res + pos) = j;
        }

        label2pos.clear();
        patchpoints.clear();
        nextLabel = 0;

        code->clear();
        pos = 0;
        return res;
    }

    CodeStream& operator<<(CodeStream& cs) {
        size += cs.pos;
        code->resize(size);
        memcpy((void*)&((*code)[pos]), (void*)&((*cs.code)[0]), cs.pos);
        pos += cs.pos;
        return *this;
    }
};
}
}

#endif
