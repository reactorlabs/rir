#ifndef RIR_CODE_STREAM_H
#define RIR_CODE_STREAM_H

#include <cstring>
#include <map>
#include <vector>

#include "BC.h"
#include "Pool.h"

#include "FunctionHandle.h"
#include "CodeVerifier.h"

namespace rjit {
namespace rir {

class CodeStream {

    friend class Compiler;

    std::vector<char>* code;

    unsigned pos = 0;
    unsigned size = 1024;

    FunctionHandle& function;

    SEXP ast;

    unsigned nextLabel = 0;
    std::map<unsigned, Label> patchpoints;
    std::vector<unsigned> label2pos;

    std::vector<SEXP> sources;

  public:
    Label mkLabel() {
        assert(nextLabel < MAX_JMP);
        label2pos.resize(nextLabel + 1);
        return nextLabel++;
    }

    void setNumLabels(size_t n) {
        label2pos.resize(n);
        nextLabel = n;
    }

    void patchpoint(Label l) {
        patchpoints[pos] = l;
        insert((jmp_t)0);
    }

    CodeStream(FunctionHandle& function, SEXP ast)
        : function(function), ast(ast) {
        code = new std::vector<char>(1024);
    }

    CodeStream& operator<<(const BC& b) {
        if (b.bc == BC_t::label) {
            return *this << b.immediate.offset;
        }
        b.write(*this);
        // make space in the sources buffer
        sources.push_back(nullptr);
        return *this;
    }

    CodeStream& operator<<(Label label) {
        label2pos[label] = pos;
        return *this;
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

    void addAst(SEXP ast) {
        assert(sources.back() == nullptr);
        sources.back() = ast;
    }

    fun_idx_t finalize() {
        CodeHandle res = function.writeCode(ast, &(*code)[0], pos, sources);

        for (auto p : patchpoints) {
            unsigned pos = p.first;
            unsigned target = label2pos[p.second];
            jmp_t j = target - pos - sizeof(jmp_t);
            *(jmp_t*)((uintptr_t)res.bc() + pos) = j;
        }

        label2pos.clear();
        patchpoints.clear();
        nextLabel = 0;

        delete code;
        code = nullptr;
        pos = 0;

        CodeVerifier::calculateAndVerifyStack(res.code);

        return res.code->header;
    }
};
}
}

#endif
