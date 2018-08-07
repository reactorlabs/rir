#ifndef RIR_CODE_STREAM_H
#define RIR_CODE_STREAM_H

#include <cstring>
#include <map>
#include <vector>

#include "runtime/Code.h"
#include "BC.h"

#include "CodeVerifier.h"
#include "utils/FunctionWriter.h"

namespace rir {

class CodeStream {

    friend class Compiler;

    std::vector<char>* code;

    unsigned pos = 0;
    unsigned size = 1024;

    FunctionWriter& function;

    SEXP ast;

    unsigned nextLabel = 0;
    std::map<unsigned, BC::Label> patchpoints;
    std::vector<unsigned> label2pos;

    std::vector<unsigned> sources;

    uint32_t nextCallSiteIdx_ = 0;

  public:
    BC::Label mkLabel() {
        assert(nextLabel < BC::MAX_JMP);
        label2pos.resize(nextLabel + 1);
        return nextLabel++;
    }

    void setNumLabels(size_t n) {
        label2pos.resize(n);
        nextLabel = n;
    }

    void patchpoint(BC::Label l) {
        patchpoints[pos] = l;
        insert((BC::Jmp)0);
    }

    CodeStream(FunctionWriter& function, SEXP ast)
        : function(function), ast(ast) {
        code = new std::vector<char>(1024);
    }

    uint32_t alignedSize(uint32_t needed) {
        static const uint32_t align = 32;
        return (needed % align == 0) ? needed
                                     : needed + align - (needed % align);
    }

    CodeStream& operator<<(const BC& b) {
        b.print();
        if (b.bc == Opcode::label) {
            return *this << b.immediate.offset;
        }
        b.write(*this);
        // make space in the sources buffer
        sources.push_back(0);
        return *this;
    }

    CodeStream& operator<<(BC::Label label) {
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

    void addSrc(SEXP src) {
        assert(sources.back() == 0);
        sources.back() = src_pool_add(globalContext(), src);
    }

    void addSrcIdx(unsigned idx) {
        assert(sources.back() == 0);
        sources.back() = idx;
    }

    unsigned currentPos() const {
        return pos;
    }

    unsigned currentSourcesSize() const {
        return sources.size();
    }

    void remove(unsigned pc) {

#define INS(pc_) (reinterpret_cast<Opcode*>(&(*code)[(pc_)]))

        unsigned size = BC(INS(pc)).size();

        for (unsigned i = 0; i < size; ++i) {
            *INS(pc + i) = Opcode::nop_;
            // patchpoints are fixed by just removing the binding to label
            patchpoints.erase(pc + i);
        }

        unsigned tmp = 0, sourceIdx = 0;
        while (tmp != pc) {
            assert(tmp < pc);
            BC cur(INS(tmp));
            tmp += cur.size();
            sourceIdx++;
        }
        // need to insert source slots for the nops occupying the immediate places
        sources.insert(sources.begin() + sourceIdx + 1, size - 1, 0);
    }

    BC::FunIdx finalize(bool markDefaultArg, size_t localsCnt) {
        Code* res = function.writeCode(ast, &(*code)[0], pos, sources,
                                       markDefaultArg, localsCnt);

        for (auto p : patchpoints) {
            unsigned pos = p.first;
            unsigned target = label2pos[p.second];
            BC::Jmp j = target - pos - sizeof(BC::Jmp);
            *(BC::Jmp*)((uintptr_t)res->code() + pos) = j;
        }

        label2pos.clear();
        patchpoints.clear();
        nextLabel = 0;
        nextCallSiteIdx_ = 0;

        delete code;
        code = nullptr;
        pos = 0;

        CodeVerifier::calculateAndVerifyStack(res);
        return res->header;
    }
};
}

#endif
