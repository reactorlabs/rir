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

    std::vector<char> callSites_;
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
    void ensureCallSiteSize(uint32_t needed) {
        needed = alignedSize(needed);
        if (callSites_.size() <= nextCallSiteIdx_ + needed) {
            unsigned newSize = pad4(needed + callSites_.size() * 1.5);
            callSites_.resize(newSize);
        }
    }

    CallSite* getNextCallSite(uint32_t needed) {
        needed = alignedSize(needed);
        CallSite* cs = (CallSite*)&callSites_[nextCallSiteIdx_];
        memset(cs, 0, needed);
        nextCallSiteIdx_ += needed;
        return cs;
    }

    CodeStream& insertCall(uint32_t nargs, SEXP call,
                           FunctionSignature* signature = nullptr) {
        insert(Opcode::call_);
        BC::CallFixedArgs a;
        a.call_id = nextCallSiteIdx_;
        a.nargs = nargs;
        insert(a);
        sources.push_back(0);

        unsigned needed = CallSite::size(false, nargs);
        ensureCallSiteSize(needed);

        CallSite* cs = getNextCallSite(needed);

        cs->nargs = nargs;
        cs->call = Pool::insert(call);
        cs->hasProfile = false;
        cs->signature = signature;

        return *this;
    }

    CodeStream& insertNamedCall(uint32_t nargs, std::vector<SEXP> names,
                                SEXP call,
                                FunctionSignature* signature = nullptr) {
        insert(Opcode::named_call_);
        BC::CallFixedArgs a;
        a.call_id = nextCallSiteIdx_;
        a.nargs = nargs;
        insert(a);

        unsigned needed = CallSite::size(false, nargs);
        ensureCallSiteSize(needed);

        CallSite* cs = getNextCallSite(needed);

        cs->nargs = nargs;
        cs->call = Pool::insert(call);
        cs->hasProfile = false;

        cs->signature = signature;

        for (unsigned i = 0; i < nargs; ++i) {
            insert(Pool::insert(names[i]));
        }

        sources.push_back(0);
        return *this;
    }

    CodeStream& insertStaticCall(uint32_t nargs, SEXP call, SEXP target,
                                 FunctionSignature* signature = nullptr) {
        insert(Opcode::static_call_);
        BC::StaticCallFixedArgs a;
        a.call_id = nextCallSiteIdx_;
        a.nargs = nargs;
        assert(TYPEOF(target) == CLOSXP || TYPEOF(target) == BUILTINSXP);
        a.target = Pool::insert(target);
        insert(a);
        sources.push_back(0);

        unsigned needed = CallSite::size(false, nargs);
        ensureCallSiteSize(needed);

        CallSite* cs = getNextCallSite(needed);

        cs->nargs = nargs;
        cs->call = Pool::insert(call);
        cs->hasProfile = false;

        cs->signature = signature;

        return *this;
    }

    CodeStream& insertNamedCallImplicit(
        std::vector<BC::FunIdx> args, std::vector<SEXP> names, SEXP call,
        SEXP selector = nullptr, FunctionSignature* signature = nullptr) {
        uint32_t nargs = args.size();

        insert(Opcode::named_call_implicit_);
        BC::CallImplicitFixedArgs a;
        a.call_id = nextCallSiteIdx_;
        a.nargs = nargs;
        insert(a);

        unsigned needed = CallSite::size(true, nargs);
        ensureCallSiteSize(needed);

        CallSite* cs = getNextCallSite(needed);

        cs->nargs = nargs;
        cs->call = Pool::insert(call);
        cs->hasProfile = true;

        cs->signature = signature;

        for (BC::FunIdx arg : args)
            insert(arg);
        for (size_t i = 0; i < nargs; ++i)
            insert(Pool::insert(names[i]));

        sources.push_back(0);
        return *this;
    }

    CodeStream& insertCallImplicit(std::vector<BC::FunIdx> args, SEXP call,
                                   SEXP selector = nullptr,
                                   FunctionSignature* signature = nullptr) {
        uint32_t nargs = args.size();

        insert(Opcode::call_implicit_);
        BC::CallImplicitFixedArgs a;
        a.call_id = nextCallSiteIdx_;
        a.nargs = nargs;
        insert(a);

        unsigned needed = CallSite::size(true, nargs);
        ensureCallSiteSize(needed);

        CallSite* cs = getNextCallSite(needed);

        cs->nargs = nargs;
        cs->call = Pool::insert(call);
        cs->hasProfile = true;

        cs->signature = signature;

        for (BC::FunIdx arg : args) {
            insert(arg);
        }

        sources.push_back(0);
        return *this;
    }

    CodeStream& insertWithCallSite(Opcode bc, CallSite* callSite) {
        insert(bc);
        insert(nextCallSiteIdx_);
        insert(callSite->nargs);
        sources.push_back(0);

        unsigned needed = callSite->size();
        ensureCallSiteSize(needed);

        void* cs = &callSites_[nextCallSiteIdx_];
        nextCallSiteIdx_ += needed;
        memcpy(cs, callSite, needed);

        return *this;
    }

    CodeStream& operator<<(const BC& b) {
        assert(b.bc != Opcode::call_implicit_);
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
        Code* res = function.writeCode(ast, &(*code)[0], pos, callSites_.data(),
                                       callSites_.size(), sources,
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
        callSites_.clear();
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
