#ifndef RIR_CODE_STREAM_H
#define RIR_CODE_STREAM_H

#include <cstring>
#include <map>
#include <vector>

#include "BC.h"

#include "utils/FunctionHandle.h"
#include "CodeVerifier.h"

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

    std::vector<unsigned> sources;

    std::vector<uint32_t> callSites_;
    uint32_t nextCallSiteIdx_ = 0;

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

    CodeStream& insertStackCall(BC_t bc, uint32_t nargs,
                                std::vector<SEXP> names, SEXP call,
                                SEXP selector = nullptr) {
        insert(bc);
        CallArgs a;
        a.call_id = nextCallSiteIdx_;
        a.nargs = nargs;
        insert(a);
        sources.push_back(0);

        bool hasNames = false;
        if (!names.empty())
            for (auto n : names) {
                if (n != R_NilValue) {
                    hasNames = true;
                    break;
                }
            }

        unsigned needed = BC::CallSiteSize(bc, nargs, hasNames);
        if (callSites_.size() <= nextCallSiteIdx_ + needed)
            callSites_.resize(needed + callSites_.size() * 1.5);

        uint32_t* cs = &callSites_[nextCallSiteIdx_];
        nextCallSiteIdx_ += needed;

        *CallSite_call(cs) = Pool::insert(call);
        *CallSite_type(cs) =
            hasNames ? CALL_SITE_STACK_NAMED : CALL_SITE_STACK_UNNAMED;

        if (bc == BC_t::dispatch_stack_) {
            assert(selector);
            assert(TYPEOF(selector) == SYMSXP);
            *CallSite_selector(cs, nargs) = Pool::insert(selector);
        }

        return *this;
    }

    CodeStream& insertCall(BC_t bc, std::vector<fun_idx_t> args,
                           std::vector<SEXP> names, SEXP call,
                           SEXP selector = nullptr) {
        uint32_t nargs = args.size();

        insert(bc);
        CallArgs a;
        a.call_id = nextCallSiteIdx_;
        a.nargs = nargs;
        insert(a);
        sources.push_back(0);

        bool hasNames = false;
        if (!names.empty())
            for (auto n : names) {
                if (n != R_NilValue) {
                    hasNames = true;
                    break;
                }
            }

        unsigned needed = BC::CallSiteSize(bc, nargs, hasNames);
        if (callSites_.size() <= nextCallSiteIdx_ + needed)
            callSites_.resize(needed + callSites_.size() * 1.5);

        uint32_t* cs = &callSites_[nextCallSiteIdx_];
        nextCallSiteIdx_ += needed;

        *CallSite_call(cs) = Pool::insert(call);
        *CallSite_type(cs) = hasNames ? CALL_SITE_NAMED : CALL_SITE_UNNAMED;

        int i = 0;
        for (auto arg : args) {
            CallSite_args(cs)[i] = arg;
            if (hasNames)
                CallSite_names(cs, nargs)[i] = Pool::insert(names[i]);
            ++i;
        }

        if (bc == BC_t::dispatch_) {
            assert(selector);
            assert(TYPEOF(selector) == SYMSXP);
            *CallSite_selector(cs, nargs) = Pool::insert(selector);
        }

        return *this;
    }

    CodeStream& insertWithCallSite(BC_t b, CallSite callSite) {
        insert(b);
        insert(nextCallSiteIdx_);
        insert(callSite.nargs());
        sources.push_back(0);

        auto nargs = callSite.nargs();
        bool hasNames = callSite.hasNames();

        unsigned needed = BC::CallSiteSize(b, nargs, hasNames);
        if (callSites_.size() <= nextCallSiteIdx_ + needed)
            callSites_.resize(needed + callSites_.size() * 1.5);

        uint32_t* cs = &callSites_[nextCallSiteIdx_];
        nextCallSiteIdx_ += needed;
        memcpy(cs, callSite.cs, needed * sizeof(uint32_t));

        return *this;
    }

    CodeStream& operator<<(const BC& b) {
        assert(b.bc != BC_t::call_);
        if (b.bc == BC_t::label) {
            return *this << b.immediate.offset;
        }
        b.write(*this);
        // make space in the sources buffer
        sources.push_back(0);
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

    void addSrc(SEXP src) {
        assert(sources.back() == 0);
        sources.back() = src_pool_add(globalContext(), src);
    }

    void addSrcIdx(unsigned idx) {
        assert(sources.back() == 0);
        sources.back() = idx;
    }

    fun_idx_t finalize() {
        CodeHandle res = function.writeCode(ast, &(*code)[0], pos, sources);

        for (auto p : patchpoints) {
            unsigned pos = p.first;
            unsigned target = label2pos[p.second];
            jmp_t j = target - pos - sizeof(jmp_t);
            *(jmp_t*)((uintptr_t)res.bc() + pos) = j;
        }

        res.code->callSites = new uint32_t[callSites_.size()];
        memcpy(res.code->callSites, callSites_.data(),
               callSites_.size() * sizeof(uint32_t));

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

#endif
