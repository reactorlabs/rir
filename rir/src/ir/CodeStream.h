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
    std::map<unsigned, LabelT> patchpoints;
    std::vector<unsigned> label2pos;

    std::vector<unsigned> sources;

    std::vector<char> callSites_;
    uint32_t nextCallSiteIdx_ = 0;

  public:
    LabelT mkLabel() {
        assert(nextLabel < MAX_JMP);
        label2pos.resize(nextLabel + 1);
        return nextLabel++;
    }

    void setNumLabels(size_t n) {
        label2pos.resize(n);
        nextLabel = n;
    }

    void patchpoint(LabelT l) {
        patchpoints[pos] = l;
        insert((JmpT)0);
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

    CodeStream& insertStackCall(Opcode bc, uint32_t nargs,
                                std::vector<SEXP> names, SEXP call,
                                SEXP targOrSelector = nullptr,
                                FunctionSignature* signature = nullptr) {
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

        unsigned needed = CallSite::size(false, hasNames, false, nargs);
        ensureCallSiteSize(needed);

        CallSite* cs = getNextCallSite(needed);

        cs->nargs = nargs;
        cs->call = Pool::insert(call);
        cs->hasProfile = false;
        cs->hasNames = hasNames;
        cs->hasSelector = (bc == Opcode::dispatch_stack_);
        cs->hasTarget = (bc == Opcode::static_call_stack_ ||
                         bc == Opcode::call_ordered_);
        cs->hasImmediateArgs = false;

        cs->signature = signature;

        if (hasNames) {
            for (unsigned i = 0; i < nargs; ++i) {
                cs->names()[i] = Pool::insert(names[i]);
            }
        }

        if (bc == Opcode::dispatch_stack_) {
            assert(TYPEOF(targOrSelector) == SYMSXP);
            *cs->selector() = Pool::insert(targOrSelector);
        } else if (bc == Opcode::static_call_stack_) {
            assert(TYPEOF(targOrSelector) == CLOSXP ||
                   TYPEOF(targOrSelector) == BUILTINSXP);
            *cs->target() = Pool::insert(targOrSelector);
        } else if (bc == Opcode::call_ordered_) {
            assert(isValidClosureSEXP(targOrSelector));
            *cs->target() = Pool::insert(targOrSelector);
        }

        return *this;
    }

    CodeStream& insertCall(Opcode bc, std::vector<FunIdxT> args,
                           std::vector<SEXP> names, SEXP call,
                           SEXP selector = nullptr,
                           FunctionSignature* signature = nullptr) {
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

        unsigned needed = CallSite::size(true, hasNames, true, nargs);
        ensureCallSiteSize(needed);

        CallSite* cs = getNextCallSite(needed);

        cs->nargs = nargs;
        cs->call = Pool::insert(call);
        cs->hasProfile = true;
        cs->hasNames = hasNames;
        cs->hasSelector = (bc == Opcode::dispatch_);
        cs->hasImmediateArgs = true;

        cs->signature = signature;

        int i = 0;
        for (auto arg : args) {
            cs->args()[i] = arg;
            if (hasNames)
                cs->names()[i] = Pool::insert(names[i]);
            ++i;
        }

        if (bc == Opcode::dispatch_) {
            assert(selector);
            assert(TYPEOF(selector) == SYMSXP);
            *cs->selector() = Pool::insert(selector);
        }

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
        assert(b.bc != Opcode::call_);
        if (b.bc == Opcode::label) {
            return *this << b.immediate.offset;
        }
        b.write(*this);
        // make space in the sources buffer
        sources.push_back(0);
        return *this;
    }

    CodeStream& operator<<(LabelT label) {
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

#define INS(pc_) (*reinterpret_cast<Opcode*>(&(*code)[(pc_)]))

        unsigned size = BC(INS(pc)).size();

        for (unsigned i = 0; i < size; ++i) {
            INS(pc + i) = Opcode::nop_;
            // patchpoints are fixed by just removing the binding to label
            patchpoints.erase(pc + i);
        }

        unsigned tmp = 0, sourceIdx = 0;
        while (tmp != pc) {
            tmp += BC(INS(tmp)).size();
            sourceIdx++;
        }
        // need to insert source slots for the nops occupying the immediate places
        sources.insert(sources.begin() + sourceIdx + 1, size - 1, 0);
    }

    FunIdxT finalize(bool markDefaultArg) {
        Code* res =
            function.writeCode(ast, &(*code)[0], pos, callSites_.data(),
                               callSites_.size(), sources, markDefaultArg);

        for (auto p : patchpoints) {
            unsigned pos = p.first;
            unsigned target = label2pos[p.second];
            JmpT j = target - pos - sizeof(JmpT);
            *(JmpT*)((uintptr_t)res->code() + pos) = j;
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
