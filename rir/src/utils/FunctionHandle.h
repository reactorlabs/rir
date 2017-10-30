#ifndef RIR_FUNCTION_HANDLE_H
#define RIR_FUNCTION_HANDLE_H

#include "interpreter/interp.h"
#include "ir/BC_inc.h"

#include "CodeHandle.h"
#include "ir/CodeVerifier.h"

#include <iostream>

namespace rir {

class FunctionHandle {
  private:
    bool storeOwner_ = true;
  public:
    constexpr static unsigned initialSize = 2*sizeof(Function);

    SEXP store;
    void* payload;

    size_t capacity;
    Function* function;

    FunctionHandle() = delete;
    // also copy and move ctors = default ?

    static FunctionHandle create() {
        assert(initialSize > sizeof(Function));
        assert(initialSize % sizeof(int) == 0);
        SEXP store = Rf_allocVector(EXTERNALSXP, initialSize);
        void* payload = INTEGER(store);

        Function* function = (Function*)payload;
        function->info.gc_area_start = sizeof(rir_header);  // just after the header
        function->info.gc_area_length = 4;  // origin, next, closure, signature
        function->origin = nullptr;
        function->next = nullptr;
        function->closure = nullptr;
        function->signature = nullptr;
        function->magic = FUNCTION_MAGIC;
        function->envLeaked = false;
        function->envChanged = false;
        function->deopt = false;
        function->size = sizeof(Function);
        function->codeLength = 0;
        function->foffset = 0;
        function->invocationCount = 0;
        function->markOpt = false;

        R_PreserveObject(store);

        FunctionHandle res(store);
        res.storeOwner_ = true;

        return res;
    }

    explicit FunctionHandle(SEXP store)
        : storeOwner_(false), store(store), payload(INTEGER(store)),
          capacity(XLENGTH(store)), function((Function*)payload) {
        assert(function->magic == FUNCTION_MAGIC);
        assert(function->size <= capacity);
    }

    ~FunctionHandle() {
        if (storeOwner_)
            R_ReleaseObject(store);
    }

    CodeHandle writeCode(SEXP ast, void* bc, unsigned codeSize,
                         char* callSiteBuffer, unsigned callSiteLength,
                         std::vector<unsigned>& sources, bool markDefaultArg) {
        assert(function->size <= capacity);

        unsigned totalSize =
            CodeHandle::totalSize(codeSize, sources.size(), callSiteLength);

        if (function->size + totalSize > capacity) {
            unsigned newCapacity = capacity;
            while (function->size + totalSize > newCapacity)
                newCapacity *= 1.5;
            newCapacity = pad4(newCapacity);

            assert(newCapacity % sizeof(int) == 0);
            assert(function->size + totalSize <= newCapacity);

            SEXP newStore = Rf_allocVector(EXTERNALSXP, newCapacity);
            void* newPayload = INTEGER(newStore);

            memcpy(newPayload, payload, capacity);

            assert(function == payload);

            // clear the fields that GC traces and release the old store
            function->origin = nullptr;
            function->next = nullptr;
            function->closure = nullptr;
            function->signature = nullptr;
            R_ReleaseObject(store);

            R_PreserveObject(newStore);

            store = newStore;
            payload = newPayload;
            function = (Function*)payload;
            capacity = newCapacity;
        }

        unsigned offset = function->size;
        void* insert = (void*)((uintptr_t)payload + function->size);
        function->size += totalSize;
        assert(function->size <= capacity);

        CodeHandle code(ast, codeSize, sources.size(), callSiteLength, offset,
                        insert, markDefaultArg);

        assert(::code2function(code.code) == function);

        memcpy(code.bc(), bc, codeSize);

        // write the sources
        unsigned skiplistLength = CodeHandle::skiplistLength(sources.size());
        unsigned skiplistEntries = 1 + sources.size() / skiplistLength;

        unsigned* skiplist = reinterpret_cast<unsigned*>(code.sources());
        unsigned* srcs = skiplist + 2 * skiplistLength;

        Opcode* start = code.bc();
        Opcode* pc = start;
        unsigned instruction_number = 0;
        unsigned sources_idx = 0;
        unsigned compressed = 0;

        while (pc < start + codeSize) {
            if (instruction_number % skiplistEntries == 0) {
                // no need to write empty source before next skiplist target
                while (pc < start + codeSize &&
                       sources[instruction_number] == 0) {
                    BC::advance(&pc);
                    ++instruction_number;
                    ++compressed;
                }

                if (pc == start + codeSize)
                    break;

                skiplistLength--;
                *(skiplist++) = pc - start;
                *(skiplist++) = sources_idx;
            }

            *(srcs++) = sources[instruction_number];

            instruction_number++;
            sources_idx++;
            if (pc < start + codeSize)
                BC::advance(&pc);
        }
        while (skiplistLength) {
            *(skiplist++) = pc - start;
            *(skiplist++) = -1;
            skiplistLength--;
        }

        assert(instruction_number == sources.size());
        assert(skiplistLength == 0);
        assert(compressed + sources_idx == instruction_number);

        code.code->srcLength -= compressed;

        function->size -= compressed * sizeof(unsigned);

        assert(code.code->srcLength == sources_idx);
        function->codeLength++;

        // set the last code offset
        function->foffset = offset;

        memcpy(callSites(code.code), callSiteBuffer, callSiteLength);

        return code;
    }

    CodeHandle entryPoint() {
        assert(function->foffset);
        return bodyCode(function);
    }

    CodeHandleIterator begin() { return CodeHandleIterator(::begin(function)); }
    CodeHandleIterator end() { return CodeHandleIterator(::end(function)); }

    inline Code* codeAtIdx(unsigned idx) {
        for (auto c : *this) {
            if (idx-- == 0)
                return c;
        }
        assert(false);
        return nullptr;
    }

    inline Code* codeAtOffset(unsigned offset) {
        return codeAt(function, offset);
    }

    SEXP ast() { return entryPoint().ast(); }

};
}

#endif
