#ifndef RIR_FUNCTION_HANDLE_H
#define RIR_FUNCTION_HANDLE_H

#include "interpreter/interp.h"
#include "interpreter/runtime.h"
#include "ir/BC_inc.h"

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

        Function* function = new (payload) Function;
        assert(function == payload);

        function->info.gc_area_start = sizeof(rir_header);  // just after the header
        function->info.gc_area_length = 3; // signature, origin, next
        function->signature = nullptr;
        function->origin = nullptr;
        function->next = nullptr;
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

    explicit FunctionHandle(Function* function)
        : storeOwner_(false), store(::function2store(function)),
          payload(function), capacity(XLENGTH(store)), function(function) {
        assert(function->magic == FUNCTION_MAGIC);
        assert(function->size <= capacity);
    }

    ~FunctionHandle() {
        if (storeOwner_)
            R_ReleaseObject(store);
    }

    unsigned indexOf(Code* code) {
        unsigned idx = 0;
        for (Code* c : *this) {
            if (c == code)
                return idx;
            ++idx;
        }
        assert(false);
        return 0;
    }

    Code* writeCode(SEXP ast, void* bc, unsigned codeSize, char* callSiteBuffer,
                    unsigned callSiteLength, std::vector<unsigned>& sources,
                    bool markDefaultArg) {
        assert(function->size <= capacity);
        assert(storeOwner_);

        unsigned totalSize =
            Code::size(codeSize, sources.size(), callSiteLength);

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
            EXTERNALSXP_SET_ENTRY(
                newStore, FUNCTION_SIGNATURE_OFFSET,
                EXTERNALSXP_ENTRY(store, FUNCTION_SIGNATURE_OFFSET));
            EXTERNALSXP_SET_ENTRY(
                newStore, FUNCTION_ORIGIN_OFFSET,
                EXTERNALSXP_ENTRY(store, FUNCTION_ORIGIN_OFFSET));
            EXTERNALSXP_SET_ENTRY(
                newStore, FUNCTION_NEXT_OFFSET,
                EXTERNALSXP_ENTRY(store, FUNCTION_NEXT_OFFSET));

            assert(function == payload);

            R_PreserveObject(newStore);
            R_ReleaseObject(store);

            store = newStore;
            payload = newPayload;
            function = (Function*)payload;
            capacity = newCapacity;
        }

        unsigned offset = function->size;
        void* insert = (void*)((uintptr_t)payload + function->size);
        function->size += totalSize;
        assert(function->size <= capacity);

        Code* code = new (insert) Code(ast, codeSize, sources.size(),
                                       callSiteLength, offset, markDefaultArg);

        assert(code->function() == function);

        memcpy(code->code(), bc, codeSize);

        // write the sources
        unsigned skiplistLength = Code::calcSkiplistLength(sources.size());
        unsigned skiplistEntries = 1 + sources.size() / skiplistLength;

        unsigned* skiplist = code->skiplist();
        unsigned* srcs = skiplist + 2 * skiplistLength;

        Opcode* start = code->code();
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

        code->srcLength -= compressed;

        function->size -= compressed * sizeof(unsigned);

        assert(code->srcLength == sources_idx);
        function->codeLength++;

        // set the last code offset
        function->foffset = offset;

        memcpy((void*)code->callSites(), callSiteBuffer, callSiteLength);

        return code;
    }

    Code* entryPoint() {
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

    SEXP ast() { return src_pool_at(globalContext(), entryPoint()->src); }
};
}

#endif
