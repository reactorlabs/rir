#ifndef RIR_FUNCTION_HANDLE_H
#define RIR_FUNCTION_HANDLE_H

#include "interpreter/interp.h"
#include "ir/BC_inc.h"

#include "CodeHandle.h"
#include "ir/CodeVerifier.h"

#include <iostream>

namespace rir {

class FunctionHandle {
  public:
    constexpr static unsigned initialSize = 2*sizeof(Function);

    SEXP store;
    void* payload;
    unsigned capacity;

    Function* function;

    FunctionHandle() : function(nullptr) {}

    static FunctionHandle create() {
        assert(initialSize > sizeof(Function));
        assert(initialSize % sizeof(int) == 0);
        SEXP store = Rf_allocVector(INTSXP, initialSize / sizeof(int));
        R_PreserveObject(store);

        void* payload = INTEGER(store);

        Function* function = new (payload) Function;
        assert(function == payload);
        function->magic = FUNCTION_MAGIC;
        function->size = sizeof(Function);
        function->origin = nullptr;
        function->codeLength = 0;
        function->foffset = 0;

        return FunctionHandle(store);
    }

    FunctionHandle(SEXP store)
        : store(store), payload(INTEGER(store)),
          capacity(Rf_length(store) * sizeof(int)),
          function((Function*)payload) {
        assert(function->magic == FUNCTION_MAGIC);
        assert(function->size <= capacity);
    }

    ~FunctionHandle() {
        R_ReleaseObject(store);
    }

    CodeHandle writeCode(SEXP ast, void* bc, unsigned codeSize,
                         std::vector<unsigned>& sources) {
        assert(function->size <= capacity);

        unsigned totalSize = CodeHandle::totalSize(codeSize, sources.size());

        if (function->size + totalSize > capacity) {
            unsigned newCapacity = capacity;
            while (function->size + totalSize > newCapacity)
                newCapacity *= 1.5;
            newCapacity = pad4(newCapacity);

            assert(newCapacity % sizeof(int) == 0);
            SEXP newStore = Rf_allocVector(INTSXP, newCapacity / sizeof(int));
            void* newPayload = INTEGER(newStore);

            R_PreserveObject(newStore);
            R_ReleaseObject(store);

            memcpy(newPayload, payload, capacity);
            memset(payload, 0xee, capacity);

            assert(function == payload);
            store = newStore;
            payload = newPayload;
            capacity = newCapacity;
            function = (Function*)payload;
        }

        unsigned offset = function->size;
        void* insert = (void*)((uintptr_t)payload + function->size);
        function->size += totalSize;

        CodeHandle code(ast, codeSize, sources.size(), offset, insert);

        assert(::function(code.code) == function);

        memcpy(code.bc(), bc, codeSize);

        // write the sources
        unsigned skiplistLength = CodeHandle::skiplistLength(sources.size());
        unsigned skiplistEntries = 1 + sources.size() / skiplistLength * 10;

        unsigned* skiplist = reinterpret_cast<unsigned*>(code.sources());
        unsigned* srcs = skiplist + 2 * skiplistLength;

        BC_t* start = code.bc();
        BC_t* pc = start;
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

        return code;
    }

    CodeHandle entryPoint() {
        assert(function->foffset);
        return (Code*)((uintptr_t)function + function->foffset);
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
        return (Code*)((uintptr_t)function + offset);
    }

    SEXP ast() { return entryPoint().ast(); }

    void print() {
        std::cout << "Fun " << this << " --------------\n";
        entryPoint().print();

        for (fun_idx_t i = 0; i < function->codeLength - 1; ++i) {
            std::cout << " P " << i << " ------ \n";
            CodeHandle(codeAtIdx(i)).print();
        }
    }
};
}

#endif
