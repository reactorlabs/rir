#ifndef RIR_FUNCTION_HANDLE_H
#define RIR_FUNCTION_HANDLE_H

#include "interpreter/interp.h"
#include "interpreter/runtime.h"
#include "ir/BC_inc.h"

#include "ir/CodeVerifier.h"
#include "runtime/Function.h"

#include <iostream>

namespace rir {

class FunctionWriter {
  public:
    constexpr static unsigned initialSize = 2 * sizeof(Function);

    Function* function;

    size_t capacity;

    FunctionWriter() = delete;
    // also copy and move ctors = default ?

    static FunctionWriter create() {
        assert(initialSize > sizeof(Function));
        assert(initialSize % sizeof(int) == 0);
        SEXP store = Rf_allocVector(EXTERNALSXP, initialSize);
        void* payload = INTEGER(store);

        Function* function = new (payload) Function;
        FunctionWriter res(function, initialSize);
        return res;
    }

    ~FunctionWriter() { R_ReleaseObject(function->container()); }

    Code* writeCode(SEXP ast, void* bc, unsigned codeSize, char* callSiteBuffer,
                    unsigned callSiteLength, std::vector<unsigned>& sources,
                    bool markDefaultArg) {
        assert(function->size <= capacity);

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

            // it is ok to bypass write barrier here, since newPayload is a new
            // object
            memcpy(newPayload, function, capacity);

            R_PreserveObject(newStore);
            R_ReleaseObject(function->container());

            function = reinterpret_cast<Function*>(newPayload);
            capacity = newCapacity;
        }

        unsigned offset = function->size;
        void* insert = (void*)((uintptr_t)function + function->size);
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

  private:
    explicit FunctionWriter(Function* function, size_t capacity)
        : function(function), capacity(capacity) {
        assert(function->magic == FUNCTION_MAGIC);
        assert(function->size <= capacity);
    }
};
}

#endif
