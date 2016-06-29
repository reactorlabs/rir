#ifndef RIR_FUNCTION_HANDLE_H
#define RIR_FUNCTION_HANDLE_H

#include "interp.h"

#include <iostream>

namespace rjit {
namespace rir {

class CodeHandle {
  public:
    Code* code;

    CodeHandle(SEXP ast, unsigned codeSize, unsigned sourceSize,
               unsigned offset, void* insert) {
        code = new (insert) Code;

        code->magic = CODE_MAGIC;
        code->header = offset;
        code->src = src_pool_add(globalContext(), ast);
        code->codeSize = codeSize;
        code->srcLength = sourceSize;
    }

    CodeHandle(Code* code) : code(code) {
        // TODO see line 50
        //assert(atEnd() || code->magic == CODE_MAGIC);
    }

    void* endData() { return (void*)((uintptr_t)data() + code->codeSize); }

    void* data() { return code->data; }

    void* sources() {
        return (void*)((uintptr_t)(code + 1) + pad4(code->codeSize));
    }

    static unsigned totalSize(unsigned codeSize, unsigned sourcesSize) {
        return sizeof(Code) + pad4(codeSize) + sourcesSize * sizeof(unsigned);
    }

    void* end() {
        return (void*)((uintptr_t)code +
                       totalSize(code->codeSize, code->srcLength));
    }

    void print();

    // TODO This won't work because CodeHandle only knows Code and calculates the function from its offset, the atEnd code is end iterator sentinel, and as such is not valid, i.e. cannot get its function and segfaults. I am not entirely sure how you wanted the API to look so I can't fix it
    bool atEnd() {
        Function * f = function(code);
        uintptr_t functionEnd = (uintptr_t)f + f->size;
        return functionEnd == (uintptr_t)code;
    }

    CodeHandle next() { return CodeHandle(::next(code)); }
};

class FunctionHandle {
  public:
    constexpr static unsigned initialSize = 1024;

    SEXP store;
    void* payload;
    unsigned capacity = initialSize;

    Function* function;

    FunctionHandle() {
        store = Rf_allocVector(INTSXP, capacity);
        payload = INTEGER(store);

        function = new (payload) Function;
        function->magic = FUNCTION_MAGIC;
        function->size = sizeof(Function);
        function->origin = nullptr;
        function->codeLength = 0;
    }

    FunctionHandle(SEXP store)
        : store(store), payload(INTEGER(store)), capacity(Rf_length(store)),
          function((Function*)payload) {
        assert(function->magic == FUNCTION_MAGIC);
        assert(function->size <= (unsigned)Rf_length(store));
    }

    FunctionHandle(const FunctionHandle&) = delete;

    fun_idx_t nextIdx() { return function->codeLength++; }

    CodeHandle writeCode(fun_idx_t idx, SEXP ast, void* bc, unsigned codeSize,
                         std::vector<SEXP>& sources) {
        unsigned totalSize = CodeHandle::totalSize(codeSize, sources.size());

        if (function->size + totalSize > capacity) {
            unsigned newCapacity = capacity * 2;
            SEXP newStore = Rf_allocVector(INTSXP, newCapacity);
            void* newPayload = INTEGER(newStore);

            memcpy(newPayload, payload, capacity);

            store = newStore;
            payload = newPayload;
            capacity = newCapacity;
        }

        unsigned offset = function->size;
        void* insert = (void*)((uintptr_t)payload + function->size);
        function->size += totalSize;

        CodeHandle code(ast, codeSize, sources.size(), offset, insert);

        assert(::function(code.code) == function);

        memcpy(code.data(), bc, codeSize);

        unsigned* srcs = reinterpret_cast<unsigned*>(code.sources());
        for (size_t i = 0, e = sources.size(); i != e; ++i)
            *(srcs++) = sources[i] == nullptr
                            ? 0
                            : src_pool_add(globalContext(), sources[i]);

        return code;
    }

    CodeHandle begin() { return CodeHandle(::begin(function)); }
};
}
}

#endif
