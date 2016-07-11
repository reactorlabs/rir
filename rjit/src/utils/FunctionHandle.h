#ifndef RIR_FUNCTION_HANDLE_H
#define RIR_FUNCTION_HANDLE_H

#include "interpreter/interp.h"
#include "ir/BC_inc.h"

#include "CodeHandle.h"

#include <iostream>

namespace rir {

class FunctionHandle {
  public:
    constexpr static unsigned initialSize = 1024;

    SEXP store;
    void* payload;
    unsigned capacity;

    Function* function;

    FunctionHandle() : function(nullptr) {}

    static FunctionHandle create() {
        SEXP store = Rf_allocVector(INTSXP, initialSize);
        R_PreserveObject(store);

        void* payload = INTEGER(store);

        Function* function = new (payload) Function;
        function->magic = FUNCTION_MAGIC;
        function->size = sizeof(Function);
        function->origin = nullptr;
        function->codeLength = 0;
        function->foffset = 0;

        return FunctionHandle(store);
    }

    FunctionHandle(SEXP store)
        : store(store), payload(INTEGER(store)), capacity(Rf_length(store)),
          function((Function*)payload) {
        assert(function->magic == FUNCTION_MAGIC);
        assert(function->size <= (unsigned)Rf_length(store));
    }

    CodeHandle writeCode(SEXP ast, void* bc, unsigned codeSize,
                         std::vector<SEXP>& sources) {
        unsigned totalSize = CodeHandle::totalSize(codeSize, sources.size());

        if (function->size + totalSize > capacity) {
            unsigned newCapacity = capacity * 2;
            SEXP newStore = Rf_allocVector(INTSXP, newCapacity);
            R_PreserveObject(newStore);
            void* newPayload = INTEGER(newStore);

            memcpy(newPayload, payload, capacity);

            R_ReleaseObject(store);
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

        unsigned* srcs = reinterpret_cast<unsigned*>(code.sources());
        for (size_t i = 0, e = sources.size(); i != e; ++i)
            *(srcs++) = sources[i] == nullptr
                            ? 0
                            : src_pool_add(globalContext(), sources[i]);

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
