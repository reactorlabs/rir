#ifndef RIR_CODE_HANDLE_H
#define RIR_CODE_HANDLE_H

#include "interpreter/interp_data.h"
#include "interpreter/interp.h"
#include "interpreter/runtime.h"
#include "ir/BC_inc.h"

#include <iostream>

namespace rir {

class FunctionHandle;

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
        code->skiplistLength = skiplistLength(sourceSize);
    }

    CodeHandle(Code* code) : code(code) { assert(code->magic == CODE_MAGIC); }

    CodeHandle() : code(nullptr) {}

    BC_t* endBc() { return (BC_t*)((uintptr_t)bc() + code->codeSize); }

    BC_t* bc() { return (BC_t*)code->data; }

    unsigned* sources() {
        return (unsigned*)((uintptr_t)(code + 1) + pad4(code->codeSize));
    }

    SEXP source(BC_t* pc) {
        unsigned sidx = getSrcIdxAt(code, (OpcodeT*)pc, true);
        if (!sidx)
            return nullptr;
        return src_pool_at(globalContext(), sidx);
    }

    SEXP ast() { return src_pool_at(globalContext(), code->src); }

    static size_t skiplistLength(unsigned sourcesSize) {
        size_t s = 0.1 * sourcesSize;
        return s ? s : 1;
    }

    static unsigned totalSize(unsigned codeSize, unsigned sourcesSize) {
        return sizeof(Code) + pad4(codeSize) + sourcesSize * sizeof(unsigned) +
               skiplistLength(sourcesSize) * 2 * sizeof(unsigned);
    }

    void* end() {
        return (void*)((uintptr_t)code +
                       totalSize(code->codeSize, code->srcLength));
    }

    FunctionHandle function();

    void print();

    fun_idx_t idx();
};

class CodeHandleIterator : public CodeHandle {
  public:
    CodeHandleIterator(Code* code) { this->code = code; }

    void operator++() { code = ::next(code); }

    bool operator!=(CodeHandleIterator other) { return code != other.code; }

    Code* operator*() { return code; }
};

}

#endif
