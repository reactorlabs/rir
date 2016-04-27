#ifndef RIR_CODE_STREAM_H
#define RIR_CODE_STREAM_H

#include <vector>
#include <cstring>

#include "Pool.h"
#include "Function.h"
#include "BC.h"

namespace rjit {
namespace rir {

class CodeStream {
    std::vector<char>* code;

    unsigned pos = 0;
    unsigned size = 1024;

    Function& fun;
    size_t insertPoint;

  public:
    CodeStream(Function& fun) : fun(fun), insertPoint(fun.next()) {
        code = new std::vector<char>(1024);
    }

    CodeStream& operator<<(const BC& b) {
        insert(b.bc);
        return *this;
    }

    CodeStream& operator<<(const BC1& b) {
        insert(b.bc);
        insert(b.immediate);
        return *this;
    }

    unsigned length() { return pos; }

    size_t finalize() {
        fun.addCode(insertPoint, toCode());
        return insertPoint;
    }

  private:
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

    BC_t* toBc() {
        BC_t* res = (BC_t*)new char[pos];
        memcpy((void*)res, (void*)&(*code)[0], pos);
        code->clear();
        pos = 0;
        return res;
    }

    Code* toCode() {
        size_t size = pos;
        return new Code(size, toBc());
    }

    CodeStream& operator<<(CodeStream& cs) {
        size += cs.pos;
        code->resize(size);
        memcpy((void*)&((*code)[pos]), (void*)&((*cs.code)[0]), cs.pos);
        pos += cs.pos;
        return *this;
    }
};
}
}

#endif
