#ifndef RIR_CODE_STREAM_H
#define RIR_CODE_STREAM_H

#include <vector>
#include <cstring>
#include "Pool.h"

namespace rjit {
namespace rir {

class CodeStream {
    std::vector<char>* code;

    unsigned pos = 0;
    unsigned size = 1024;

  public:
    CodeStream() { code = new std::vector<char>(1024); }

    CodeStream& operator<<(BC b) {
        insert(b);
        return *this;
    }

    CodeStream& operator<<(SEXP e) {
        auto idx = Pool::instance().insert(e);
        insert(idx);
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

    unsigned length() { return pos; }

    BC* to_bc() {
        BC* res = (BC*)new char[pos];
        memcpy((void*)res, (void*)&(*code)[0], pos);
        code->clear();
        pos = 0;
        return res;
    }

    Code* to_code() {
        size_t size = pos;
        return new Code(size, to_bc());
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
