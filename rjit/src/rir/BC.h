#ifndef RJIT_RIR_BC
#define RJIT_RIR_BC

#include <cstdint>
#include <cstddef>

namespace rjit {
namespace rir {

enum class BC : uint8_t {
    invalid,
    push,
    getfun,
    getvar,
    call,
    mkprom,
    mkclosure,
};

class Code {
  public:
    size_t size;
    BC* bc;

    Code(size_t size, BC* bc) : size(size), bc(bc){};
    ~Code() { delete bc; }

    void print();
};

} // rir
} // rjit

#endif
