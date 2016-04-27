#ifndef RJIT_RIR_BC_INC
#define RJIT_RIR_BC_INC

#include <cstdint>
#include <cstddef>

namespace rjit {
namespace rir {

typedef uint32_t immediate_t;

enum class BC_t : uint8_t {
    invalid,
    push,
    getfun,
    getvar,
    call,
    mkprom,
    mkclosure,
};

} // rir
} // rjit

#endif
