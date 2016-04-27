#ifndef RJIT_RIR_BC
#define RJIT_RIR_BC

#include <cstdint>
#include <cstddef>

#include "Pool.h"
#include "RDefs.h"

#include "BC_inc.h"

namespace rjit {
namespace rir {

class CodeStream;

class BC {
  public:
    const BC_t bc;
    BC(BC_t bc) : bc(bc) {}
    const static BC call() { return BC(BC_t::call); }
};

class BC1 {
  public:
    const BC_t bc;
    const immediate_t immediate;
    BC1(BC_t bc, immediate_t immediate) : bc(bc), immediate(immediate) {}

    const static BC1 push(SEXP constant) {
        return BC1(BC_t::push, Pool::instance().insert(constant));
    }
    const static BC1 getfun(SEXP sym) {
        return BC1(BC_t::getfun, Pool::instance().insert(sym));
    }
    const static BC1 getvar(SEXP sym) {
        return BC1(BC_t::getvar, Pool::instance().insert(sym));
    }
    const static BC1 mkprom(size_t prom) { return BC1(BC_t::mkprom, prom); }
};

class Code {
  public:
    size_t size;
    BC_t* bc;

    Code(size_t size, BC_t* bc) : size(size), bc(bc){};
    ~Code() { delete bc; }

    void print();
};

} // rir
} // rjit

#endif
