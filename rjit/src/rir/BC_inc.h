#ifndef RJIT_RIR_BC_INC
#define RJIT_RIR_BC_INC

#include <cstdint>
#include <cstddef>

#include "RDefs.h"

namespace rjit {
namespace rir {

enum class BC_t : uint8_t {
    invalid,
    push,
    getfun,
    getvar,
    call,
    call_name,
    mkprom,
    mkclosure,
    num_of
};

typedef uint32_t pool_idx_t;
typedef uint16_t fun_idx_t;
typedef uint16_t num_args_t;

union immediate_t {
    pool_idx_t pool;
    fun_idx_t fun;
    num_args_t numArgs;
};

static constexpr size_t MAX_NUM_ARGS = 1L << (8 * sizeof(num_args_t));
static constexpr size_t MAX_FUN_IDX = 1L << (8 * sizeof(fun_idx_t));
static constexpr size_t MAX_POOL_IDX = 1L << (8 * sizeof(pool_idx_t));

class CodeStream;
class BC {
    BC(BC_t bc) : bc(bc), immediate({0}) {}
    BC(BC_t bc, immediate_t immediate) : bc(bc), immediate(immediate) {}

  public:
    const BC_t bc;
    const immediate_t immediate;

    inline size_t size() const;

    void write(CodeStream& cs) const;

    SEXP immediateConst();
    fun_idx_t immediateFunIdx();

    inline const static BC read(BC_t* pc);
    inline const static BC advance(BC_t** pc);

    inline const static BC call(num_args_t numArgs);
    inline const static BC push(SEXP constant);
    inline const static BC getfun(SEXP sym);
    inline const static BC getvar(SEXP sym);
    inline const static BC mkprom(fun_idx_t prom);
};

} // rir
} // rjit

#endif
