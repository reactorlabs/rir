#ifndef RJIT_RIR_BC_INC
#define RJIT_RIR_BC_INC

#include <cstdint>
#include <cstddef>

#include "RDefs.h"

namespace rjit {
namespace rir {

// ============================================================
// ==== Bytecode Layout:
//
// [ uint8_t bytecode | optional immediate argument ]
//
enum class BC_t : uint8_t {
    invalid,

    // Push a constant to the stack
    // I: constant (via Pool)
    // S: +1
    push,

    // Function lookup
    // I: symbol (via Pool)
    // S: +1
    getfun,

    // Variable lookup
    // I: symbol (via Pool)
    // S: +1
    getvar,

    // Call function
    // I: N - number of arguments
    // S: -N
    call,

    // Call function with named arguments
    // I: list of names (via Pool)
    // S: - #names
    call_name,

    // Create a promise
    // I: promise index
    // S: +1
    mkprom,

    // Create a closure
    // I: closure index
    // S: +1
    mkclosure,

    ret,
    force,
    pop,
    load_arg,
    get_ast,
    setvar,
    numargi,
    to_bool,
    jmp_true,
    jmp_false,
    jmp,
    lti,
    eqi,
    call_builtin,
    call_special,
    force_all,
    pushi,
    dupi,
    load_argi,
    inci,
    dup,
    add,
    sub,
    lt,

    num_of
};

// ============================================================
// ==== immediate argument types
//
typedef uint32_t pool_idx_t;
typedef uint16_t fun_idx_t;
typedef uint16_t num_args_t;
typedef int16_t jmp_t;
typedef int primitive_t;

union immediate_t {
    pool_idx_t pool;
    fun_idx_t fun;
    num_args_t numArgs;
    jmp_t offset;
    primitive_t prim;
    int i;
};

static constexpr num_args_t VARIADIC_ARGS =
    (1L << (8 * sizeof(num_args_t))) - 1;
static constexpr size_t MAX_NUM_ARGS = VARIADIC_ARGS - 1;
static constexpr size_t MAX_FUN_IDX = 1L << (8 * sizeof(fun_idx_t));
static constexpr size_t MAX_POOL_IDX = 1L << (8 * sizeof(pool_idx_t));
static constexpr size_t MAX_JMP = (1L << ((8 * sizeof(jmp_t)) - 1)) - 1;
static constexpr size_t MIN_JMP = -(1L << ((8 * sizeof(jmp_t)) - 1));

// ============================================================
// ==== Creation and decoding of Bytecodes
//
// * Static factory functions to create an instance of a bytecode
//   which can be pushed onto a CodeStream
// * read and advance to read the next bytecode from an array
//
class CodeStream;
class BC {
    BC(BC_t bc) : bc(bc), immediate({0}) {}
    BC(BC_t bc, immediate_t immediate) : bc(bc), immediate(immediate) {}

  public:
    const BC_t bc;
    const immediate_t immediate;

    inline size_t size() const;
    void write(CodeStream& cs) const;

    void print();

    // Getters for the immediate arguments
    SEXP immediateConst();
    inline fun_idx_t immediateFunIdx() { return immediate.fun; }
    inline num_args_t immediateNumArgs() { return immediate.numArgs; }
    inline jmp_t immediateOffset() { return immediate.offset; }

    // Decode BC from bytecode stream
    inline const static BC read(BC_t* pc);
    inline const static BC advance(BC_t** pc);
    inline static BC_t* rewind(BC_t* pc, BC cur);

    // Create a new BC instance
    inline const static BC call(num_args_t numArgs);
    inline const static BC call_name(SEXP names);
    inline const static BC push(SEXP constant);
    inline const static BC getfun(SEXP sym);
    inline const static BC getvar(SEXP sym);
    inline const static BC mkprom(fun_idx_t prom);
    inline const static BC ret();
    inline const static BC pop();
    inline const static BC force();
    inline const static BC load_arg(num_args_t);
    inline const static BC get_ast();
    inline const static BC setvar();
    inline const static BC numargi();
    inline const static BC to_bool();
    inline const static BC jmp_true(jmp_t);
    inline const static BC jmp_false(jmp_t);
    inline const static BC jmp(jmp_t);
    inline const static BC lti();
    inline const static BC eqi();
    inline const static BC call_special(primitive_t);
    inline const static BC call_builtin(primitive_t);
    inline const static BC force_all();
    inline const static BC pushi(int);
    inline const static BC load_argi();
    inline const static BC dupi();
    inline const static BC dup();
    inline const static BC inci();
    inline const static BC mkclosure();
    inline const static BC add();
    inline const static BC sub();
    inline const static BC lt();
};

} // rir
} // rjit

#endif
