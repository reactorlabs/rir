#ifndef RJIT_RIR_BC_INC
#define RJIT_RIR_BC_INC

#include <cstddef>
#include <cstdint>
#include <cassert>

#include "RDefs.h"

#include <vector>

#include "interp.h"

namespace rjit {
namespace rir {

/* ============================================================================
 * ==== Bytecode
 *
 * This file contains all definitions concerning Bytecodes.
 * Bytecodes are variable length, they all start with a BC_t type tag which is
 * followed by a variable sized immediate argument.
 *
 * The BC class contains constructor methods and all the infrastructure needed
 * to decode/write bytecodes to/from a byte stream.
 *
 * To add a bytecode:
 * 1. add it to BC_t
 * 2. add a constructor method to class BC
 * 3. add an entry to immediate_size in BC.h
 * 4. implement it (ie. resolve all switch statement warnings)
 */

// ============================================================
// ==== BC types
//
enum class BC_t : OpcodeT {

#define DEF_INSTR(name, ...) name,
#include "insns.h"

    // push the number of arguments given to a CC::*Stack function
    // TODO DELETE
    NUMARGI_DEPRECATED,

    // force all promise arguments to this function passed on the stack
    // (currently unused)
    DEPRECATED_FORCE_ALL,

    // A label/jump target (used internally by CodeEditor only!)
    label,

    num_of

};

// TODO the immediate argument types should follow the C immediate types

// ============================================================
// ==== immediate argument types
//
#pragma pack(push)
#pragma pack(0)

// index into the constant pool
typedef uint32_t pool_idx_t;
// index into a functions array of code objects
typedef uint32_t fun_idx_t;
// number of arguments
typedef uint32_t num_args_t;
// jmp offset
typedef int32_t jmp_t;
typedef jmp_t Label;
// immediate arguments to call
typedef struct {
    pool_idx_t args;
    pool_idx_t names;
} call_args_t;

#pragma pack(pop)

static constexpr num_args_t VARIADIC_ARGS =
    (1L << (8 * sizeof(num_args_t))) - 1;
static constexpr size_t MAX_NUM_ARGS = VARIADIC_ARGS - 1;
static constexpr size_t MISSING_ARG_IDX = (1L << (8 * sizeof(fun_idx_t))) - 1;
static constexpr size_t MAX_FUN_IDX = MISSING_ARG_IDX - 1;
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
  public:
    // This is only used internally in the BC handle objects
    // On the bytecode stream each immediate argument uses only the actual
    // space required.
    union immediate_t {
        call_args_t call_args;
        pool_idx_t pool;
        fun_idx_t fun;
        num_args_t numArgs;
        jmp_t offset;
        int i;
    };

    BC() : bc(BC_t::invalid_), immediate({0}) {}
    BC operator=(BC other) {
        bc = other.bc;
        immediate = other.immediate;
        return other;
    }

    BC_t bc;
    immediate_t immediate;


    static unsigned size(BC_t bc) {
        switch (bc) {
#define DEF_INSTR(name, imm, opop, opush, ipop, ipush) case BC_t::name : return imm * sizeof(ArgT) + 1;
#include "insns.h"
        default:
            return 0;
        }
    }

    static unsigned immCount(BC_t bc) {
        switch (bc) {
#define DEF_INSTR(name, imm, opop, opush, ipop, ipush) case BC_t::name : return imm;
#include "insns.h"
        default:
            assert(false);
            return 0;
        }
    }

    static char const * name(BC_t bc) {
        switch (bc) {
#define DEF_INSTR(name, imm, opop, opush, ipop, ipush) case BC_t::name : return #name;
#include "insns.h"
        default:
            return "???";
        }
    }

    static unsigned pushCount(BC_t bc) {
        switch (bc) {
#define DEF_INSTR(name, imm, opop, opush, ipop, ipush) case BC_t::name : return opush;
#include "insns.h"
        default:
            assert(false);
            return 0;
        }
    }

    static unsigned iPushCount(BC_t bc) {
        switch (bc) {
#define DEF_INSTR(name, imm, opop, opush, ipop, ipush) case BC_t::name : return ipush;
#include "insns.h"
        default:
            assert(false);
            return 0;
        }
    }

    static unsigned popCount(BC_t bc) {
        switch (bc) {
#define DEF_INSTR(name, imm, opop, opush, ipop, ipush) case BC_t::name : return opop;
#include "insns.h"
        default:
            assert(false);
            return 0;
        }
    }

    static unsigned iPopCount(BC_t bc) {
        switch (bc) {
#define DEF_INSTR(name, imm, opop, opush, ipop, ipush) case BC_t::name : return ipop;
#include "insns.h"
        default:
            assert(false);
            return 0;
        }
    }

    inline size_t size() const;

    // Used to serialize bc to CodeStream
    void write(CodeStream& cs) const;

    // Print it to stdout
    void print();

    // Accessors to load immediate constant from the pool
    SEXP immediateConst();
    fun_idx_t* immediateCallArgs();
    num_args_t immediateCallNargs();
    SEXP immediateCallNames();

    inline bool isJmp();

    // ==== BC decoding logic
    // There are two interfaces:
    //
    // 1. Slow but convenient:
    //    Decode one BC from stream and advance pc accordingly
    inline static BC advance(BC_t** pc);

    // 2. Fast but dangerous:
    //    Read bytecodes and immediates directly from the stream, without
    //    creating BC object wrapper. Dispatching on BC_t needs handle
    //    different sizes of immediate argument.
    template <typename T>
    inline static T readImmediate(BC_t** pc);
    inline static BC_t readBC(BC_t** pc);

    // ==== Factory methods
    // to create new BC objects, which can be streamed to a CodeStream
    const static BC call(std::vector<fun_idx_t> args, std::vector<SEXP> names);
    inline const static BC push(SEXP constant);
    inline const static BC push(double constant);
    inline const static BC getfun(SEXP sym);
    inline const static BC getvar(SEXP sym);
    inline const static BC mkprom(fun_idx_t prom);
    inline const static BC push_arg(fun_idx_t prom);
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
    inline const static BC label(jmp_t);
    inline const static BC lti();
    inline const static BC eqi();
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
    static inline BC check_function();
    inline const static BC check_primitive(SEXP sym);

  private:
    BC(BC_t bc) : bc(bc), immediate({0}) {}
    BC(BC_t bc, immediate_t immediate) : bc(bc), immediate(immediate) {}
};

} // rir
} // rjit

#endif
