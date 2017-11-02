#ifndef RJIT_RIR_BC_INC
#define RJIT_RIR_BC_INC

#include <cstddef>
#include <cstdint>
#include <cassert>
#include <cstring>

#include "R/r.h"

#include <vector>

typedef uint32_t ArgT;

// type  for constant & ast pool indices
typedef uint32_t Immediate;

// type  signed immediate values (unboxed ints)
typedef uint32_t SignedImmediate;

// type of relative jump offset (all jumps are relative)
typedef int32_t JumpOffset;

typedef unsigned FunctionIndex;
typedef unsigned ArgumentsCount;

namespace rir {

/* ============================================================================
 * ==== Bytecode
 *
 * This file contains all definitions concerning Bytecodes.
 * Bytecodes are variable length, they all start with a Opcode type tag which is
 * followed by a variable sized immediate argument.
 *
 * The BC class contains constructor methods and all the infrastructure needed
 * to decode/write bytecodes to/from a byte stream.
 *
 * To add a bytecode:
 * 1. add it to Opcode
 * 2. add a constructor method to class BC
 * 3. add an entry to immediate_size in BC.h
 * 4. implement it (ie. resolve all switch statement warnings)
 */

// ============================================================
// ==== BC types
//

struct Code;
struct CallSiteStruct;
struct CallSiteProfile;

enum class Opcode : uint8_t {

#define DEF_INSTR(name, ...) name,
#include "insns.h"

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
typedef uint32_t PoolIdxT;
// index into a functions array of code objects
typedef uint32_t FunIdxT;
// number of arguments
typedef uint32_t NumArgsT;
// jmp offset
typedef int32_t JmpT;
typedef JmpT LabelT;
typedef struct {
    uint32_t call_id;
    uint32_t nargs;
} CallArgs;
typedef struct {
    uint32_t name;
    uint32_t expected;
    uint32_t id;
} GuardFunArgs;
typedef uint32_t GuardT;
#pragma pack(pop)

static constexpr size_t MAX_NUM_ARGS = 1L << (8 * sizeof(PoolIdxT));
static constexpr size_t MAX_POOL_IDX = 1L << (8 * sizeof(PoolIdxT));
static constexpr size_t MAX_JMP = (1L << ((8 * sizeof(JmpT)) - 1)) - 1;
static constexpr size_t MIN_JMP = -(1L << ((8 * sizeof(JmpT)) - 1));

// ============================================================
// ==== Creation and decoding of Bytecodes
//
// * Static factory functions to create an instance of a bytecode
//   which can be pushed onto a CodeStream
// * read and advance to read the next bytecode from an array

class CodeStream;
class BC {
  public:
    // This is only used internally in the BC handle objects
    // On the bytecode stream each immediate argument uses only the actual
    // space required.
    union ImmediateT {
        CallArgs call_args;
        GuardFunArgs guard_fun_args;
        GuardT guard_id;
        PoolIdxT pool;
        FunIdxT fun;
        NumArgsT arg_idx;
        JmpT offset;
        uint32_t i;
    };

    BC() : bc(Opcode::invalid_), immediate({{0}}) {}
    BC operator=(BC other) {
        bc = other.bc;
        immediate = other.immediate;
        return other;
    }

    bool operator==(const BC& other) const;

    bool is(Opcode aBc) { return bc == aBc; }

    Opcode bc;
    ImmediateT immediate;

    inline size_t size() { return size(bc); }
    inline size_t popCount() {
        // return also is a leave
        assert(bc != Opcode::return_);
        if (bc == Opcode::call_stack_)
            return immediate.call_args.nargs + 1;
        if (bc == Opcode::static_call_stack_ || bc == Opcode::dispatch_stack_)
            return immediate.call_args.nargs;
        return popCount(bc);
    }
    inline size_t pushCount() { return pushCount(bc); }

    // Used to serialize bc to CodeStream
    void write(CodeStream& cs) const;

    // Print it to stdout
    void print(CallSiteStruct* cs = nullptr);
    void printArgs(CallSiteStruct* cs);
    void printNames(CallSiteStruct* cs);
    void printProfile(CallSiteStruct* cs);

    // Accessors to load immediate constant from the pool
    SEXP immediateConst();

    // Return the callsite of this BC, needs the cassSites buffer as input
    CallSiteStruct* callSite(Code* code);

    inline static Opcode* jmpTarget(Opcode* pos) {
        BC bc = BC::decode(pos);
        assert(bc.isJmp());
        return (Opcode*)((uintptr_t)pos + bc.size() + bc.immediate.offset);
    }

    bool isCallsite() const {
        return bc == Opcode::call_ || bc == Opcode::dispatch_ ||
               bc == Opcode::call_stack_ || bc == Opcode::dispatch_stack_ ||
               bc == Opcode::static_call_stack_;
    }

    bool hasPromargs() const {
        return bc == Opcode::call_ || bc == Opcode::dispatch_ ||
               bc == Opcode::promise_ || bc == Opcode::push_code_;
    }

    bool isCondJmp() const {
        return bc == Opcode::brtrue_ || bc == Opcode::brfalse_ ||
               bc == Opcode::brobj_ || bc == Opcode::beginloop_;
    }

    bool isUncondJmp() const {
        return bc == Opcode::br_;
    }

    bool isJmp() const {
        return isCondJmp() || isUncondJmp();
    }

    bool isPure() { return isPure(bc); }

    bool isReturn() const { return bc == Opcode::ret_ || bc == Opcode::return_; }

    bool isLabel() const { return bc == Opcode::label; }

    bool isGuard() const {
        return bc == Opcode::guard_fun_ || bc == Opcode::guard_env_;
    }

    // ==== BC decoding logic
    inline static BC advance(Opcode** pc) {
        Opcode bc = **pc;
        BC cur(bc, decodeImmediate(bc, (*pc) + 1));
        *pc = (Opcode*)((uintptr_t)(*pc) + cur.size());
        return cur;
    }

    inline static BC decode(Opcode* pc) {
        Opcode bc = *pc;
        BC cur(bc, decodeImmediate(bc, pc + 1));
        return cur;
    }

    // ==== Factory methods
    // to create new BC objects, which can be streamed to a CodeStream
    inline static BC nop();
    inline static BC push(SEXP constant);
    inline static BC push(double constant);
    inline static BC push(int constant);
    inline static BC push_code(FunIdxT i);
    inline static BC ldfun(SEXP sym);
    inline static BC ldvar(SEXP sym);
    inline static BC ldvar2(SEXP sym);
    inline static BC ldlval(SEXP sym);
    inline static BC ldarg(SEXP sym);
    inline static BC ldddvar(SEXP sym);
    inline static BC promise(FunIdxT prom);
    inline static BC ret();
    inline static BC pop();
    inline static BC force();
    inline static BC asast();
    inline static BC stvar(SEXP sym);
    inline static BC stvar2(SEXP sym);
    inline static BC missing(SEXP sym);
    inline static BC subassign();
    inline static BC subassign2(SEXP sym);
    inline static BC length();
    inline static BC names();
    inline static BC setNames();
    inline static BC alloc(int type);
    inline static BC asbool();
    inline static BC beginloop(JmpT);
    inline static BC endcontext();
    inline static BC brtrue(JmpT);
    inline static BC brfalse(JmpT);
    inline static BC br(JmpT);
    inline static BC brobj(JmpT);
    inline static BC label(JmpT);
    inline static BC dup();
    inline static BC dup2();
    inline static BC testBounds();
    inline static BC inc();
    inline static BC close();
    inline static BC add();
    inline static BC mul();
    inline static BC div();
    inline static BC pow();
    inline static BC idiv();
    inline static BC mod();
    inline static BC sub();
    inline static BC uplus();
    inline static BC uminus();
    inline static BC Not();
    inline static BC lt();
    inline static BC gt();
    inline static BC le();
    inline static BC ge();
    inline static BC eq();
    inline static BC ne();
    inline static BC seq();
    inline static BC colon();
    inline static BC makeUnique();
    inline static BC setShared();
    inline static BC asLogical();
    inline static BC lglOr();
    inline static BC lglAnd();
    inline static BC guardName(SEXP, SEXP);
    inline static BC guardNamePrimitive(SEXP);
    inline static BC guardEnv(uint32_t id);
    inline static BC isfun();
    inline static BC invisible();
    inline static BC visible();
    inline static BC extract1();
    inline static BC subset1();
    inline static BC extract2();
    inline static BC subset2();
    inline static BC swap();
    inline static BC put(uint32_t);
    inline static BC pick(uint32_t);
    inline static BC pull(uint32_t);
    inline static BC is(uint32_t);
    inline static BC return_();
    inline static BC int3();

  private:
    explicit BC(Opcode bc) : bc(bc), immediate({{0}}) {}
    BC(Opcode bc, ImmediateT immediate) : bc(bc), immediate(immediate) {}

    static unsigned size(Opcode bc) {
        switch (bc) {
#define DEF_INSTR(name, imm, opop, opush, pure)                                \
    case Opcode::name:                                                         \
        return imm * sizeof(ArgT) + 1;
#include "insns.h"
        default:
            return 0;
        }
    }

    static unsigned immCount(Opcode bc) {
        switch (bc) {
#define DEF_INSTR(name, imm, opop, opush, pure)                                \
    case Opcode::name:                                                         \
        return imm;
#include "insns.h"
        default:
            assert(false);
            return 0;
        }
    }

    static char const* name(Opcode bc) {
        switch (bc) {
#define DEF_INSTR(name, imm, opop, opush, pure)                                \
    case Opcode::name:                                                         \
        return #name;
#include "insns.h"
        default:
            return "???";
        }
    }

    static unsigned pushCount(Opcode bc) {
        switch (bc) {
#define DEF_INSTR(name, imm, opop, opush, pure)                                \
    case Opcode::name:                                                         \
        return opush;
#include "insns.h"
        default:
            assert(false);
            return 0;
        }
    }

    static unsigned popCount(Opcode bc) {
        switch (bc) {
#define DEF_INSTR(name, imm, opop, opush, pure)                                \
    case Opcode::name:                                                         \
        assert(opop != -1);                                                    \
        return opop;
#include "insns.h"
        default:
            assert(false);
            return 0;
        }
    }

    static unsigned isPure(Opcode bc) {
        switch (bc) {
#define DEF_INSTR(name, imm, opop, opush, pure)                                \
    case Opcode::name:                                                         \
        return pure;
#include "insns.h"
        case Opcode::label:
            return false;
        default:
            assert(false);
            return 0;
        }
    }

    inline static ImmediateT decodeImmediate(Opcode bc, Opcode* pc) {
        ImmediateT immediate = {{0}};
        switch (bc) {
        case Opcode::push_:
        case Opcode::ldfun_:
        case Opcode::ldarg_:
        case Opcode::ldvar_:
        case Opcode::ldvar2_:
        case Opcode::ldlval_:
        case Opcode::ldddvar_:
        case Opcode::stvar_:
        case Opcode::stvar2_:
        case Opcode::missing_:
        case Opcode::subassign2_:
            immediate.pool = *(PoolIdxT*)pc;
            break;
        case Opcode::dispatch_stack_:
        case Opcode::call_:
        case Opcode::dispatch_:
        case Opcode::call_stack_:
        case Opcode::static_call_stack_:
            immediate.call_args = *(CallArgs*)pc;
            break;
        case Opcode::guard_env_:
            immediate.guard_id = *(uint32_t*)pc;
            break;
        case Opcode::guard_fun_:
            immediate.guard_fun_args = *(GuardFunArgs*)pc;
            break;
        case Opcode::promise_:
        case Opcode::push_code_:
            immediate.fun = *(FunIdxT*)pc;
            break;
        case Opcode::br_:
        case Opcode::brtrue_:
        case Opcode::brobj_:
        case Opcode::brfalse_:
        case Opcode::label:
        case Opcode::beginloop_:
            immediate.offset = *(JmpT*)pc;
            break;
        case Opcode::pick_:
        case Opcode::pull_:
        case Opcode::is_:
        case Opcode::put_:
        case Opcode::alloc_:
            immediate.i = *(uint32_t*)pc;
            break;
        case Opcode::nop_:
        case Opcode::test_bounds_:
        case Opcode::extract1_:
        case Opcode::subset1_:
        case Opcode::extract2_:
        case Opcode::subset2_:
        case Opcode::close_:
        case Opcode::ret_:
        case Opcode::pop_:
        case Opcode::force_:
        case Opcode::asast_:
        case Opcode::asbool_:
        case Opcode::dup_:
        case Opcode::dup2_:
        case Opcode::swap_:
        case Opcode::int3_:
        case Opcode::make_unique_:
        case Opcode::set_shared_:
        case Opcode::aslogical_:
        case Opcode::lgl_and_:
        case Opcode::lgl_or_:
        case Opcode::inc_:
        case Opcode::add_:
        case Opcode::mul_:
        case Opcode::div_:
        case Opcode::idiv_:
        case Opcode::mod_:
        case Opcode::pow_:
        case Opcode::seq_:
        case Opcode::colon_:
        case Opcode::sub_:
        case Opcode::uplus_:
        case Opcode::uminus_:
        case Opcode::not_:
        case Opcode::lt_:
        case Opcode::gt_:
        case Opcode::le_:
        case Opcode::ge_:
        case Opcode::eq_:
        case Opcode::ne_:
        case Opcode::return_:
        case Opcode::isfun_:
        case Opcode::invisible_:
        case Opcode::visible_:
        case Opcode::endcontext_:
        case Opcode::subassign_:
        case Opcode::length_:
        case Opcode::names_:
        case Opcode::set_names_:
            break;
        case Opcode::invalid_:
        case Opcode::num_of:
            assert(false);
            break;
        }
        return immediate;
    }

    friend class CodeEditor;
    friend class CodeStream;
};

} // rir

#endif
