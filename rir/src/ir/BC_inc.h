#ifndef RJIT_RIR_BC_INC
#define RJIT_RIR_BC_INC

#include <cassert>
#include <cstddef>
#include <cstdint>
#include <cstring>

#include "R/r.h"

#include <vector>

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
struct CallSite;
struct CallSiteProfile;

enum class Opcode : uint8_t {

#define DEF_INSTR(name, ...) name,
#include "insns.h"

    // A label/jump target (used internally by CodeEditor only!)
    label,

    num_of

};

// ============================================================
// ==== Creation and decoding of Bytecodes
//
// * Static factory functions to create an instance of a bytecode
//   which can be pushed onto a CodeStream
// * read and advance to read the next bytecode from an array

class CodeStream;
class BC {
  public:
    // ============================================================
    // ==== immediate argument types
    //
    // index into the constant pool
    typedef Immediate PoolIdx;
    // index into a functions array of code objects
    typedef Immediate FunIdx;
    typedef Immediate NumArgs;
    // index into arguments
    typedef Immediate ArgIdx;
    // jmp offset
    typedef int32_t Jmp;
    typedef Jmp Label;
    typedef struct {
        Immediate call_id;
        NumArgs nargs;
    } CommonCallArgs;
    typedef struct {
        Immediate name;
        Immediate expected;
        Immediate id;
    } GuardFunArgs;
    typedef Immediate Guard;
    typedef Immediate NumLocals;
    typedef struct {
        Immediate target;
        Immediate source;
    } LocalsCopy;

    static constexpr size_t MAX_NUM_ARGS = 1L << (8 * sizeof(PoolIdx));
    static constexpr size_t MAX_POOL_IDX = 1L << (8 * sizeof(PoolIdx));
    static constexpr size_t MAX_JMP = (1L << ((8 * sizeof(Jmp)) - 1)) - 1;
    static constexpr size_t MIN_JMP = -(1L << ((8 * sizeof(Jmp)) - 1));

    // This is only used internally in the BC handle objects
    // On the bytecode stream each immediate argument uses only the actual
    // space required.
    union ImmediateArguments {
        CommonCallArgs commonCallArgs;
        GuardFunArgs guard_fun_args;
        Guard guard_id;
        PoolIdx pool;
        FunIdx fun;
        ArgIdx arg_idx;
        Jmp offset;
        uint32_t i;
        NumLocals loc;
        LocalsCopy loc_cpy;
    };

    static Immediate readImmediate(Opcode** pc) {
        Immediate i = *(Immediate*)*pc;
        *pc = (Opcode*)((uintptr_t)*pc + sizeof(Immediate));
        return i;
    }

    std::vector<ArgIdx> immediateCallArguments;

    BC() : bc(Opcode::invalid_), immediate({{0}}) {}
    
    BC(Opcode* pc) {
        bc = *pc;
        pc++;
        immediate = decodeImmediateArguments(bc, pc);
        if (bc == Opcode::call_implicit_) {
            pc += sizeof(CommonCallArgs) / sizeof(Opcode);
            for (size_t i = 0; i < immediate.commonCallArgs.nargs; ++i)
                immediateCallArguments.push_back(readImmediate(&pc));
        }
    }

    BC operator=(BC other) {
        bc = other.bc;
        immediate = other.immediate;
        return other;
    }

    bool operator==(const BC& other) const;

    bool is(Opcode aBc) { return bc == aBc; }

    Opcode bc;
    ImmediateArguments immediate;

    inline size_t size() {
        if (bc == Opcode::call_implicit_)
            return immediate.commonCallArgs.nargs * sizeof(FunIdx) +
                   fixedSize(bc);
        return fixedSize(bc);
    }
    inline size_t popCount() {
        // return also is a leave
        assert(bc != Opcode::return_);
        if (bc == Opcode::call_)
            return immediate.commonCallArgs.nargs + 1;
        if (bc == Opcode::static_call_)
            return immediate.commonCallArgs.nargs;
        return popCount(bc);
    }
    inline size_t pushCount() { return pushCount(bc); }

    // Used to serialize bc to CodeStream
    void write(CodeStream& cs) const;

    // Print it to stdout
    void print(CallSite* cs = nullptr);
    void printImmediateArgs();
    void printNames(CallSite* cs);
    void printProfile(CallSite* cs);

    // Accessors to load immediate constant from the pool
    SEXP immediateConst();

    // Return the callsite of this BC, needs the cassSites buffer as input
    CallSite* callSite(Code* code);

    inline static Opcode* jmpTarget(Opcode* pos) {
        BC bc = BC::decode(pos);
        assert(bc.isJmp());
        return (Opcode*)((uintptr_t)pos + bc.size() + bc.immediate.offset);
    }

    bool isCallsite() const {
        return bc == Opcode::call_implicit_ || bc == Opcode::call_ ||
               bc == Opcode::static_call_;
    }

    bool hasPromargs() const {
        return bc == Opcode::call_implicit_ || bc == Opcode::promise_ ||
               bc == Opcode::push_code_;
    }

    bool isCondJmp() const {
        return bc == Opcode::brtrue_ || bc == Opcode::brfalse_ ||
               bc == Opcode::brobj_ || bc == Opcode::beginloop_;
    }

    bool isUncondJmp() const { return bc == Opcode::br_; }

    bool isJmp() const { return isCondJmp() || isUncondJmp(); }

    bool isPure() { return isPure(bc); }

    bool isReturn() const {
        return bc == Opcode::ret_ || bc == Opcode::return_;
    }

    bool isLabel() const { return bc == Opcode::label; }

    bool isGuard() const {
        return bc == Opcode::guard_fun_ || bc == Opcode::guard_env_;
    }

    // ==== BC decoding logic
    inline static BC advance(Opcode** pc) {
        BC cur(*pc);
        *pc = (Opcode*)((uintptr_t)(*pc) + cur.size());
        return cur;
    }

    inline static BC decode(Opcode* pc) {
        BC cur(pc);
        return cur;
    }

    inline static Opcode* next(Opcode* pc) {
        BC cur(pc);
        return (Opcode*)((uintptr_t)pc + cur.size());
    }

    // ==== Factory methods
    // to create new BC objects, which can be streamed to a CodeStream
    inline static BC nop();
    inline static BC makeEnv();
    inline static BC callerEnv();
    inline static BC getEnv();
    inline static BC setEnv();
    inline static BC push(SEXP constant);
    inline static BC push(double constant);
    inline static BC push(int constant);
    inline static BC push_code(FunIdx i);
    inline static BC ldfun(SEXP sym);
    inline static BC ldvar(SEXP sym);
    inline static BC ldvarNoForce(SEXP sym);
    inline static BC ldvarSuper(SEXP sym);
    inline static BC ldvarNoForceSuper(SEXP sym);
    inline static BC ldlval(SEXP sym);
    inline static BC ldddvar(SEXP sym);
    inline static BC ldarg(uint32_t offset);
    inline static BC ldloc(uint32_t offset);
    inline static BC stloc(uint32_t offset);
    inline static BC copyloc(uint32_t target, uint32_t source);
    inline static BC promise(FunIdx prom);
    inline static BC ret();
    inline static BC pop();
    inline static BC force();
    inline static BC asast();
    inline static BC stvar(SEXP sym);
    inline static BC stvarSuper(SEXP sym);
    inline static BC missing(SEXP sym);
    inline static BC checkMissing();
    inline static BC subassign1();
    inline static BC subassign2(SEXP sym);
    inline static BC length();
    inline static BC names();
    inline static BC setNames();
    inline static BC alloc(int type);
    inline static BC asbool();
    inline static BC beginloop(Jmp);
    inline static BC endcontext();
    inline static BC brtrue(Jmp);
    inline static BC brfalse(Jmp);
    inline static BC br(Jmp);
    inline static BC brobj(Jmp);
    inline static BC label(Jmp);
    inline static BC dup();
    inline static BC dup2();
    inline static BC forSeqSize();
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
    inline static BC identical();
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
    inline static BC extract1_1();
    inline static BC extract1_2();
    inline static BC extract2_1();
    inline static BC extract2_2();
    inline static BC swap();
    inline static BC put(uint32_t);
    inline static BC pick(uint32_t);
    inline static BC pull(uint32_t);
    inline static BC is(uint32_t);
    inline static BC isObj();
    inline static BC return_();
    inline static BC int3();

  private:
    explicit BC(Opcode bc) : bc(bc), immediate({{0}}) {}
    BC(Opcode bc, ImmediateArguments immediate)
        : bc(bc), immediate(immediate) {}

    static unsigned fixedSize(Opcode bc) {
        switch (bc) {
#define DEF_INSTR(name, imm, opop, opush, pure)                                \
    case Opcode::name:                                                         \
        return imm * sizeof(Immediate) + 1;
#include "insns.h"
        default:
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

    inline static ImmediateArguments decodeImmediateArguments(Opcode bc,
                                                              Opcode* pc) {
        ImmediateArguments immediate = {{0}};
        switch (bc) {
        case Opcode::push_:
        case Opcode::ldfun_:
        case Opcode::ldvar_:
        case Opcode::ldvar_noforce_:
        case Opcode::ldvar_super_:
        case Opcode::ldvar_noforce_super_:
        case Opcode::ldlval_:
        case Opcode::ldddvar_:
        case Opcode::stvar_:
        case Opcode::stvar_super_:
        case Opcode::missing_:
        case Opcode::subassign2_:
            immediate.pool = *(PoolIdx*)pc;
            break;
        case Opcode::call_implicit_:
        case Opcode::call_:
        case Opcode::static_call_:
            immediate.commonCallArgs = *(CommonCallArgs*)pc;
            break;
        case Opcode::guard_env_:
            immediate.guard_id = *(uint32_t*)pc;
            break;
        case Opcode::guard_fun_:
            immediate.guard_fun_args = *(GuardFunArgs*)pc;
            break;
        case Opcode::promise_:
        case Opcode::push_code_:
            immediate.fun = *(FunIdx*)pc;
            break;
        case Opcode::br_:
        case Opcode::brtrue_:
        case Opcode::brobj_:
        case Opcode::brfalse_:
        case Opcode::label:
        case Opcode::beginloop_:
            immediate.offset = *(Jmp*)pc;
            break;
        case Opcode::pick_:
        case Opcode::pull_:
        case Opcode::is_:
        case Opcode::put_:
        case Opcode::alloc_:
            immediate.i = *(uint32_t*)pc;
            break;
        case Opcode::ldarg_:
            immediate.arg_idx = *(ArgIdx*)pc;
            break;
        case Opcode::ldloc_:
        case Opcode::stloc_:
            immediate.loc = *(NumLocals*)pc;
            break;
        case Opcode::movloc_:
            immediate.loc_cpy = *(LocalsCopy*)pc;
            break;
        case Opcode::nop_:
        case Opcode::make_env_:
        case Opcode::get_env_:
        case Opcode::caller_env_:
        case Opcode::set_env_:
        case Opcode::for_seq_size_:
        case Opcode::extract1_1_:
        case Opcode::extract2_1_:
        case Opcode::extract1_2_:
        case Opcode::extract2_2_:
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
        case Opcode::identical_:
        case Opcode::ne_:
        case Opcode::return_:
        case Opcode::isfun_:
        case Opcode::invisible_:
        case Opcode::visible_:
        case Opcode::endcontext_:
        case Opcode::subassign1_:
        case Opcode::length_:
        case Opcode::names_:
        case Opcode::set_names_:
        case Opcode::isobj_:
        case Opcode::check_missing_:
            break;
        case Opcode::invalid_:
        case Opcode::num_of:
            assert(false);
            break;
        }
        return immediate;
    }
};

} // rir

#endif
